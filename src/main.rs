use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt::Display,
    process::Command,
    str::FromStr,
};

use tracing::{debug, error, info, span, warn, Level};

use std::io::Write;

use cargo_toml::{Dependency, Inheritable, Manifest};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Bump {
    PATCH,
    MINOR,
    // we explicitly do not handle breaking changes
    // they are rare enough and high risk enough that
    // a human should explicitly be in the loop on them
    //MAJOR
}

#[derive(Debug, Clone)]
struct Version {
    major: u64,
    minor: u64,
    patch: u64,
}

impl FromStr for Version {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split('.');
        let major = tokens.next().unwrap().parse()?;
        let minor = tokens.next().unwrap().parse()?;
        let patch = tokens.next().unwrap().parse()?;
        Ok(Version {
            major,
            minor,
            patch,
        })
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl Version {
    fn bump(&self, bump: Bump) -> Self {
        let mut new = self.clone();

        match bump {
            Bump::PATCH => new.patch += 1,
            Bump::MINOR => {
                new.minor += 1;
                new.patch = 0
            }
        };
        new
    }
}

#[tokio::main]
async fn main() {
    initialize_logger();

    // these are the crates that we actually care to publish
    let crates = vec![
        "quic/s2n-quic",
        "quic/s2n-quic-core",
        "quic/s2n-quic-crypto",
        "quic/s2n-quic-platform",
        "quic/s2n-quic-rustls",
        "quic/s2n-quic-tls",
        "quic/s2n-quic-tls-default",
        "quic/s2n-quic-transport",
        "quic/s2n-quic-xdp",
        "common/s2n-codec",
    ];

    let mut versions = HashMap::new();
    // use the raw string for find and replace
    let mut manifests_raw = HashMap::new();
    // use the parsed manifest to calculate the dependency tree
    // we can't use cargo tree because it does feature resolution which leaves
    // dependencies out of the build tree
    let mut manifests_parsed = HashMap::new();
    for c in crates.iter() {
        let manifest_path = format!("{c}/Cargo.toml");
        let manifest_string = std::fs::read_to_string(&manifest_path).unwrap();
        let manifest = Manifest::from_path(&manifest_path).unwrap();
        let version: Version = manifest.package().version().parse().unwrap();
        manifests_raw.insert((*c).to_owned(), manifest_string);
        manifests_parsed.insert((*c).to_owned(), manifest);
        versions.insert((*c).to_owned(), version);
    }

    // build dependency graph
    // we want a list of the immediate dependencies for each of our crates of
    // interest. This is used to calculate which crates need to have their
    // versions bumped.
    // package -> [consumers], e.g. s2n-quic-transport -> [s2n-quic]
    let dep_graph = build_dep_graph(&crates, manifests_parsed);
    for (dep, cons) in dep_graph.iter() {
        println!("{} -> {:?}", dep, cons);
    }

    // get the hash of the last commit that was released
    let (version, previous_release_commit) = get_release().await;
    info!("latest release: {version}");
    info!("latest release commit: {previous_release_commit}");

    // get a list of the commits that have touched each crate
    let mut crate_commits = HashMap::new();
    for c in crates.iter() {
        crate_commits.insert(
            (*c).to_owned(),
            get_crate_commits(*c, &previous_release_commit),
        );
    }

    // everything that has been touched gets a patch bump
    // if a commit has a "feat", then the crate gets a minor bump
    let mut reasons = HashMap::new();
    let mut bumps = HashMap::new();
    for c in crates.iter() {
        if let Some((bump, reason)) = calculate_initial_bumps(crate_commits.get(*c).unwrap()) {
            bumps.insert((*c).to_owned(), bump);
            reasons.insert((*c).to_owned(), reason);
        }
    }

    // cascade update
    // if a crate has a version bump, then all of it's consumers need at least
    // a patch update
    consumer_bump(&crates, &dep_graph, &mut bumps, &mut reasons);

    for (c, k) in reasons.iter() {
        println!("{} has a {:?} bump because of", c, bumps.get(c).unwrap());
        for (hash, description) in k.iter() {
            println!("\t{}", description);
        }
    }

    println!("bumps after cascade: {:?}", &bumps);

    // replace crate versions
    for (c, b) in bumps.iter() {
        // we need to bump the version
        let v = versions.get(c).unwrap();
        let old_version = format!("version = \"{}\"", v);
        let new_version = format!("version = \"{}\"", v.bump(*b));
        if *b == Bump::MINOR {
            println!("bumped: {}, {}", old_version, new_version);
        }
        let new_manifest = manifests_raw
            .get(c.as_str())
            .unwrap()
            .replace(&old_version, &new_version);
        manifests_raw.insert(c.to_string(), new_manifest);
    }

    // for each crate that was updated
    for (c, b) in bumps.iter() {
        // calculate the dependency string other's used to refer to it
        let v = versions.get(c).unwrap();
        // s2n-codec = { version = "=0.4.0", path = "../../common/s2n-codec", default-features = false }
        let old_dep = format!("{} = {{ version = \"={}\",", crate_name_from_path(c), v);
        let new_dep = format!(
            "{} = {{ version = \"={}\",",
            crate_name_from_path(c),
            v.bump(*b)
        );
        // if other packages consumed it, update their dependency stuff
        if let Some(consumers) = dep_graph.get(crate_name_from_path(c)) {
            for consumer in consumers {
                println!("checking {} for {}", consumer, old_dep);
                let new_manifest = manifests_raw
                    .get(consumer.as_str())
                    .unwrap()
                    .replace(&old_dep, &new_dep);
                manifests_raw.insert(consumer.to_string(), new_manifest);
            }
        }
    }

    for (crate_path, manifest) in manifests_raw {
        //let name = crate_name_from_path(crate_path);
        let path = crate_path.to_owned() + "/Cargo.toml";
        println!("writing to {path}");
        let mut output = std::fs::File::create(path).unwrap();
        write!(output, "{}", manifest);
    }

    // create a pr with the changes

    // ensure that no new commits have happened since then
}
/// returns a map from crate to consumers of the crate
fn build_dep_graph(
    crates: &Vec<&str>,
    manifests: HashMap<String, Manifest>,
) -> HashMap<String, Vec<String>> {
    let mut dep_graph: HashMap<String, Vec<String>> = HashMap::new();

    let crate_names: Vec<String> = crates
        .iter()
        .map(|path| path.split_once("/").unwrap().1.to_owned())
        .collect();

    let mut name_to_path = HashMap::new();
    for (crate_path, crate_name) in std::iter::zip(crates, crate_names.clone()) {
        name_to_path.insert(crate_name, *crate_path);
    }
    // we can not just look at the dependency graph for, e.g. s2n-quic, because
    // some crates, like s2n-quic-rustls won't show up in it. So we look at each
    for name in crates.iter().cloned() {
        println!("trying to get {:?}", name);
        println!("manifest keys {:?}", manifests.keys());
        let manifest = manifests.get(name).unwrap();
        //if name.contains("tls-default") {
        //    println!("manifest target: {:?}", manifest.target);
        //    panic!();
        //}
        let deps: Vec<String> = manifest
            .dependencies
            .iter()
            .chain(manifest.build_dependencies.iter())
            .chain(manifest.dev_dependencies.iter())
            .chain(
                manifest
                    .target
                    .values()
                    .map(|target| {
                        target
                            .dependencies
                            .iter()
                            .chain(target.dev_dependencies.iter())
                            .chain(target.build_dependencies.iter())
                    })
                    .flatten(),
            )
            .map(|(dep_name, dep_info)| dep_name.to_owned())
            .filter(|dep_name| crate_names.contains(dep_name))
            .collect();
        //let deps = get_dependencies(&name, &crate_names);
        for d in deps {
            dep_graph.entry(d).or_default().push(name.to_owned());
        }
    }
    dep_graph
}

// need to switch dep graph to just use paths

/// `get_dependencies` shells out to `cargo tree` to calculate the direct
/// dependencies for the crate `name`. All crates except for those in
/// `interest_list` are dropped from the dependency tree.
fn get_dependencies(name: &str, interest_list: &Vec<&str>) -> Vec<String> {
    // example output of the dep tree
    // 0s2n-quic v1.17.1 (/home/ubuntu/workspace/s2n-quic/quic/s2n-quic)
    // 1bytes v1.4.0
    // 1cfg-if v1.0.0
    // 1cuckoofilter v0.5.0
    // 2byteorder v1.4.3
    // 2fnv v1.0.7
    // 2rand v0.7.3
    // 3getrandom v0.1.16
    // 4cfg-if v1.0.0
    // 4libc v0.2.140
    // 3libc v0.2.140
    // 3rand_chacha v0.2.2
    // 4ppv-lite86 v0.2.17

    let dep_tree = Command::new("cargo")
        .arg("tree")
        .arg("-p")
        .arg(name)
        .arg("-e")
        .arg("normal")
        .arg("--prefix")
        .arg("depth")
        .output()
        .unwrap();

    // parse std out into a string
    let output = String::from_utf8(dep_tree.stdout).unwrap();

    // I could probably parse this more easily with a regex, but for now this is
    // fine
    output
        .lines()
        .map(|l| {
            let depth_end = l.find(|c: char| c.is_alphabetic()).unwrap();
            let depth = l[0..depth_end].parse::<u8>().unwrap();
            let crate_name_end = l.find(' ').unwrap();
            let crate_name = &l[depth_end..crate_name_end];
            (depth, crate_name)
        })
        // only look at direct dependencies
        .filter(|(depth, _name)| *depth == 1)
        // we only care about the crates we publish
        .filter(|(_depth, name)| interest_list.contains(name))
        .map(|(_depth, name)| name.to_owned())
        .collect()
}

async fn get_release() -> (String, String) {
    let octocrab = octocrab::instance();

    let page = octocrab
        .repos("aws", "s2n-quic")
        .releases()
        .get_latest()
        .await
        .unwrap();
    let version = page.tag_name;
    let commit = page.target_commitish;
    (version, commit)
}

fn get_crate_commits(krate: &str, previous_release_commit: &str) -> Vec<(String, String)> {
    // example output from the git log output
    // 41adde17 failing attempt with single dependency tree
    // 9d44dcde retrieve version and commit from github
    // 7497f1ab initial commit of bumper crate

    let git_commits = Command::new("git")
        .arg("log")
        .arg("--oneline")
        // don't add extraneous information like branch name
        .arg("--no-decorate")
        // use the full commit hash because we aren't barbarians
        .arg("--no-abbrev-commit")
        // include all commits from the previous_release_commit to HEAD
        .arg(format!("{previous_release_commit}..origin/main"))
        // only include commits that touched "crate" path
        .arg(format!("{krate}"))
        .output()
        .unwrap();
    String::from_utf8(git_commits.stdout)
        .unwrap()
        .lines()
        .map(|line| line.split_once(' ').unwrap())
        .map(|(hash, description)| (hash.to_owned(), description.to_owned()))
        .collect()
}

fn calculate_initial_bumps(
    commits: &Vec<(String, String)>,
) -> Option<(Bump, Vec<(String, String)>)> {
    if commits.is_empty() {
        return None;
    }

    let feat_commit: Vec<(String, String)> = commits
        .iter()
        .cloned()
        .filter(|(hash, description)| description.starts_with("feat"))
        .collect();

    if !feat_commit.is_empty() {
        Some((Bump::MINOR, feat_commit))
    } else {
        Some((Bump::PATCH, commits.clone()))
    }
}

fn consumer_bump(
    crates: &Vec<&str>,
    dep_graph: &HashMap<String, Vec<String>>,
    bumps: &mut HashMap<String, Bump>,
    reasons: &mut HashMap<String, Vec<(String, String)>>,
) {
    // for any package that has been changed, it's consumers must at least do a
    // minor bump to actually consume the updated dependency
    loop {
        // we have a "cascading" update as we go through the dependency chain,
        // so keep looping until we have reached a steady state.
        let mut change = false;
        // iterate over the crates instead of bumps to avoid the mut borrow issues
        // this loop could be much more efficient, but my computer is fast and I'm
        // busy, so here we go
        for release_crate in crates.iter() {
            // if a crate is going to have a version bump, then all of the
            // consumers must have at least a patch bump
            if bumps.contains_key(*release_crate) {
                let consumers = match dep_graph.get(crate_name_from_path(*release_crate)) {
                    Some(c) => c,
                    // might not have any consumers, in which case skip
                    None => continue,
                };
                for consumer in consumers {
                    if !bumps.contains_key(consumer) {
                        change = true;
                        bumps.insert(consumer.clone(), Bump::PATCH);
                        reasons.insert(
                            consumer.clone(),
                            vec![("META".to_owned(), "TRANSITIVE UPDATE".to_owned())],
                        );
                    }
                }
            }
        }

        if !change {
            break;
        }
    }
}

fn crate_name_from_path(path: &str) -> &str {
    path.split_once("/").unwrap().1
}

fn initialize_logger() {
    // always write to the same file, and don't rotate it. This would be a
    // bad idea for a long running process, but is useful to make sure that
    // all the logs of our program end up in the same file.
    tracing_subscriber::fmt()
        .with_max_level(Level::DEBUG)
        .init();
}
