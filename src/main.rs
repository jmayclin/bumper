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

        // how was this not working earlier?
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
        "quic/s2n-quic-core",
        "quic/s2n-quic-platform",
        "quic/s2n-quic-crypto",
        "quic/s2n-quic-rustls",
        "quic/s2n-quic-tls",
        "quic/s2n-quic-tls-default",
        "quic/s2n-quic-transport",
        "quic/s2n-quic",
        "common/s2n-codec",
    ];

    // build dependency graph
    // we want a list of the immediate dependencies for each of our crates of
    // interest. This is used to calculate which crates need to have their
    // versions bumped.
    // package -> [consumers], e.g. s2n-quic-transport -> [s2n-quic]
    let dep_graph = build_dep_graph(&crates);
    for (dep, cons) in dep_graph.iter() {
        println!("{} -> {:?}", dep, cons);
    }

    // get the hash of the last commit that was released
    let (version, previous_release_commit) = get_release().await;
    info!("latest release: {version}");
    info!("latest release commit: {previous_release_commit}");

    // get a list of all the commits that have happened since that release
    let commits = get_commits(&previous_release_commit);

    let changed_files = get_changed_files_range(&previous_release_commit);
    let changed_crates = get_changed_crates(changed_files, &crates);
    debug!("The following crates have had files changed:\n");
    for c in changed_crates.iter() {
        debug!("\t{c}");
    }

    let mut bumps = HashMap::new();

    // each crate that has been changed needs at least a patch bump
    for release_crate in changed_crates {
        bumps.insert(release_crate, Bump::PATCH);
    }
    println!("bumps after patch: {:?}", &bumps);

    let feat_files = commits
        .iter()
        .filter(|(_hash, description)| description.starts_with("feat"))
        .inspect(|(_hash, description)| println!("feature commit found: {description}"))
        .map(|(hash, _desciption)| get_changed_files(hash))
        .inspect(|col| println!("{:?}", col))
        .flatten()
        .collect::<HashSet<String>>()
        .into_iter()
        .collect();
    let changed_crates: Vec<String> = get_changed_crates(feat_files, &crates);
    debug!("The following crates have had feature changes:\n");
    for c in changed_crates.iter() {
        debug!("\t{c}");
    }

    for release_crate in changed_crates {
        bumps.insert(release_crate, Bump::MINOR);
    }

    debug!("{:?}", bumps);

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
                    }
                }
            }
        }

        if !change {
            break;
        }
    }

    println!("bumps after cascade: {:?}", &bumps);

    let mut versions = HashMap::new();
    let mut manifests = HashMap::new();
    for c in crates.iter() {
        let manifest_path = format!("{c}/Cargo.toml");
        let manifest_string = std::fs::read_to_string(&manifest_path).unwrap();
        let manifest = Manifest::from_path(&manifest_path).unwrap();
        let version: Version = manifest.package().version().parse().unwrap();
        manifests.insert(*c, manifest_string);
        versions.insert((*c).to_owned(), version);
    }

    println!("parsed versions: {:?}", versions);

    // replace crate versions
    for (c, b) in bumps.iter() {
        // we need to bump the version
        let v = versions.get(c).unwrap();
        let old_version = format!("version = \"{}\"", v);
        let new_version = format!("version = \"{}", v.bump(*b));
        if *b == Bump::MINOR {
            println!("bumped: {}, {}", old_version, new_version);
        }
        let new_manifest = manifests
            .get(c.as_str())
            .unwrap()
            .replace(&old_version, &new_version);
        manifests.insert(c, new_manifest);
    }

    // replace all of the dependencies
    for (c, b) in bumps.iter() {
        let v = versions.get(c).unwrap();
        // s2n-codec = { version = "=0.4.0", path = "../../common/s2n-codec", default-features = false }
        let old_dep = format!("{} = {{ version = \"={}\",", crate_name_from_path(c), v);
        let new_dep = format!(
            "{} = {{ version = \"={}\",",
            crate_name_from_path(c),
            v.bump(*b)
        );
        if let Some(consumers) = dep_graph.get(crate_name_from_path(c)) {
            for consumer in consumers {
                let new_manifest = manifests
                    .get(consumer.as_str())
                    .unwrap()
                    .replace(&old_dep, &new_dep);
                manifests.insert(consumer, new_manifest);
            }
        }
    }

    for (crate_path, manifest) in manifests {
        let name = crate_name_from_path(crate_path);
        let mut output = std::fs::File::create(name).unwrap();
        write!(output, "{}", manifest);
    }

    // create a pr with the changes

    // ensure that no new commits have happened since then
}

fn build_dep_graph(crates: &Vec<&str>) -> HashMap<String, Vec<String>> {
    let mut dep_graph: HashMap<String, Vec<String>> = HashMap::new();

    let crate_names: Vec<&str> = crates
        .iter()
        .map(|path| path.split_once("/").unwrap().1)
        .collect();

    let mut name_to_path = HashMap::new();
    for (crate_path, crate_name) in std::iter::zip(crates, crate_names.clone()) {
        name_to_path.insert(crate_name, *crate_path);
    }

    // we can not just look at the dependency graph for, e.g. s2n-quic, because
    // some crates, like s2n-quic-rustls won't show up in it. So we look at each
    for name in crate_names.iter().cloned() {
        let deps = get_dependencies(name, &crate_names);
        for d in deps {
            dep_graph
                .entry(d)
                .or_default()
                .push((*name_to_path.get(name).unwrap()).to_owned());
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
    //return ("v1.17.1".to_owned(), "a6c8fbe52596564d632343e7cb4969954a1098ff".to_owned());
    //return (
    //    "v1.17.0".to_owned(),
    //    "db9671be670549421845e5b869b7a2e0735d2aca".to_owned(),
    //);
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
/// get_commits returns all of the commits that have happend since
/// previous_release_commit
fn get_commits(previous_release_commit: &str) -> Vec<(String, String)> {
    // example output from the git log output
    // 41adde17 failing attempt with single dependency tree
    // 9d44dcde retrieve version and commit from github
    // 7497f1ab initial commit of bumper crate

    let git_commits = Command::new("git")
        .arg("log")
        .arg("--oneline")
        // don't add extraenous information like branch name
        .arg("--no-decorate")
        // use the full commit hash because we aren't barbarians
        .arg("--no-abbrev-commit")
        // include all commits from the previous_release_commit to HEAD
        .arg(format!("{previous_release_commit}..HEAD"))
        .output()
        .unwrap();
    String::from_utf8(git_commits.stdout)
        .unwrap()
        .lines()
        .map(|line| line.split_once(' ').unwrap())
        .map(|(hash, description)| (hash.to_owned(), description.to_owned()))
        .collect()
}

/// `get_changed_files_range` will return all the files that have been changed
/// after `previous_release_commit`.
fn get_changed_files_range(previous_release_commit: &str) -> Vec<String> {
    let file_diff = Command::new("git")
        .arg("diff")
        .arg(previous_release_commit)
        .arg("--name-only")
        .output()
        .unwrap();
    String::from_utf8(file_diff.stdout)
        .unwrap()
        .lines()
        .map(|line| line.to_owned())
        .collect()
}

/// `get_changed_files` will get the files that were changed in a specific
/// `commit` by shelling out to `git diff-tree`.
fn get_changed_files(commit: &str) -> Vec<String> {
    let file_diff = Command::new("git")
        .arg("diff-tree")
        .arg("--no-commit-id")
        .arg("--name-only")
        .arg(commit)
        .arg("-r")
        .output()
        .unwrap();
    String::from_utf8(file_diff.stdout)
        .unwrap()
        .lines()
        .map(|line| line.to_owned())
        .collect()
}

fn crate_name_from_path(path: &str) -> &str {
    path.split_once("/").unwrap().1
}

fn get_changed_crates(changed_files: Vec<String>, crates: &Vec<&str>) -> Vec<String> {
    crates
        .iter()
        .filter(|release_crate| {
            changed_files
                .iter()
                .any(|file| file.starts_with(&format!("{}/", release_crate)))
        })
        .map(|release_crate| (*release_crate).to_owned())
        .collect()
}

fn initialize_logger() {
        // always write to the same file, and don't rotate it. This would be a
        // bad idea for a long running process, but is useful to make sure that
        // all the logs of our program end up in the same file.
        tracing_subscriber::fmt()
            .with_max_level(Level::DEBUG)
            .init();
}

/// get the reason that a particular crate had the specified version bump
fn get_reasons(crates: &Vec<&str>, bumps: HashMap<String, Bump>) -> HashMap<String, Vec<String>> {
    HashMap::new()

}