#![allow(dead_code, unused_macros)]
use std::{
    borrow::BorrowMut,
    collections::HashSet,
    env,
    ffi::{OsStr, OsString},
    fmt,
    fs::File,
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    result, str,
};

macro_rules! scan {
    ($string:expr, $sep:expr; $($x:ty),+) => ({
        let mut iter = $string.split($sep);
        ($(iter.next().and_then(|word| word.parse::<$x>().ok()),)*)
    });
    ($string:expr; $($x:ty),+) => (
        scan!($string, char::is_whitespace; $($x),+)
    );
}

macro_rules! warn_err {
    ($res:expr, $msg:expr $(,$args:expr)*) => {
        $res.warn_err(format_args!($msg $(,$args)*))
    };
}

pub type Result<T> = result::Result<T, ()>;

pub trait ResultExt<T> {
    fn warn_err<D: fmt::Display>(self, msg: D) -> Result<T>;
}

impl<T, E: fmt::Debug> ResultExt<T> for result::Result<T, E> {
    fn warn_err<D: fmt::Display>(self, msg: D) -> Result<T> {
        self.map_err(|e| eprintln!("{}: {:?}", msg, e))
    }
}

impl<T> ResultExt<T> for Option<T> {
    fn warn_err<D: fmt::Display>(self, msg: D) -> Result<T> {
        self.ok_or_else(|| eprintln!("{}", msg))
    }
}

pub fn get_env<S: AsRef<str>>(name: S) -> Option<OsString> {
    let name = name.as_ref();
    println!("cargo:rerun-if-env-changed={}", name);
    env::var_os(name)
}

// Based on cmake boolean variables
pub fn is_truthy<S: AsRef<OsStr>>(value: S) -> bool {
    let value = value.as_ref();
    if value.is_empty() {
        return false;
    }
    let s = match value.to_str() {
        Some(s) => s,
        None => return true,
    };
    !s.eq_ignore_ascii_case("false")
        && !s.eq_ignore_ascii_case("no")
        && !s.eq_ignore_ascii_case("off")
}

pub fn run<C>(mut cmd: C) -> Result<String>
where C: BorrowMut<Command> {
    let cmd = cmd.borrow_mut();
    eprintln!("running: {:?}", cmd);
    let output = cmd
        .stdin(Stdio::null())
        .spawn()
        .and_then(|c| c.wait_with_output())
        .warn_err("failed to execute command")?;
    if output.status.success() {
        String::from_utf8(output.stdout).or(Err(()))
    } else {
        eprintln!(
            "command did not execute successfully, got: {}",
            output.status
        );
        Err(())
    }
}

pub fn output<C>(mut cmd: C) -> Result<String>
where C: BorrowMut<Command> {
    run(cmd.borrow_mut().stdout(Stdio::piped()))
}

pub struct Project {
    pub name: String,
    pub prefix: String,
    pub links: String,
    pub host: String,
    pub target: String,
    pub out_dir: PathBuf,
}

impl Default for Project {
    fn default() -> Self {
        let name = {
            let mut name = env::var("CARGO_PKG_NAME").unwrap();
            if name.ends_with("-sys") {
                let len = name.len() - 4;
                name.truncate(len);
            }
            name
        };
        let prefix = {
            let mut prefix = name.replace('-', "_");
            prefix.make_ascii_uppercase();
            prefix
        };
        let links = env::var("CARGO_MANIFEST_LINKS").ok().unwrap_or_else(|| {
            if name.starts_with("lib") {
                String::from(&name[3..])
            } else {
                name.clone()
            }
        });
        let host = env::var("HOST").unwrap();
        let target = env::var("TARGET").unwrap();
        let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
        Self {
            name,
            prefix,
            links,
            host,
            target,
            out_dir,
        }
    }
}

impl Project {
    pub fn try_env(&self) -> Result<Config> {
        let prefix = get_env(self.prefix.clone() + "_PREFIX").map(PathBuf::from);
        let include_dir = get_env(self.prefix.clone() + "_INCLUDE")
            .or_else(|| prefix.as_ref().map(|x| x.join("include").into()));
        let lib_dir = get_env(self.prefix.clone() + "_LIB_DIR")
            .or_else(|| prefix.as_ref().map(|x| x.join("lib").into()));
        let libs = get_env(self.prefix.clone() + "_LIBS");
        if libs.is_some() || lib_dir.is_some() {
            let statik = get_env(self.prefix.clone() + "_STATIC").map_or(false, |s| is_truthy(s));
            let include_dir = include_dir.iter().flat_map(env::split_paths).collect();
            let lib_dir = lib_dir.iter().flat_map(env::split_paths).collect();
            let libs = libs.as_ref().map(|s| &**s).unwrap_or(self.links.as_ref());
            let libs = env::split_paths(libs).map(|x| x.into()).collect();
            Ok(Config {
                prefix,
                include_dir,
                lib_dir,
                libs,
                statik,
                ..Config::default()
            })
        } else {
            Err(())
        }
    }

    pub fn try_config<C>(&self, cmd: C) -> Result<Config>
    where C: BorrowMut<Command> {
        let output = output(cmd)?;
        let mut config = Config::default();
        config.parse_flags(&output)?;
        Ok(config)
    }
}

#[derive(Debug, Clone)]
pub struct Config {
    pub version: Option<String>,
    pub prefix: Option<PathBuf>,
    pub include_dir: HashSet<PathBuf>,
    pub lib_dir: HashSet<PathBuf>,
    pub libs: HashSet<OsString>,
    pub statik: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            version: None,
            prefix: None,
            include_dir: HashSet::default(),
            lib_dir: HashSet::default(),
            libs: HashSet::default(),
            statik: false,
        }
    }
}

impl Config {
    pub fn try_detect_version(&mut self, header: &str, prefix: &str) -> Result<()> {
        eprintln!("detecting installed version...");
        let defaults = &["/usr/include".as_ref(), "/usr/local/include".as_ref()];
        for dir in self
            .include_dir
            .iter()
            .map(Path::new)
            .chain(defaults.iter().cloned())
        {
            let name = dir.join(header);
            let file = match File::open(name.clone()) {
                Ok(f) => BufReader::new(f),
                Err(e) => {
                    if e.kind() == io::ErrorKind::NotFound {
                        eprintln!("skipping non-existent file: {}", name.display());
                    } else {
                        eprintln!("unable to inspect file `{}`: {}", name.display(), e);
                    }
                    continue;
                }
            };
            for line in file.lines() {
                let line = match warn_err!(line, "unable to read file '{}'", name.display()) {
                    Ok(l) => l,
                    Err(_) => break,
                };
                if let Some(p) = line.find(prefix) {
                    if let Some(v) = (&line[p..]).split('\"').nth(1) {
                        eprintln!("found version: {} in {}", v, name.display());
                        self.version = Some(v.trim().into());
                        return Ok(());
                    }
                    break;
                }
            }
        }
        eprintln!("unable to detect version!");
        Err(())
    }

    pub fn parse_flags(&mut self, flags: &str) -> Result<()> {
        let parts = flags.split(|c: char| c.is_ascii_whitespace()).map(|p| {
            if p.starts_with("-") && (p.len() > 2) {
                p.split_at(2)
            } else {
                ("", p)
            }
        });

        for (flag, val) in parts {
            match flag {
                "-I" => {
                    self.include_dir.insert(val.into());
                }
                "-L" => {
                    self.lib_dir.insert(val.into());
                }
                "-l" | "" if !val.is_empty() => {
                    self.libs.insert(val.into());
                }
                _ => (),
            }
        }
        Ok(())
    }

    pub fn write_version_macro(&self, name: &str) {
        use std::io::Write;

        // TODO: refactor this to improve clarity and robustness
        let (major, minor) = self
            .version
            .as_ref()
            .and_then(|v| {
                v.trim()
                    .splitn(2, |c: char| (c != '.') && !c.is_digit(10))
                    .next()
                    .and_then(|v| {
                        let mut components =
                            v.split('.').scan((), |_, x| x.parse::<u8>().ok()).fuse();
                        Some((components.next()?, components.next()?))
                    })
            })
            .expect("cannot parse version");
        let path = PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("version.rs");
        let mut output = File::create(path).unwrap();
        writeln!(
            output,
            "pub const MIN_{}_VERSION: &str = \"{}.{}.0\\0\";",
            name.to_uppercase(),
            major,
            minor
        )
        .unwrap();
        writeln!(
            output,
            "#[macro_export]\nmacro_rules! require_{0}_ver {{\n\
        ($ver:tt => {{ $($t:tt)*  }}) => (require_{0}_ver! {{ $ver => {{ $($t)* }} else {{}} }});",
            name
        )
        .unwrap();
        for i in 0..=minor {
            writeln!(
                output,
                "(({0},{1}) => {{ $($t:tt)* }} else {{ $($u:tt)* }}) => ($($t)*);",
                major, i
            )
            .unwrap();
        }
        for i in 0..major {
            writeln!(
                output,
                "(({0},$ver:tt) => {{ $($t:tt)* }} else {{ $($u:tt)* }}) => ($($t)*);",
                i
            )
            .unwrap();
        }
        writeln!(
            output,
            "($ver:tt => {{ $($t:tt)* }} else {{ $($u:tt)* }}) => ($($u)*);\n}}"
        )
        .unwrap();
    }

    pub fn print(&self) {
        if let Some(ref v) = self.version {
            println!("cargo:version={}", v);
        }
        if !self.include_dir.is_empty() {
            println!(
                "cargo:include={}",
                env::join_paths(&self.include_dir)
                    .unwrap()
                    .to_string_lossy()
            );
        }
        if !self.lib_dir.is_empty() {
            println!(
                "cargo:lib_dir={}",
                env::join_paths(&self.lib_dir).unwrap().to_string_lossy()
            );
            for dir in &self.lib_dir {
                println!("cargo:rustc-link-search=native={}", dir.to_string_lossy());
            }
        }
        if !self.libs.is_empty() {
            println!(
                "cargo:libs={}",
                env::join_paths(&self.libs).unwrap().to_string_lossy()
            );
            let default_mode = if self.statik { "static=" } else { "" };
            for lib in &self.libs {
                let lib = lib.to_string_lossy();
                let mode = if lib.starts_with("static=") || lib.starts_with("dynamic=") {
                    ""
                } else {
                    default_mode
                };
                println!("cargo:rustc-link-lib={}{}", mode, lib)
            }
        }
    }
}
