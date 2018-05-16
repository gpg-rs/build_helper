use std::borrow::BorrowMut;
use std::collections::HashSet;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};
use std::result;
use std::str;

use cc;

macro_rules! scan {
    ($string:expr, $sep:expr; $($x:ty),+) => ({
        let mut iter = $string.split($sep);
        ($(iter.next().and_then(|word| word.parse::<$x>().ok()),)*)
    });
    ($string:expr; $($x:ty),+) => (
        scan!($string, char::is_whitespace; $($x),+)
    );
}

pub type Result<T> = result::Result<T, ()>;

pub trait ContextExt<T> {
    fn context<D: fmt::Display>(self, msg: D) -> Result<T>;
}

impl<T, E: fmt::Display> ContextExt<T> for result::Result<T, E> {
    fn context<D: fmt::Display>(self, msg: D) -> Result<T> {
        self.map_err(|e| eprintln!("{}: {}", msg, e))
    }
}

impl<T> ContextExt<T> for Option<T> {
    fn context<D: fmt::Display>(self, msg: D) -> Result<T> {
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
    !s.eq_ignore_ascii_case("false") && !s.eq_ignore_ascii_case("no")
        && !s.eq_ignore_ascii_case("off")
}

pub fn make_env_name<S: AsRef<str>>(value: S) -> String {
    let mut value = value.as_ref().replace('-', "_");
    value.make_ascii_uppercase();
    value
}

pub fn msys_compatible<P: AsRef<OsStr>>(path: P) -> Result<OsString> {
    if !cfg!(windows) {
        return Ok(path.as_ref().to_owned());
    }

    let mut path = path.as_ref()
        .to_str()
        .context("path is not valid utf-8")?
        .to_owned();
    if let Some(b'a'...b'z') = path.as_bytes().first().map(u8::to_ascii_lowercase) {
        if path.split_at(1).1.starts_with(":\\") {
            (&mut path[..1]).make_ascii_lowercase();
            path.remove(1);
            path.insert(0, '/');
        }
    }
    Ok(path.replace("\\", "/").into())
}

pub fn gnu_target(target: &str) -> String {
    match target {
        "i686-pc-windows-gnu" => "i686-w64-mingw32".to_string(),
        "x86_64-pc-windows-gnu" => "x86_64-w64-mingw32".to_string(),
        s if s.starts_with("i686-unknown") || s.starts_with("x86_64-unknown") => {
            s.replacen("unknown", "pc", 1)
        }
        s => s.to_string(),
    }
}

pub fn for_each_line<P, F>(path: P, mut f: F) -> Result<()>
where
    P: AsRef<Path>,
    F: FnMut(&str) -> Result<()>, {
    let path = path.as_ref();
    let mut file = BufReader::new(File::open(path).context(format_args!("failed to open file: {}", path.display()))?);
    let mut line = String::new();
    loop {
        line.clear();
        if file.read_line(&mut line).or(Err(()))? == 0 {
            return Ok(());
        }
        f(&line)?;
    }
}

pub fn run<C>(mut cmd: C) -> Result<String>
where C: BorrowMut<Command> {
    let cmd = cmd.borrow_mut();
    eprintln!("running: {:?}", cmd);
    let output = cmd.stdin(Stdio::null())
        .spawn()
        .and_then(|c| c.wait_with_output())
        .context("failed to execute command")?;
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

#[derive(Debug, Clone)]
pub struct Config {
    pub version: Option<String>,
    pub root: Option<PathBuf>,
    pub include_dir: HashSet<PathBuf>,
    pub lib_dir: HashSet<PathBuf>,
    pub libs: HashSet<OsString>,
    pub statik: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            version: None,
            root: None,
            include_dir: HashSet::default(),
            lib_dir: HashSet::default(),
            libs: HashSet::default(),
            statik: false,
        }
    }
}

impl Config {
    pub fn from_root(root: PathBuf) -> Self {
        let mut config = Self {
            root: Some(root.clone()),
            ..Self::default()
        };
        let include = root.join("include");
        if include.exists() {
            config.include_dir.insert(include);
        }
        let lib = root.join("lib");
        if lib.exists() {
            config.lib_dir.insert(lib);
        }
        config
    }

    pub fn try_detect_version(&mut self, header: &str, prefix: &str) -> Result<()> {
        eprintln!("detecting installed version of libgcrypt");
        let defaults = &["/usr/include".as_ref(), "/usr/local/include".as_ref()];
        for dir in self.include_dir.iter().map(|x| Path::new(x)).chain(self.root.iter().map(|x| x.as_ref())).chain(defaults.iter().cloned()) {
            let name = dir.join(header);
            let mut file = match File::open(name.clone()) {
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
            let mut line = String::new();
            loop {
                line.clear();
                if file.read_line(&mut line).unwrap() == 0 {
                    break;
                }

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
                    if val.ends_with(".la") {
                        self.parse_libtool_file(val)?;
                    } else {
                        self.libs.insert(val.into());
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }

    pub fn parse_libtool_file<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        for_each_line(path, |l| {
            if l.trim().starts_with("old_library") {
                if let Some(s) = l.splitn(2, '=').nth(1) {
                    let mut s = s.trim().trim_matches('\'');
                    if s.ends_with(".a") {
                        if s.starts_with("lib") {
                            s = s.split_at(3).1;
                        }
                        s = s.split_at(s.len() - 2).0;
                    }

                    if !s.is_empty() {
                        self.libs.insert(format!("static={}", s).into());
                    }
                }
            }
            if l.trim().starts_with("dependency_libs") {
                if let Some(s) = l.splitn(2, '=').nth(1) {
                    self.parse_flags(s.trim().trim_matches('\''))?
                }
            }
            Ok(())
        })
    }

    pub fn print(&self) {
        if let Some(ref v) = self.version {
            println!("cargo:version={}", v);
        }
        if let Some(ref v) = self.root {
            println!("cargo:root={}", v.display());
        }
        if !self.include_dir.is_empty() {
            println!(
                "cargo:include={}",
                env::join_paths(&self.include_dir)
                    .unwrap()
                    .to_string_lossy()
            );
        } else if let Some(ref v) = self.root {
            println!("cargo:include={}/include", v.display())
        }
        if !self.lib_dir.is_empty() {
            println!(
                "cargo:lib_dir={}",
                env::join_paths(&self.lib_dir).unwrap().to_string_lossy()
            );
            for dir in &self.lib_dir {
                println!("cargo:rustc-link-search=native={}", dir.to_string_lossy());
            }
        } else if let Some(ref v) = self.root {
            println!("cargo:rustc-link-search=native={}/lib", v.display())
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
                } else { default_mode };
                println!("cargo:rustc-link-lib={}{}", mode, lib)
            }
        }
    }
}

pub struct Project {
    pub name: String,
    pub prefix: String,
    pub links: Option<String>,
    pub compiler: cc::Tool,
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
        let prefix = make_env_name(&name);
        let links = env::var("CARGO_MANIFEST_LINKS").ok();
        let compiler = cc::Build::new().get_compiler();
        let host = env::var("HOST").unwrap();
        let target = env::var("TARGET").unwrap();
        let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
        Self {
            name,
            prefix,
            links,
            compiler,
            host,
            target,
            out_dir,
        }
    }
}

impl Project {
    pub fn configure<F: FnOnce(&Self) -> Result<Config>>(&self, f: F) {
        match f(self) {
            Ok(cfg) => cfg.print(),
            Err(_) => process::exit(1),
        }
    }

    pub fn try_env(&self) -> Result<Config> {
        let include_dir = get_env(self.prefix.clone() + "_INCLUDE");
        let lib_dir = get_env(self.prefix.clone() + "_LIB_DIR");
        let libs = get_env(self.prefix.clone() + "_LIBS");
        if libs.is_some() || lib_dir.is_some() {
            let statik = get_env(self.prefix.clone() + "_STATIC").map_or(false, |s| is_truthy(s));
            let include_dir = include_dir.iter().flat_map(env::split_paths).map(|x| x.into()).collect();
            let lib_dir = lib_dir.iter().flat_map(env::split_paths).map(|x| x.into()).collect();
            let libs = libs.as_ref()
                .map(|s| &**s)
                .or_else(|| self.links.as_ref().map(|s| s.as_ref()))
                .unwrap_or(self.name.as_ref());
            let libs = env::split_paths(libs).map(|x| x.into()).collect();
            Ok(Config {
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

    pub fn try_build<F>(&self, f: F) -> Result<Config>
    where F: FnOnce(&Self) -> Result<Config> {
        if get_env(self.prefix.clone() + "_USE_BUNDLED").map_or(cfg!(feature = "bundled"), |s| is_truthy(s)) {
            let _ = run(Command::new("git").args(&["submodule", "update", "--init"]));

            if let r @ Ok(_) = f(self) {
                return r;
            }
        }
        Err(())
    }

    pub fn new_build<S: AsRef<OsStr>>(&self, name: S) -> Result<Build> {
        Build::new(self, name)
    }
}

pub struct Build<'a> {
    pub project: &'a Project,
    pub src: PathBuf,
    pub build: PathBuf,
}

impl<'a> Build<'a> {
    pub fn new<S: AsRef<OsStr>>(project: &'a Project, name: S) -> Result<Self> {
        let src = PathBuf::from(env::current_dir().unwrap()).join(name.as_ref());
        let mut build = project.out_dir.join("build");
        build.push(name.as_ref());

        fs::create_dir_all(&build).context("unable to create build directory")?;

        Ok(Self {
            project,
            src,
            build,
        })
    }

    pub fn config(&self) -> Config {
        let mut config = Config::from_root(self.project.out_dir.clone().into());
        config.statik = true;
        config
    }

    pub fn configure_cmd(&self) -> Result<Command> {
        let mut cmd = Command::new("sh");
        let mut cc = self.project.compiler.cc_env();
        if cc.is_empty() {
            cc = self.project.compiler.path().as_os_str().to_owned();
        }
        cmd.current_dir(&self.build)
            .env("CC", cc)
            .env("CFLAGS", self.project.compiler.cflags_env())
            .arg(msys_compatible(self.src.join("configure"))?);
        if self.project.host != self.project.target {
            cmd.arg("--build").arg(gnu_target(&self.project.host));
            cmd.arg("--host").arg(gnu_target(&self.project.target));
        }
        cmd.arg("--disable-dependency-tracking");
        cmd.arg("--enable-static");
        cmd.arg("--disable-shared");
        cmd.arg("--with-pic");
        cmd.arg({
            let mut s = OsString::from("--prefix=");
            s.push(msys_compatible(&self.project.out_dir)?);
            s
        });
        Ok(cmd)
    }

    pub fn make_cmd(&self) -> Command {
        let name = if cfg!(any(target_os = "freebsd", target_os = "dragonfly")) {
            "gmake"
        } else {
            "make"
        };
        let mut cmd = Command::new(name);
        cmd.env_remove("DESTDIR");
        if cfg!(windows) {
            cmd.env_remove("MAKEFLAGS").env_remove("MFLAGS");
        }
        cmd.current_dir(&self.build);
        cmd
    }
}
