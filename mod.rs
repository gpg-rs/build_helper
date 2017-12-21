use std::borrow::BorrowMut;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::result;
use std::str;
use std::sync::{Once, ONCE_INIT};

use cc;

pub type Result<T> = result::Result<T, ()>;

macro_rules! lazy {
    ($t:ty: $e:expr) => ({
        use std::mem::ManuallyDrop;
        static INIT: Once = ONCE_INIT;
        static mut VALUE: Option<ManuallyDrop<$t>> = None;
        INIT.call_once(|| {
            let r = $e;
            unsafe {
                VALUE = Some(ManuallyDrop::new(r));
            }
        });
        unsafe {
            VALUE.as_ref().unwrap()
        }
    });
}

pub fn host() -> &'static str {
    lazy!(String: env::var("HOST").unwrap())
}

pub fn target() -> &'static str {
    lazy!(String: env::var("TARGET").unwrap())
}

pub fn out_dir() -> &'static Path {
    lazy!(PathBuf: PathBuf::from(env::var_os("OUT_DIR").unwrap()))
}

pub fn get_env(name: &str) -> Option<OsString> {
    println!("cargo:rerun-if-env-changed={}", name);
    env::var_os(name)
}

pub fn for_each_line<P: AsRef<Path>, F: FnMut(&str)>(path: P, mut f: F) -> Result<()> {
    let mut file = BufReader::new(File::open(path).or(Err(()))?);
    let mut line = String::new();
    loop {
        line.clear();
        if file.read_line(&mut line).or(Err(()))? == 0 {
            return Ok(());
        }
        f(&line);
    }
}

pub fn run<C>(mut cmd: C) -> Result<String>
where C: BorrowMut<Command> {
    let cmd = cmd.borrow_mut();
    eprintln!("running: {:?}", cmd);
    match cmd.stdin(Stdio::null())
        .spawn()
        .and_then(|c| c.wait_with_output())
    {
        Ok(output) => if output.status.success() {
            String::from_utf8(output.stdout).or(Err(()))
        } else {
            eprintln!(
                "command did not execute successfully, got: {}",
                output.status
            );
            Err(())
        },
        Err(e) => {
            eprintln!("failed to execute command: {}", e);
            Err(())
        }
    }
}

pub fn output<C>(mut cmd: C) -> Result<String>
where C: BorrowMut<Command> {
    run(cmd.borrow_mut().stdout(Stdio::piped()))
}

pub fn msys_compatible<P: AsRef<OsStr>>(path: P) -> Result<OsString> {
    use std::ascii::AsciiExt;

    if !cfg!(windows) {
        return Ok(path.as_ref().to_owned());
    }

    let mut path = path.as_ref()
        .to_str()
        .ok_or_else(|| eprintln!("path is not valid utf-8"))?
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

pub struct Config {
    pub compiler: cc::Tool,
    pub src: PathBuf,
    pub dst: PathBuf,
    pub build: PathBuf,
}

impl Config {
    pub fn new<S: AsRef<OsStr>>(name: S) -> Result<Config> {
        let compiler = cc::Build::new().get_compiler();
        let src = PathBuf::from(env::current_dir().unwrap()).join(name.as_ref());
        let dst = out_dir().to_owned();
        let mut build = dst.join("build");
        build.push(name.as_ref());

        fs::create_dir_all(&build)
            .map_err(|e| eprintln!("unable to create build directory: {}", e))?;

        Ok(Config {
            compiler,
            src,
            dst,
            build,
        })
    }

    pub fn configure(&self) -> Result<Command> {
        let mut cmd = Command::new("sh");
        cmd.current_dir(&self.build)
            .env("CC", self.compiler.cc_env())
            .env("CFLAGS", self.compiler.cflags_env())
            .arg(msys_compatible(self.src.join("configure"))?);
        if host() != target() {
            cmd.arg("--build").arg(gnu_target(host()));
            cmd.arg("--host").arg(gnu_target(target()));
        }
        cmd.arg("--disable-dependency-tracking");
        cmd.arg("--enable-static");
        cmd.arg("--disable-shared");
        cmd.arg("--with-pic");
        cmd.arg({
            let mut s = OsString::from("--prefix=");
            s.push(msys_compatible(&self.dst)?);
            s
        });
        Ok(cmd)
    }

    pub fn make(&self) -> Command {
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

pub fn parse_libtool_file<P: AsRef<Path>>(path: P) -> Result<()> {
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
                    println!("cargo:rustc-link-lib=static={}", s);
                }
            }
        }
        if l.trim().starts_with("dependency_libs") {
            if let Some(s) = l.splitn(2, '=').nth(1) {
                parse_linker_flags(s.trim().trim_matches('\''))
            }
        }
    })
}

pub fn parse_linker_flags(flags: &str) {
    let parts = flags.split(|c: char| c.is_whitespace()).map(|p| {
        if p.starts_with("-") && (p.len() > 2) {
            p.split_at(2)
        } else {
            ("", p)
        }
    });

    for (flag, val) in parts {
        match flag {
            "-L" => {
                println!("cargo:rustc-link-search=native={}", val);
            }
            "-F" => {
                println!("cargo:rustc-link-search=framework={}", val);
            }
            "-l" | "" if !val.is_empty() => {
                if val.ends_with(".la") {
                    parse_libtool_file(val).unwrap();
                } else {
                    println!("cargo:rustc-link-lib={}", val);
                }
            }
            _ => (),
        }
    }
}
