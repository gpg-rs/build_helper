use std::env;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::result;
use std::str;

use gcc;

pub type Result<T> = result::Result<T, ()>;

pub struct Config {
    pub target: String,
    pub host: String,
    pub compiler: gcc::Tool,
    pub src: PathBuf,
    pub dst: PathBuf,
    pub build: PathBuf,
}

impl Config {
    pub fn new<S: AsRef<OsStr>>(name: S) -> Result<Config> {
        let target = env::var("TARGET").unwrap();
        let host = env::var("HOST").unwrap();
        let compiler = gcc::Build::new().get_compiler();
        let src = PathBuf::from(env::current_dir().unwrap()).join(name.as_ref());
        let dst = PathBuf::from(env::var_os("OUT_DIR").unwrap());
        let mut build = dst.join("build");
        build.push(name.as_ref());

        fs::create_dir_all(&build)
            .map_err(|e| eprintln!("unable to create build directory: {}", e))?;

        Ok(Config {
            target,
            host,
            compiler,
            src,
            dst,
            build,
        })
    }

    pub fn configure(&self) -> Result<Command> {
        let cflags = self.compiler
            .args()
            .iter()
            .fold(OsString::new(), |mut c, a| {
                c.push(a);
                c.push(" ");
                c
            });

        let mut cmd = Command::new("sh");
        cmd.current_dir(&self.build)
            .env("CC", self.compiler.path())
            .env("CFLAGS", &cflags)
            .arg(msys_compatible(self.src.join("configure"))?);
        if self.host != self.target {
            cmd.arg("--build").arg(gnu_target(&self.host));
            cmd.arg("--host").arg(gnu_target(&self.target));
        }
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

pub fn run(cmd: &mut Command) -> Result<String> {
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

pub fn output(cmd: &mut Command) -> Result<String> {
    run(cmd.stdout(Stdio::piped()))
}
