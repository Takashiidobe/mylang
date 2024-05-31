use std::process::Stdio;

use assert_cmd::cargo::CommandCargoExt as _;
use insta::glob;
use insta_cmd::{assert_cmd_snapshot, Command};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct TestOutput {
    status: i32,
    stdout: Vec<String>,
    stderr: Vec<String>,
}

// #[test]
// fn gen_asm() {
//     glob!("input", "**/*.my", |path| {
//         let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
//
//         let Output {
//             status,
//             stdout,
//             stderr,
//         } = cmd.arg(path).output().unwrap();
//
//         let test_output = TestOutput {
//             status: status.code().unwrap(),
//             stdout: String::from_utf8_lossy(&stdout)
//                 .to_string()
//                 .lines()
//                 .map(|x| x.to_owned())
//                 .collect(),
//             stderr: String::from_utf8_lossy(&stderr)
//                 .to_string()
//                 .lines()
//                 .map(|x| x.to_owned())
//                 .collect(),
//         };
//
//         assert_yaml_snapshot!(test_output);
//     });
// }

#[test]
fn gen_bin() {
    glob!("input", "**/*.my", |path| {
        let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();

        let ps_child = cmd
            .arg(path)
            .stdout(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();

        let out_name = format!("{}.out", path.display());

        Command::new("gcc")
            .arg("-o")
            .arg(&out_name)
            .arg("-xassembler")
            .arg("-")
            .stdin(Stdio::from(ps_child.stdout.unwrap()))
            .spawn()
            .unwrap();

        assert_cmd_snapshot!(Command::new(&out_name));
    });
}
