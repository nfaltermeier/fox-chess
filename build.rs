use std::{env, fs, path::Path, process::Command};

use quote::quote;

const NEURAL_NETWORK_NAME: &str = "ruppell";

fn main() {
    build_info_build::build_script();

    let out_dir = env::var_os("OUT_DIR").unwrap();

    {
        let dest_path = Path::new(&out_dir).join("ln_fixedpoint_128_values.rs");

        let ln_fixedpoint_128_values = generate_ln_fixedpoint_128_values();

        let ln_fixedpoint_128_values_code = quote! {
            pub static LN_FIXEDPOINT_128_VALUES: [i16; 64] = [#(#ln_fixedpoint_128_values),*];
        };

        let parsed_code = syn::parse_file(ln_fixedpoint_128_values_code.to_string().as_str()).unwrap();

        fs::write(&dest_path, prettyplease::unparse(&parsed_code)).unwrap();
    }

    let directory = "networks";
    let neural_network_filepath = format!("{directory}/{NEURAL_NETWORK_NAME}.nnue");

    if !fs::exists(&neural_network_filepath).expect("Error while checking if neural network file exists") {
        if !fs::exists(directory).expect("Error while checking if 'networks' folder exists") {
            fs::create_dir(directory).expect("Failed to create 'networks' folder");
        }

        let url = format!("https://github.com/nfaltermeier/fox-chess-nets/releases/download/{NEURAL_NETWORK_NAME}/{NEURAL_NETWORK_NAME}.nnue");

        let mut cmd = Command::new("curl");
        cmd
            .arg("-fL")
            .arg(&url)
            .args(["--output", &format!("{NEURAL_NETWORK_NAME}.nnue")])
            .current_dir(directory);
        let mut download = cmd.spawn().expect("failed to start curl to download neural network");
        if !download.wait().expect("Waiting for curl command failed").success() {
            println!("cargo::error=Downloading neural network from {url} failed");
            return;
        }
    }

    println!("cargo::rustc-env=NEURAL_NETWORK={neural_network_filepath}");
    println!("cargo::rerun-if-changed={neural_network_filepath}");
}

fn generate_ln_fixedpoint_128_values() -> Box<[i16; 64]> {
    let mut result = Box::new([0; 64]);

    for (i, v) in result.iter_mut().enumerate() {
        *v = ((i as f32).ln() * 128.0).round() as i16;
    }

    result
}
