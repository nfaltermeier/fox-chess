use std::{env, fs, path::Path};

use quote::quote;

fn main() {
    build_info_build::build_script();

    let out_dir = env::var_os("OUT_DIR").unwrap();

    {
        let dest_path = Path::new(&out_dir).join("king_attack_unit_values.rs");

        let king_attack_unit_values = generate_king_attack_unit_values();

        let king_attack_unit_values_code = quote! {
            pub static KING_ATTACK_UNIT_VALUES: [i16; 100] = [#(#king_attack_unit_values),*];
        };

        let parsed_code = syn::parse_file(king_attack_unit_values_code.to_string().as_str()).unwrap();

        fs::write(&dest_path, prettyplease::unparse(&parsed_code)).unwrap();
    }

    {
        let dest_path = Path::new(&out_dir).join("ln_fixedpoint_128_values.rs");

        let ln_fixedpoint_128_values = generate_ln_fixedpoint_128_values();

        let ln_fixedpoint_128_values_code = quote! {
            pub static LN_FIXEDPOINT_128_VALUES: [i16; 64] = [#(#ln_fixedpoint_128_values),*];
        };

        let parsed_code = syn::parse_file(ln_fixedpoint_128_values_code.to_string().as_str()).unwrap();

        fs::write(&dest_path, prettyplease::unparse(&parsed_code)).unwrap();
    }
}

fn generate_king_attack_unit_values() -> Box<[i16; 100]> {
    let mut result = Box::new([0; 100]);

    for (i, v) in result.iter_mut().enumerate() {
        let power = (35.0 - i as f32) / 10.0;
        *v = ((500.0 * 1.47) / (1.0 + 4.0_f32.powf(power))).round() as i16
    }

    result
}

fn generate_ln_fixedpoint_128_values() -> Box<[i16; 64]> {
    let mut result = Box::new([0; 64]);

    for (i, v) in result.iter_mut().enumerate() {
        *v = ((i as f32).ln() * 128.0).round() as i16;
    }

    result
}
