//! ```cargo
//! [dependencies]
//! aes = "0.8.2"
//! ctr = "0.9.2"
//! data-encoding = "2.3.3"
//! fastcdc = "3.0.1"
//! hex-literal = "0.3.4"
//! zstd = "0.12.3"
//! ```

fn mask(plaintext: &mut [u8]) {
    use aes::cipher::{KeyIvInit, StreamCipher};
    use hex_literal::hex;

    // from:  openssl enc -e -aes-256-ctr -nopad -nosalt -k "" -P
    let key: [u8; 32] = hex!("E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855");
    let iv: [u8; 16] = hex!("5DF6E0E2761359D30A8275058E299FCC");

    // encrypt in-place
    let mut cipher = ctr::Ctr128BE::<aes::Aes256>::new(&key.into(), &iv.into());
    cipher.apply_keystream(plaintext);
}

fn dofile<T: std::io::Read + 'static>(input: T) -> impl Iterator<Item = std::io::Result<usize>> {
    fastcdc::v2020::StreamCDC::new(Box::new(input), 512 << 10, 1024 << 10, 2048 << 10)
        .map(|x| -> std::io::Result<usize> { Ok((x?).data.len()) })
}

fn read_onefile(addr: &str) -> std::io::Result<Vec<u8>> {
    let first = format!(
        "{}{}",
        addr.chars().nth(0).unwrap(),
        addr.chars().nth(1).unwrap()
    );
    let second = format!(
        "{}{}",
        addr.chars().nth(2).unwrap(),
        addr.chars().nth(3).unwrap()
    );
    let dirname = format!("{}/{}", first, second,);
    let paths = std::fs::read_dir(dirname)?;
    let addr = paths
        .filter(|p| {
            p.as_ref()
                .unwrap()
                .file_name()
                .into_string()
                .unwrap()
                .starts_with(addr)
        })
        .next()
        .unwrap()
        .unwrap()
        .file_name()
        .into_string()
        .unwrap();
    let filename = format!("{}/{}/{}", first, second, addr);
    let mut ret = std::fs::read(filename)?;
    mask(&mut ret);
    zstd::stream::decode_all(&ret[..])
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    assert!(args.len() <= 2);
    if args.len() == 2 {
        // cat mode
        let content = read_onefile(&args[1]).unwrap();
        for ch in content.chunks_exact(32) {
            let newname = data_encoding::BASE32
                .encode(ch)
                .to_lowercase()
                .replace("====", "");
            let innercontent = read_onefile(&newname).unwrap();
            // TODO check hash
            std::io::copy(&mut &innercontent[..], &mut std::io::stdout()).unwrap();
        }
    } else {
        // put mode
        todo!()
    }
}
