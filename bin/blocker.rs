//! ```cargo
//! [dependencies]
//! aes = "0.8.2"
//! ctr = "0.9.2"
//! data-encoding = "2.3.3"
//! fastcdc = "3.0.1"
//! hex-literal = "0.3.4"
//! zstd = "0.12.3"
//! ```

struct Hash {
    some: String,
}

impl Hash {
    fn new(arg: &str) -> Self {
        Self { some: arg.into() }
    }
    fn from_binary(bin: &[u8]) -> Self {
        Self {
            some: data_encoding::BASE32
                .encode(bin)
                .to_lowercase()
                .replace("====", ""),
        }
    }
    fn first(&self) -> String {
        format!(
            "{}{}",
            self.some.chars().nth(0).unwrap(),
            self.some.chars().nth(1).unwrap()
        )
    }
    fn second(&self) -> String {
        format!(
            "{}{}",
            self.some.chars().nth(2).unwrap(),
            self.some.chars().nth(3).unwrap()
        )
    }
    fn dirname(&self) -> String {
        format!("{}/{}", self.first(), self.second())
    }

    fn try_filename(&self) -> String {
        let paths = std::fs::read_dir(self.dirname()).unwrap();
        let addr = paths
            .filter(|p| {
                p.as_ref()
                    .unwrap()
                    .file_name()
                    .into_string()
                    .unwrap()
                    .starts_with(&self.some)
            })
            .next()
            .unwrap()
            .unwrap()
            .file_name()
            .into_string()
            .unwrap();
        format!("{}/{}/{}", self.first(), self.second(), addr)
    }
    fn read(&self) -> std::io::Result<Vec<u8>> {
        let mut content = std::fs::read(self.try_filename())?;
        // TODO check hash
        mask(&mut content);
        zstd::stream::decode_all(&content[..])
    }
}

impl From<&str> for Hash {
    fn from(source: &str) -> Self {
        Self::new(source)
    }
}

impl From<&[u8]> for Hash {
    fn from(source: &[u8]) -> Self {
        Self::from_binary(source)
    }
}

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

// fn dofile<T: std::io::Read + 'static>(input: T) -> impl Iterator<Item = std::io::Result<usize>> {
//     fastcdc::v2020::StreamCDC::new(Box::new(input), 512 << 10, 1024 << 10, 2048 << 10)
//         .map(|x| -> std::io::Result<usize> { Ok((x?).data.len()) })
// }

fn main() {
    let args: Vec<String> = std::env::args().collect();
    assert!(args.len() <= 2);
    if args.len() == 2 {
        // cat mode
        let h: Hash = args[1].as_str().into();
        let content = h.read().unwrap();
        for ch in content.chunks_exact(32) {
            let chh: Hash = ch.into();
            let innercontent = chh.read().unwrap();
            std::io::copy(&mut &innercontent[..], &mut std::io::stdout()).unwrap();
        }
    } else {
        // put mode
        todo!()
    }
}
