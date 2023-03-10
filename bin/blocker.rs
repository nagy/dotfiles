//! ```cargo
//! [dependencies]
//! aes = "0.8.2"
//! ctr = "0.9.2"
//! data-encoding = "2.3.3"
//! fastcdc = "3.0.1"
//! hex-literal = "0.3.4"
//! zstd = "0.12.3"
//! sha2 = "0.10.6"
//! ```

use std::fs::{create_dir_all, read, read_dir, File};
use std::io::prelude::*;
use std::io::{copy, stdin, stdout, Result};

#[derive(Default)]
struct Hash {
    some: String,
    data: Vec<u8>,
}

impl Hash {
    fn new(arg: &str) -> Self {
        Self {
            some: arg.into(),
            ..Default::default()
        }
    }
    fn from_binary(bin: &[u8]) -> Self {
        assert_eq!(bin.len(), 32);
        Self {
            some: data_encoding::BASE32
                .encode(bin)
                .to_lowercase()
                .replace("====", ""),
            ..Default::default()
        }
    }
    fn from_data(data: Vec<u8>) -> Self {
        use sha2::Digest;
        let hash = &sha2::Sha256::new_with_prefix(&data).finalize()[..];
        Self {
            some: data_encoding::BASE32
                .encode(hash)
                .to_lowercase()
                .replace("====", ""),
            data: data,
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

    fn force_filename(&self) -> String {
        format!("{}/{}", self.dirname(), &self.some)
    }

    fn try_filename(&self) -> String {
        let paths = read_dir(self.dirname()).unwrap();
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
    fn read(&self) -> Result<Vec<u8>> {
        let mut content = read(self.try_filename())?;
        // TODO check hash
        mask(&mut content);
        zstd::stream::decode_all(&content[..])
    }
    fn calc_hash(&self) -> Vec<u8> {
        use sha2::Digest;
        let data = &self.data[..];
        let mut hasher = sha2::Sha256::new();
        hasher.update(data);
        hasher.finalize()[..].into()
    }
    fn write(&self) -> Result<String> {
        let cohash: Self = (&self.calc_hash()[..]).into();
        let cohash_filename = cohash.force_filename();
        if std::path::Path::new(&cohash_filename).is_file() {
            Ok(cohash.some[..32].into())
        } else {
            create_dir_all(cohash.dirname())?;
            let mut file = File::options()
                .read(false)
                .write(true)
                .create_new(true)
                .open(cohash_filename)?;
            self.write_to(&mut file)
        }
    }
    fn write_to<W: Write>(&self, mut writer: W) -> Result<String> {
        let cohash: Self = (&self.calc_hash()[..]).into();
        let mut compr = zstd::stream::encode_all(&*self.data, 9).unwrap();
        mask(&mut compr);
        writer.write_all(&compr[..])?;
        Ok(cohash.some[..32].into())
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

impl From<fastcdc::v2020::ChunkData> for Hash {
    fn from(source: fastcdc::v2020::ChunkData) -> Self {
        Self::from_data(source.data)
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
            copy(&mut &innercontent[..], &mut stdout()).unwrap();
        }
    } else {
        // put mode
        let mut combined_hash = Vec::new();
        for ch in
            fastcdc::v2020::StreamCDC::new(Box::new(stdin()), 512 << 10, 1024 << 10, 2048 << 10)
        {
            let h: Hash = ch.unwrap().into();
            h.write().unwrap();
            combined_hash.extend_from_slice(&h.calc_hash()[..]);
        }
        let output = Hash::from_data(combined_hash).write().unwrap();
        println!("{output}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_str() {
        let h = Hash::new("yu2z5ogmhbfomjeyrhfxwwlzganb5cso");
        assert_eq!(h.first(), "yu");
        assert_eq!(h.second(), "2z");
        assert_eq!(h.dirname(), "yu/2z");
    }

    #[test]
    fn test_bytes() {
        let h = Hash::from_binary(&hex_literal::hex!(
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        ));
        assert_eq!(h.first(), "4o");
        assert_eq!(h.second(), "ym");
    }

    #[test]
    fn test_write() {
        let h: Vec<u8> = hex_literal::hex!("01020304050607080910").into();
        let chs: Vec<Hash> = fastcdc::v2020::StreamCDC::new(
            Box::new(Cursor::new(h)),
            512 << 10,
            1024 << 10,
            2048 << 10,
        )
        .map(|x| x.unwrap().into())
        .collect();
        assert_eq!(chs[0].data.len(), 10);
        assert_eq!(chs[0].data[0], 1);
        assert_eq!(chs[0].data[1], 2);

        let first = &chs[0];
        let mut vec = vec![];
        first.write_to(&mut vec).unwrap();
        assert_eq!(vec.len(), 19);
        assert_eq!(
            first.force_filename(),
            "yu/2z/yu2z5ogmhbfomjeyrhfxwwlzganb5csowpzqwsjfqh6p7wwx3lrq"
        );
    }
}
