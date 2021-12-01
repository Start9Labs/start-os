use std::fs::read_dir;
use std::fs::read_to_string;
use std::fs::write;
use std::process::Command;
use std::thread::sleep;
use std::time::Duration;

const WHITELIST: [(&'static str, &'static str); 3] =
    [("1d6b", "0002"), ("1d6b", "0003"), ("2109", "3431")];

fn main() {
    let mut quirks = vec![("152d".to_owned(), "0562".to_owned())];
    loop {
        for dir in read_dir("/sys/bus/usb/devices/").unwrap() {
            let dir = dir.unwrap();
            if !dir.path().join("idVendor").exists() {
                continue;
            }
            let vendor = read_to_string(dir.path().join("idVendor")).unwrap();
            let product = read_to_string(dir.path().join("idProduct")).unwrap();
            if WHITELIST.contains(&(&vendor, &product)) {
                continue;
            }
            let id = (vendor, product);
            if quirks.contains(&id) {
                continue;
            }
            quirks.push(id);
            let quirk_string =
                quirks
                    .iter()
                    .fold("quirks=".to_owned(), |mut acc, (vendor, product)| {
                        if acc.is_empty() {
                            acc.push_str(",")
                        };
                        acc.push_str(vendor);
                        acc.push_str(":");
                        acc.push_str(product);
                        acc.push_str(":u");
                        acc
                    });
            let modprobe_out = Command::new("modprobe")
                .arg("usb_storage")
                .arg(&quirk_string)
                .output()
                .unwrap();
            if !modprobe_out.status.success() {
                panic!("{}", String::from_utf8(modprobe_out.stdout).unwrap());
            }
            write(dir.path().join("authorized"), "0").unwrap();
            write(dir.path().join("authorized"), "1").unwrap();
        }
        sleep(Duration::from_secs(1));
    }
}
