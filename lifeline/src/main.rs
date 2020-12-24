use std::time::Duration;

mod sound;

use sound::{notes::*, Song};

const SUCCESS_SONG: Song = Song(&[(Some(A_4), Duration::from_millis(100))]);

fn main() {
    std::fs::write("/sys/class/pwm/pwmchip0/export", "0").unwrap();
    let res = SUCCESS_SONG.play();
    std::fs::write("/sys/class/pwm/pwmchip0/unexport", "0").unwrap();
    res.unwrap();
}
