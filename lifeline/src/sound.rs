#![allow(dead_code)]

use std::io::Error;
use std::time::Duration;

pub mod notes {
    pub const A_4: f64 = 440.0;
    pub const B_4: f64 = 493.88;
    pub const C_5: f64 = 523.25;
    pub const D_5: f64 = 587.33;
    pub const E_5: f64 = 659.25;
    pub const F_5: f64 = 698.46;
    pub const G_5: f64 = 783.99;
    pub const A_5: f64 = 880.00;
    pub const B_5: f64 = 987.77;
    pub const E_6: f64 = 1318.51;
}

pub fn freq_to_period(freq: f64) -> Duration {
    Duration::from_secs(1).div_f64(freq)
}

pub fn play(freq: f64) -> Result<(), Error> {
    // set freq
    let period = freq_to_period(freq);
    let period_bytes = std::fs::read("/sys/class/pwm/pwmchip0/pwm0/period")?;
    if period_bytes == b"0\n" {
        std::fs::write("/sys/class/pwm/pwmchip0/pwm0/period", format!("{}", 1000))?;
    }
    std::fs::write("/sys/class/pwm/pwmchip0/pwm0/duty_cycle", "0")?;
    std::fs::write("/sys/class/pwm/pwmchip0/pwm0/period", format!("{}", period.as_nanos()))?;
    std::fs::write(
        "/sys/class/pwm/pwmchip0/pwm0/duty_cycle",
        format!("{}", (period / 2).as_nanos()),
    )?;
    // enable the thing
    std::fs::write("/sys/class/pwm/pwmchip0/pwm0/enable", "1")?;
    Ok(())
}

pub fn stop() -> Result<(), Error> {
    // disable the thing
    std::fs::write("/sys/class/pwm/pwmchip0/pwm0/enable", "0")?;
    // sleep small amount
    std::thread::sleep(Duration::from_micros(30));
    Ok(())
}

pub fn play_for_duration(freq: f64, duration: Duration) -> Result<(), Error> {
    play(freq)?;
    std::thread::sleep(duration);
    stop()
}

#[derive(Clone, Debug)]
pub struct Song<'a>(pub &'a [(Option<f64>, Duration)]);
impl<'a> Song<'a> {
    pub fn play(&self) -> Result<(), Error> {
        for (note, duration) in self.0 {
            if let Some(note) = note {
                play_for_duration(*note, *duration)?;
            } else {
                std::thread::sleep(*duration);
            }
        }
        Ok(())
    }
}

impl Song<'static> {
    pub fn play_while<T, F: FnOnce() -> T>(&'static self, f: F) -> T {
        let run = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));
        let t_run = run.clone();
        let handle = std::thread::spawn(move || -> Result<(), Error> {
            while t_run.load(std::sync::atomic::Ordering::SeqCst) {
                self.play()?;
            }
            Ok(())
        });
        let res = f();
        run.store(false, std::sync::atomic::Ordering::SeqCst);
        let e = handle.join().unwrap().err();
        if let Some(e) = e {
            eprintln!("ERROR PLAYING SOUND: {}\n{:?}", e, e);
        }
        res
    }
}
impl<'a> From<&'a [(Option<f64>, Duration)]> for Song<'a> {
    fn from(t: &'a [(Option<f64>, Duration)]) -> Self {
        Song(t)
    }
}
