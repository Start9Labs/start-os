use std::path::Path;
use std::time::Duration;

use clap::{value_parser, Arg, ArgMatches, Command};

const PWM_DIR: &str = "/sys/class/pwm/pwmchip0/pwm0";
const EXPORT_FILE: &str = "/sys/class/pwm/pwmchip0/export";
const PERIOD_FILE: &str = "/sys/class/pwm/pwmchip0/pwm0/period";
const DUTY_FILE: &str = "/sys/class/pwm/pwmchip0/pwm0/duty_cycle";
const SWITCH_FILE: &str = "/sys/class/pwm/pwmchip0/pwm0/enable";

fn main() {
    let arg0 = std::env::args().next().unwrap().to_owned();
    let cmd = Command::new("pi-beep")
        .args_override_self(true)
        .arg(
            Arg::new("freq")
                .short('f')
                .value_name("FREQ_Hz")
                .value_parser(value_parser!(f64))
                .default_value("440")
                .help("frequency of the tone in Hertz (Hz)"),
        )
        .arg(
            Arg::new("length")
                .short('l')
                .value_name("LENGTH_ms")
                .value_parser(value_parser!(u64))
                .default_value("200")
                .help("length of the tone in milliseconds (ms)"),
        )
        .arg(
            Arg::new("delay")
                .short('d')
                .value_name("DELAY_ms")
                .value_parser(value_parser!(u64))
                .default_value("100")
                .help(concat!(
                    "delay between repetitions of the tone\n",
                    "*without* delay after last repetition of the tone"
                )),
        )
        .arg(
            Arg::new("Delay")
                .short('D')
                .value_name("DELAY_ms")
                .value_parser(value_parser!(u64))
                .help(concat!(
                    "delay between repetitions of the tone\n",
                    "*with* delay after last repetition of the tone"
                )),
        )
        .arg(
            Arg::new("reps")
                .short('r')
                .value_name("REPS")
                .value_parser(value_parser!(usize))
                .default_value("1")
                .help("number of repetitions of the last tone"),
        )
        .arg(
            Arg::new("new")
                .short('n')
                .long("new")
                .raw(true)
                .num_args(0..)
                .last(false),
        );
    fn rec(arg0: &String, cmd: Command, matches: ArgMatches) {
        run_beep(
            *matches.get_one("freq").unwrap(),
            Duration::from_millis(*matches.get_one("length").unwrap()),
            Duration::from_millis(
                *matches
                    .get_one("Delay")
                    .unwrap_or(matches.get_one("delay").unwrap()),
            ),
            *matches.get_one("reps").unwrap(),
            matches.contains_id("Delay"),
        );
        if let Some(rest) = matches.get_many::<String>("new") {
            rec(
                arg0,
                cmd.clone(),
                cmd.get_matches_from(std::iter::once(arg0).chain(rest)),
            )
        }
    }
    let matches = cmd.clone().get_matches();
    if !Path::new(PWM_DIR).exists() {
        std::fs::write(EXPORT_FILE, b"0").unwrap();
    }
    rec(&arg0, cmd, matches)
}

fn run_beep(freq: f64, len: Duration, delay: Duration, reps: usize, end_delay: bool) {
    for rep in 1..=reps {
        beep(freq, len);
        if rep < reps || end_delay {
            std::thread::sleep(delay);
        }
    }
}

fn beep(freq: f64, len: Duration) {
    let curr_period = std::fs::read_to_string(&*PERIOD_FILE).unwrap();
    if curr_period == "0\n" {
        std::fs::write(&*PERIOD_FILE, "1000").unwrap();
    }
    let new_period = ((1.0 / freq) * 1_000_000_000.0).round() as u64;
    std::fs::write(DUTY_FILE, "0").unwrap();
    std::fs::write(PERIOD_FILE, format!("{}", new_period)).unwrap();
    std::fs::write(DUTY_FILE, format!("{}", new_period / 2)).unwrap();
    std::fs::write(SWITCH_FILE, "1").unwrap();
    std::thread::sleep(len);
    std::fs::write(SWITCH_FILE, "0").unwrap();
}
