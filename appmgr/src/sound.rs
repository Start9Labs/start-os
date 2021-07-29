use divrem::DivRem;
use futures::stream::repeat_with;

lazy_static::lazy_static! {
    static ref SEMITONE_K: f64 = 2f64.powf(1f64 / 12f64);
}

fn export() {}
fn unexport() {}

#[derive(Clone)]
pub struct Note {
    semitone: Semitone,
    octave: u8,
}
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Semitone {
    C = 0,
    Db = 1,
    D = 2,
    Eb = 3,
    E = 4,
    F = 5,
    Gb = 6,
    G = 7,
    Ab = 8,
    A = 9,
    Bb = 10,
    B = 11,
}

impl Semitone {
    pub fn rotate(&self, n: isize) -> Semitone {
        let mut temp = (*self as isize) + n;
        while temp >= 12 {
            temp -= 12;
        }
        while temp < 0 {
            temp += 12;
        }
        match temp {
            0 => Semitone::C,
            1 => Semitone::Db,
            2 => Semitone::D,
            3 => Semitone::Eb,
            4 => Semitone::E,
            5 => Semitone::F,
            6 => Semitone::Gb,
            7 => Semitone::G,
            8 => Semitone::Ab,
            9 => Semitone::A,
            10 => Semitone::Bb,
            11 => Semitone::B,
            _ => panic!("crate::sound::Semitone::rotate: Unreachable"),
        }
    }
}

pub struct Interval(isize);
pub enum TimeSlice {
    Sixteenth,
    Eighth,
    Quarter,
    Half,
    Whole,
    Triplet(Box<TimeSlice>),
    Dot(Box<TimeSlice>),
    Tie(Box<TimeSlice>, Box<TimeSlice>),
}

fn interval(i: &Interval, note: &Note) -> Note {
    match (i, note) {
        (Interval(n), Note { semitone, octave }) => {
            use std::cmp::Ordering::*;
            let (o_t, s_t) = n.div_rem(12);
            let new_semitone = semitone.rotate(s_t);
            let new_octave = match (new_semitone.cmp(semitone), s_t.cmp(&0)) {
                (Greater, Less) => octave.clone() as isize + o_t - 1,
                (Less, Greater) => octave.clone() as isize + o_t + 1,
                _ => octave.clone() as isize + o_t,
            };
            Note {
                semitone: new_semitone,
                octave: new_octave as u8,
            }
        }
    }
}

static MINOR_THIRD: Interval = Interval(3);
static MAJOR_THIRD: Interval = Interval(4);
static FOURTH: Interval = Interval(5);
static FIFTH: Interval = Interval(7);

fn iterate<T: Clone, F: Fn(&T) -> T>(f: F, init: &T) -> impl Iterator<Item = T> {
    let mut temp = init.clone();
    let ff = move || {
        let next = f(&temp);
        let now = std::mem::replace(&mut temp, next);
        now
    };
    std::iter::repeat_with(ff)
}

fn circle_of_fifths(note: &Note) -> impl Iterator<Item = Note> {
    iterate(|n| interval(&FIFTH, n), note)
}

fn circle_of_fourths(note: &Note) -> impl Iterator<Item = Note> {
    iterate(|n| interval(&FOURTH, n), note)
}
