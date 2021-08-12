use crate::{Error, ErrorKind, ResultExt};
use divrem::DivRem;
use proptest_derive::Arbitrary;
use std::{cmp::Ordering, path::Path, time::Duration};
use tokio::sync::{Mutex, MutexGuard};

lazy_static::lazy_static! {
    static ref SEMITONE_K: f64 = 2f64.powf(1f64 / 12f64);
    static ref A_4: f64 = 440f64;
    static ref C_0: f64 = *A_4 / SEMITONE_K.powf(9f64) / 2f64.powf(4f64);
    static ref EXPORT_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/pwm0/export");
    static ref UNEXPORT_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/pwm0/unexport");
    static ref PERIOD_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/pwm0/period");
    static ref DUTY_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/pwm0/duty_cycle");
    static ref SWITCH_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/pwm0/enable");
    static ref SOUND_MUTEX: Mutex<Option<fd_lock_rs::FdLock<tokio::fs::File>>> = Mutex::new(None);
}

pub const SOUND_LOCK_FILE: &'static str = "/TODO/AIDEN/CHANGEME";

struct SoundInterface(Option<MutexGuard<'static, Option<fd_lock_rs::FdLock<tokio::fs::File>>>>);
impl SoundInterface {
    pub async fn lease() -> Result<Self, Error> {
        tokio::fs::write(&*EXPORT_FILE, "0")
            .await
            .map_err(|e| Error {
                source: e.into(),
                kind: ErrorKind::SoundError,
                revision: None,
            })?;
        let mut guard = SOUND_MUTEX.lock().await;
        let sound_file = tokio::fs::File::create(SOUND_LOCK_FILE).await?;
        *guard = Some(
            tokio::task::spawn_blocking(move || {
                fd_lock_rs::FdLock::lock(sound_file, fd_lock_rs::LockType::Exclusive, true)
            })
            .await
            .map_err(|e| {
                Error::new(
                    anyhow::anyhow!("Sound file lock panicked: {}", e),
                    ErrorKind::SoundError,
                )
            })?
            .with_kind(ErrorKind::SoundError)?,
        );
        Ok(SoundInterface(Some(guard)))
    }
    pub async fn play(&mut self, note: &Note) -> Result<(), Error> {
        {
            let curr_period = tokio::fs::read_to_string(&*PERIOD_FILE).await?;
            if curr_period == "0\n" {
                tokio::fs::write(&*PERIOD_FILE, "1000").await?;
            }
            let new_period = ((1.0 / note.frequency()) * 1_000_000_000.0).round() as u64;
            tokio::fs::write(&*DUTY_FILE, "0").await?;
            tokio::fs::write(&*PERIOD_FILE, format!("{}", new_period)).await?;
            tokio::fs::write(&*DUTY_FILE, format!("{}", new_period / 2)).await?;
            tokio::fs::write(&*SWITCH_FILE, "1").await
        }
        .map_err(|e| Error {
            source: e.into(),
            kind: ErrorKind::SoundError,
            revision: None,
        })
    }
    pub async fn play_for_time_slice(
        &mut self,
        tempo_qpm: u16,
        note: &Note,
        time_slice: &TimeSlice,
    ) -> Result<(), Error> {
        {
            self.play(note).await?;
            tokio::time::sleep(time_slice.to_duration((tempo_qpm as u64 * 19 / 20) as u16)).await;
            self.stop().await?;
            tokio::time::sleep(time_slice.to_duration(tempo_qpm / 20)).await;
            Ok(())
        }
        .or_else(|e: Error| {
            // we could catch this error and propagate but I'd much prefer the original error bubble up
            let _mute = self.stop();
            Err(e)
        })
    }
    pub async fn stop(&mut self) -> Result<(), Error> {
        tokio::fs::write(&*SWITCH_FILE, "0")
            .await
            .map_err(|e| Error {
                source: e.into(),
                kind: ErrorKind::SoundError,
                revision: None,
            })
    }
}

pub struct Song<Notes> {
    tempo_qpm: u16,
    note_sequence: Notes,
}
impl<'a, T: 'a> Song<T>
where
    &'a T: IntoIterator<Item = &'a (Option<Note>, TimeSlice)>,
{
    pub async fn play(&'a self) -> Result<(), Error> {
        let mut sound = SoundInterface::lease().await?;
        for (note, slice) in &self.note_sequence {
            match note {
                None => tokio::time::sleep(slice.to_duration(self.tempo_qpm)).await,
                Some(n) => sound.play_for_time_slice(self.tempo_qpm, n, slice).await?,
            };
        }
        Ok(())
    }
}

impl Drop for SoundInterface {
    fn drop(&mut self) {
        let guard = self.0.take();
        tokio::spawn(async move {
            if let Err(e) = tokio::fs::write(&*UNEXPORT_FILE, "0").await {
                log::error!("Failed to Unexport Sound Interface: {}", e)
            }
            if let Some(mut guard) = guard {
                if let Some(lock) = guard.take() {
                    if let Err(e) = tokio::task::spawn_blocking(|| lock.unlock(true))
                        .await
                        .unwrap()
                    {
                        log::error!("Failed to drop Sound Interface File Lock: {}", e.1)
                    }
                }
            }
        });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Arbitrary)]
pub struct Note {
    semitone: Semitone,
    octave: i8,
}
impl Note {
    pub fn frequency(&self) -> f64 {
        SEMITONE_K.powf((self.semitone as isize) as f64) * (*C_0) * (2f64.powf(self.octave as f64))
    }
}
impl PartialOrd for Note {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Note {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.octave == other.octave {
            self.semitone.cmp(&other.semitone)
        } else {
            self.octave.cmp(&other.octave)
        }
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Arbitrary)]
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

        match temp.rem_euclid(12) {
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Arbitrary)]
pub struct Interval(isize);

#[derive(Clone, Copy)]
pub enum TimeSlice {
    Sixteenth,
    Eighth,
    Quarter,
    Half,
    Whole,
    Triplet(&'static TimeSlice),
    Dot(&'static TimeSlice),
    Tie(&'static TimeSlice, &'static TimeSlice),
}
impl TimeSlice {
    pub fn to_duration(&self, tempo_qpm: u16) -> Duration {
        let micros_per_quarter = (tempo_qpm as f64) * 1_000_000f64;
        match &self {
            &Self::Sixteenth => Duration::from_micros((micros_per_quarter / 4.0) as u64),
            &Self::Eighth => Duration::from_micros((micros_per_quarter / 2.0) as u64),
            &Self::Quarter => Duration::from_micros(micros_per_quarter as u64),
            &Self::Half => Duration::from_micros((micros_per_quarter * 2.0) as u64),
            &Self::Whole => Duration::from_micros((micros_per_quarter * 4.0) as u64),
            &Self::Triplet(ts) => ts.to_duration(tempo_qpm) * 2 / 3,
            &Self::Dot(ts) => ts.to_duration(tempo_qpm) * 3 / 2,
            &Self::Tie(ts0, ts1) => ts0.to_duration(tempo_qpm) + ts1.to_duration(tempo_qpm),
        }
    }
}

pub fn interval(i: &Interval, note: &Note) -> Note {
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
                octave: new_octave as i8,
            }
        }
    }
}

pub const MINOR_THIRD: Interval = Interval(3);
pub const MAJOR_THIRD: Interval = Interval(4);
pub const FOURTH: Interval = Interval(5);
pub const FIFTH: Interval = Interval(7);

fn iterate<T: Clone, F: Fn(&T) -> T>(f: F, init: &T) -> impl Iterator<Item = T> {
    let mut temp = init.clone();
    let ff = move || {
        let next = f(&temp);
        let now = std::mem::replace(&mut temp, next);
        now
    };
    std::iter::repeat_with(ff)
}

pub fn circle_of_fifths(note: &Note) -> impl Iterator<Item = Note> {
    iterate(|n| interval(&FIFTH, n), note)
}

pub fn circle_of_fourths(note: &Note) -> impl Iterator<Item = Note> {
    iterate(|n| interval(&FOURTH, n), note)
}

pub struct CircleOf<'a> {
    current: Note,
    duration: TimeSlice,
    interval: &'a Interval,
}
impl<'a> CircleOf<'a> {
    pub const fn new(interval: &'a Interval, start: Note, duration: TimeSlice) -> Self {
        CircleOf {
            current: start,
            duration,
            interval,
        }
    }
}
impl<'a> Iterator for CircleOf<'a> {
    type Item = (Option<Note>, TimeSlice);
    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current;
        let prev = std::mem::replace(&mut self.current, interval(&self.interval, &current));
        Some((Some(prev), self.duration.clone()))
    }
}

macro_rules! song {
    ($tempo:expr, [$($note:expr;)*]) => {
        {
            const fn note(semi: Semitone, octave: i8, duration: TimeSlice) -> (Option<Note>, TimeSlice) {
                (
                    Some(Note {
                        semitone: semi,
                        octave,
                    }),
                    duration,
                )
            }
            const fn rest(duration: TimeSlice) -> (Option<Note>, TimeSlice) {
                (None, duration)
            }

            use crate::sound::Semitone::*;
            use crate::sound::TimeSlice::*;
            Song {
                tempo_qpm: $tempo as u16,
                note_sequence: [
                    $(
                        $note,
                    )*
                ]
            }
        }
    };
}

pub const MARIO_DEATH: Song<[(Option<Note>, TimeSlice); 12]> = song!(400, [
    note(B, 4, Quarter);
    note(F, 5, Quarter);
    rest(Quarter);
    note(F, 5, Quarter);
    note(F, 5, Triplet(&Half));
    note(E, 5, Triplet(&Half));
    note(D, 5, Triplet(&Half));
    note(C, 5, Quarter);
    note(E, 5, Quarter);
    rest(Quarter);
    note(E, 5, Quarter);
    note(C, 4, Half);
]);

pub const MARIO_POWER_UP: Song<[(Option<Note>, TimeSlice); 15]> = song!(400, [
    note(G,4,Triplet(&Eighth));
    note(B,4,Triplet(&Eighth));
    note(D,5,Triplet(&Eighth));
    note(G,5,Triplet(&Eighth));
    note(B,5,Triplet(&Eighth));
    note(Ab,4,Triplet(&Eighth));
    note(C,5,Triplet(&Eighth));
    note(Eb,5,Triplet(&Eighth));
    note(Ab,5,Triplet(&Eighth));
    note(C,5,Triplet(&Eighth));
    note(Bb,4,Triplet(&Eighth));
    note(D,5,Triplet(&Eighth));
    note(F,5,Triplet(&Eighth));
    note(Bb,5,Triplet(&Eighth));
    note(D,6,Triplet(&Eighth));
]);

pub const MARIO_COIN: Song<[(Option<Note>, TimeSlice); 2]> = song!(400, [
    note(B, 5, Eighth);
    note(E, 6, Tie(&Dot(&Quarter), &Half));
]);

pub const BEETHOVEN: Song<[(Option<Note>, TimeSlice); 9]> = song!(216, [
    note(E, 5, Eighth);
    note(E, 5, Eighth);
    note(E, 5, Eighth);
    note(C, 5, Half);
    rest(Half);
    note(D, 5, Eighth);
    note(D, 5, Eighth);
    note(D, 5, Eighth);
    note(B, 4, Half);
]);

lazy_static::lazy_static! {
    pub static ref CIRCLE_OF_5THS_SHORT: Song<std::iter::Take<CircleOf<'static>>> = Song {
        tempo_qpm: 300,
        note_sequence: CircleOf::new(
            &FIFTH,
            Note {
                semitone: Semitone::A,
                octave: 3,
            },
            TimeSlice::Triplet(&TimeSlice::Eighth),
        )
        .take(6),
    };
    pub static ref CIRCLE_OF_4THS_SHORT: Song<std::iter::Take<CircleOf<'static>>> = Song {
        tempo_qpm: 300,
        note_sequence: CircleOf::new(
            &FOURTH,
            Note {
                semitone: Semitone::C,
                octave: 4,
            },
            TimeSlice::Triplet(&TimeSlice::Eighth)
        ).take(6)
    };
}

proptest::prop_compose! {
    fn arb_interval() (i in -88isize..88isize) -> Interval {
        Interval(i)
    }
}
proptest::prop_compose! {
    fn arb_note() (o in 0..8i8, s: Semitone) -> Note {
        Note {
            semitone: s,
            octave: o,
        }
    }
}

proptest::proptest! {
    #[test]
    fn positive_interval_greater(a in arb_note(), i in arb_interval()) {
        proptest::prop_assume!(i > Interval(0));
        proptest::prop_assert!(interval(&i, &a) > a)
    }

    #[test]
    fn negative_interval_less(a in arb_note(), i in arb_interval()) {
        proptest::prop_assume!(i < Interval(0));
        proptest::prop_assert!(interval(&i, &a) < a)
    }

    #[test]
    fn zero_interval_equal(a in arb_note()) {
        proptest::prop_assert!(interval(&Interval(0), &a) == a)
    }

    #[test]
    fn positive_negative_cancellation(a in arb_note(), i in arb_interval()) {
        let neg_i = match i {
            Interval(n) => Interval(0-n)
        };
        proptest::prop_assert_eq!(interval(&neg_i, &interval(&i, &a)), a)
    }

    #[test]
    fn freq_conversion_preserves_ordering(a in arb_note(), b in arb_note()) {
        proptest::prop_assert_eq!(Some(a.cmp(&b)), a.frequency().partial_cmp(&b.frequency()))
    }

}
