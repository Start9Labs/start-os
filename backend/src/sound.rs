use std::cmp::Ordering;
use std::path::Path;
use std::time::{Duration, Instant};

use divrem::DivRem;
use proptest_derive::Arbitrary;
use tracing::instrument;

use crate::util::FileLock;
use crate::{Error, ErrorKind, ResultExt};

lazy_static::lazy_static! {
    static ref SEMITONE_K: f64 = 2f64.powf(1f64 / 12f64);
    static ref A_4: f64 = 440f64;
    static ref C_0: f64 = *A_4 / SEMITONE_K.powf(9f64) / 2f64.powf(4f64);
    static ref EXPORT_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/export");
    static ref UNEXPORT_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/unexport");
    static ref PERIOD_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/pwm0/period");
    static ref DUTY_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/pwm0/duty_cycle");
    static ref SWITCH_FILE: &'static Path = Path::new("/sys/class/pwm/pwmchip0/pwm0/enable");
}

pub const SOUND_LOCK_FILE: &'static str = "/etc/embassy/sound.lock";

struct SoundInterface(Option<FileLock>);
impl SoundInterface {
    #[instrument]
    pub async fn lease() -> Result<Self, Error> {
        let guard = FileLock::new(SOUND_LOCK_FILE, true).await?;
        tokio::fs::write(&*EXPORT_FILE, "0")
            .await
            .or_else(|e| {
                if e.raw_os_error() == Some(16) {
                    Ok(())
                } else {
                    Err(e)
                }
            })
            .with_ctx(|_| (ErrorKind::SoundError, EXPORT_FILE.to_string_lossy()))?;
        let instant = Instant::now();
        while tokio::fs::metadata(&*PERIOD_FILE).await.is_err()
            && instant.elapsed() < Duration::from_secs(1)
        {
            tokio::time::sleep(Duration::from_millis(1)).await;
        }
        Ok(SoundInterface(Some(guard)))
    }
    #[instrument(skip(self))]
    pub async fn play(&mut self, note: &Note) -> Result<(), Error> {
        let curr_period = tokio::fs::read_to_string(&*PERIOD_FILE)
            .await
            .with_ctx(|_| (ErrorKind::SoundError, PERIOD_FILE.to_string_lossy()))?;
        if curr_period == "0\n" {
            tokio::fs::write(&*PERIOD_FILE, "1000")
                .await
                .with_ctx(|_| (ErrorKind::SoundError, PERIOD_FILE.to_string_lossy()))?;
        }
        let new_period = ((1.0 / note.frequency()) * 1_000_000_000.0).round() as u64;
        tokio::fs::write(&*DUTY_FILE, "0")
            .await
            .with_ctx(|_| (ErrorKind::SoundError, DUTY_FILE.to_string_lossy()))?;
        tokio::fs::write(&*PERIOD_FILE, format!("{}", new_period))
            .await
            .with_ctx(|_| (ErrorKind::SoundError, PERIOD_FILE.to_string_lossy()))?;
        tokio::fs::write(&*DUTY_FILE, format!("{}", new_period / 2))
            .await
            .with_ctx(|_| (ErrorKind::SoundError, DUTY_FILE.to_string_lossy()))?;
        tokio::fs::write(&*SWITCH_FILE, "1")
            .await
            .with_ctx(|_| (ErrorKind::SoundError, SWITCH_FILE.to_string_lossy()))?;
        Ok(())
    }
    #[instrument(skip(self))]
    pub async fn play_for_time_slice(
        &mut self,
        tempo_qpm: u16,
        note: &Note,
        time_slice: &TimeSlice,
    ) -> Result<(), Error> {
        if let Err(e) = async {
            self.play(note).await?;
            tokio::time::sleep(time_slice.to_duration(tempo_qpm) * 19 / 20).await;
            self.stop().await?;
            tokio::time::sleep(time_slice.to_duration(tempo_qpm) / 20).await;
            Ok::<_, Error>(())
        }
        .await
        {
            // we could catch this error and propagate but I'd much prefer the original error bubble up
            let _mute = self.stop().await;
            Err(e)
        } else {
            Ok(())
        }
    }
    #[instrument(skip(self))]
    pub async fn stop(&mut self) -> Result<(), Error> {
        tokio::fs::write(&*SWITCH_FILE, "0")
            .await
            .with_ctx(|_| (ErrorKind::SoundError, SWITCH_FILE.to_string_lossy()))
    }
    #[instrument(skip(self))]
    pub async fn close(mut self) -> Result<(), Error> {
        if let Some(lock) = self.0.take() {
            lock.unlock().await?;
        }
        Ok(())
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
    #[instrument(skip(self))]
    pub async fn play(&'a self) -> Result<(), Error> {
        #[cfg(feature = "sound")]
        {
            let mut sound = SoundInterface::lease().await?;
            for (note, slice) in &self.note_sequence {
                match note {
                    None => tokio::time::sleep(slice.to_duration(self.tempo_qpm)).await,
                    Some(n) => sound.play_for_time_slice(self.tempo_qpm, n, slice).await?,
                };
            }
            sound.close().await?;
        }
        Ok(())
    }
}

impl Drop for SoundInterface {
    fn drop(&mut self) {
        let guard = self.0.take();
        tokio::spawn(async move {
            if let Err(e) = tokio::fs::write(&*UNEXPORT_FILE, "0").await {
                tracing::error!("Failed to Unexport Sound Interface: {}", e);
                tracing::debug!("{:?}", e);
            }
            if let Some(guard) = guard {
                if let Err(e) = guard.unlock().await {
                    tracing::error!("Failed to drop Sound Interface File Lock: {}", e);
                    tracing::debug!("{:?}", e);
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
        let temp = (*self as isize) + n;

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

#[derive(Debug, Clone, Copy)]
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
        let micros_per_quarter = 1_000_000f64 * 60f64 / tempo_qpm as f64;
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
            #[allow(dead_code)]
            const fn note(semi: Semitone, octave: i8, duration: TimeSlice) -> (Option<Note>, TimeSlice) {
                (
                    Some(Note {
                        semitone: semi,
                        octave,
                    }),
                    duration,
                )
            }
            #[allow(dead_code)]
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

pub const BEP: Song<[(Option<Note>, TimeSlice); 1]> = song!(150, [note(A, 4, Sixteenth);]);

pub const CHIME: Song<[(Option<Note>, TimeSlice); 2]> = song!(400, [
    note(B, 5, Eighth);
    note(E, 6, Tie(&Dot(&Quarter), &Half));
]);

pub const CHARGE: Song<[(Option<Note>, TimeSlice); 7]> = song!(128, [
    note(G, 4, Triplet(&Eighth));
    note(C, 5, Triplet(&Eighth));
    note(E, 5, Triplet(&Eighth));
    note(G, 5, Triplet(&Eighth));
    rest(Triplet(&Eighth));
    note(E, 5, Triplet(&Eighth));
    note(G, 5, Half);
]);

pub const SHUTDOWN: Song<[(Option<Note>, TimeSlice); 12]> = song!(120, [
    note(C, 5, Eighth);
    rest(Eighth);
    note(G, 4, Triplet(&Eighth));
    note(Gb, 4, Triplet(&Eighth));
    note(G, 4, Triplet(&Eighth));
    note(Ab, 4, Quarter);
    note(G, 4, Eighth);
    rest(Eighth);
    rest(Quarter);
    note(B, 4, Eighth);
    rest(Eighth);
    note(C, 5, Eighth);
]);

pub const UPDATE_FAILED_1: Song<[(Option<Note>, TimeSlice); 5]> = song!(120, [
    note(C, 4, Triplet(&Sixteenth));
    note(Eb, 4, Triplet(&Sixteenth));
    note(Gb, 4, Triplet(&Sixteenth));
    note(A, 4, Quarter);
    rest(Eighth);
]);
pub const UPDATE_FAILED_2: Song<[(Option<Note>, TimeSlice); 5]> = song!(110, [
    note(B, 3, Triplet(&Sixteenth));
    note(D, 4, Triplet(&Sixteenth));
    note(F, 4, Triplet(&Sixteenth));
    note(Ab, 4, Quarter);
    rest(Eighth);
]);
pub const UPDATE_FAILED_3: Song<[(Option<Note>, TimeSlice); 5]> = song!(100, [
    note(Bb, 3, Triplet(&Sixteenth));
    note(Db, 4, Triplet(&Sixteenth));
    note(E, 4, Triplet(&Sixteenth));
    note(G, 4, Quarter);
    rest(Eighth);
]);
pub const UPDATE_FAILED_4: Song<[(Option<Note>, TimeSlice); 5]> = song!(90, [
    note(A, 3, Triplet(&Sixteenth));
    note(C, 4, Triplet(&Sixteenth));
    note(Eb, 4, Triplet(&Sixteenth));
    note(Gb, 4, Tie(&Dot(&Quarter), &Quarter));
    rest(Quarter);
]);

pub const BEETHOVEN: Song<[(Option<Note>, TimeSlice); 9]> = song!(216, [
    note(G, 4, Eighth);
    note(G, 4, Eighth);
    note(G, 4, Eighth);
    note(Eb, 4, Half);
    rest(Half);
    note(F, 4, Eighth);
    note(F, 4, Eighth);
    note(F, 4, Eighth);
    note(D, 4, Half);
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
