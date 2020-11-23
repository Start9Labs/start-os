{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Lib.Sound where

import           Startlude               hiding ( rotate )

import           Control.Monad.Trans.Cont
import           Control.Carrier.Writer.Strict
import           System.FileLock

import           Util.Function

-- General

rotate :: forall a . (Enum a, Bounded a) => a -> Int -> a
rotate base step = toEnum $ (fromEnum base + step) `mod` size + (fromEnum $ minBound @a)
    where size = fromEnum (maxBound @a) - fromEnum (minBound @a) + 1
{-# INLINE rotate #-}


-- Interface

export :: IO ()
export = writeFile "/sys/class/pwm/pwmchip0/export" "0"

unexport :: IO ()
unexport = writeFile "/sys/class/pwm/pwmchip0/unexport" "0"


-- Constants

semitoneK :: Double
semitoneK = 2 ** (1 / 12)
{-# INLINE semitoneK #-}


-- Data Types

data Note = Note Semitone Word8
    deriving (Eq, Show)

data Semitone =
      C
    | Db
    | D
    | Eb
    | E
    | F
    | Gb
    | G
    | Ab
    | A
    | Bb
    | B
    deriving (Eq, Ord, Show, Enum, Bounded)

newtype Interval = Interval Int deriving newtype (Num)

data TimeSlice =
      Sixteenth
    | Eighth
    | Quarter
    | Half
    | Whole
    | Triplet TimeSlice
    | Dot TimeSlice
    | Tie TimeSlice TimeSlice
    deriving (Eq, Show)


-- Theory Manipulation

interval :: Interval -> Note -> Note
interval (Interval n) (Note step octave) =
    let (o', s') = n `quotRem` 12
        newStep  = step `rotate` s'
        offset   = if
            | newStep > step && s' < 0 -> subtract 1
            | newStep < step && s' > 0 -> (+ 1)
            | otherwise                -> id
    in  Note newStep (offset $ octave + fromIntegral o')
{-# INLINE interval #-}

minorThird :: Interval
minorThird = Interval 3

majorThird :: Interval
majorThird = Interval 3

fourth :: Interval
fourth = Interval 5

fifth :: Interval
fifth = Interval 7

circleOfFourths :: Note -> [Note]
circleOfFourths = iterate (interval fourth)

circleOfFifths :: Note -> [Note]
circleOfFifths = iterate (interval fifth)

-- Theory To Interface Target

noteFreq :: Note -> Double
noteFreq (Note semi oct) = semitoneK ** (fromIntegral $ fromEnum semi) * c0 * (2 ** fromIntegral oct)
    where
        a4 = 440
        c0 = a4 / (semitoneK ** 9) / (2 ** 4)

-- tempo is in quarters per minute
timeSliceToMicro :: Word16 -> TimeSlice -> Int
timeSliceToMicro tempo timeSlice = case timeSlice of
    Sixteenth          -> uspq `div` 4
    Eighth             -> uspq `div` 2
    Quarter            -> uspq
    Half               -> uspq * 2
    Whole              -> uspq * 4
    Triplet timeSlice' -> timeSliceToMicro tempo timeSlice' * 2 `div` 3
    Dot     timeSlice' -> timeSliceToMicro tempo timeSlice' * 3 `div` 2
    Tie ts1 ts2        -> timeSliceToMicro tempo ts1 + timeSliceToMicro tempo ts2
    where uspq = floor @Double $ 60 / fromIntegral tempo * 1_000_000


-- Player

periodFile :: FilePath
periodFile = "/sys/class/pwm/pwmchip0/pwm0/period"

dutyFile :: FilePath
dutyFile = "/sys/class/pwm/pwmchip0/pwm0/duty_cycle"

switchFile :: FilePath
switchFile = "/sys/class/pwm/pwmchip0/pwm0/enable"

play :: Note -> IO ()
play note' = do
    prd' <- readFile periodFile
    case prd' of
        "0\n" -> writeFile periodFile "1000"
        _     -> pure ()
    let prd = round @_ @Int $ 1 / noteFreq note' * 1_000_000_000 -- pwm needs it in nanos
    writeFile dutyFile   "0"
    writeFile periodFile (show prd)
    writeFile dutyFile   (show $ prd `div` 2)
    writeFile switchFile "1"

stop :: IO ()
stop = writeFile switchFile "0"

playForDuration :: Note -> Int -> IO ()
playForDuration note' duration = handle @SomeException (\e -> stop *> throwIO e) $ do
    play note'
    threadDelay (floor @Double $ fromIntegral duration * 0.95)
    stop
    threadDelay (ceiling @Double $ fromIntegral duration * 0.05)

time :: IO () -> IO (UTCTime, UTCTime)
time action = do
    t0 <- getCurrentTime
    action
    t1 <- getCurrentTime
    pure (t0, t1)

playSong :: Word16 -> Song -> IO ()
playSong = flip runCont id .* playSong'
{-# INLINE playSong #-}

playSongTimed :: Word16 -> Song -> IO (UTCTime, UTCTime)
playSongTimed tempo song = runCont (playSong' tempo song) time
{-# INLINE playSongTimed #-}

playSong' :: Word16 -> Song -> Cont (IO b) (IO ())
playSong' tempo song = cont $ \f -> bracket acquire release $ \_ -> f $ do
    for_ song $ \(n, ts) -> do
        let duration = timeSliceToMicro tempo ts
        case n of
            Nothing -> threadDelay duration
            Just x  -> playForDuration x duration
    where
        soundLock = "/root/agent/sound.lock"
        acquire   = do
            l <- lockFile soundLock Exclusive
            export
            pure l
        release l = do
            void $ try @SomeException stop
            void $ try @SomeException unexport
            unlockFile l


-- Songs

type Song = [(Maybe Note, TimeSlice)]

marioDeath :: Song
marioDeath =
    [ (Just $ Note B 4, Quarter)
    , (Just $ Note F 5, Quarter)
    , (Nothing        , Quarter)
    , (Just $ Note F 5, Quarter)
    , (Just $ Note F 5, Triplet Half)
    , (Just $ Note E 5, Triplet Half)
    , (Just $ Note D 5, Triplet Half)
    , (Just $ Note C 5, Quarter)
    , (Just $ Note E 4, Quarter)
    , (Nothing        , Quarter)
    , (Just $ Note E 4, Quarter)
    , (Just $ Note C 4, Half)
    ]

marioPowerUp :: Song
marioPowerUp =
    [ (Just $ Note G 4 , Triplet Eighth)
    , (Just $ Note B 4 , Triplet Eighth)
    , (Just $ Note D 5 , Triplet Eighth)
    , (Just $ Note G 5 , Triplet Eighth)
    , (Just $ Note B 5 , Triplet Eighth)
    , (Just $ Note Ab 4, Triplet Eighth)
    , (Just $ Note C 5 , Triplet Eighth)
    , (Just $ Note Eb 5, Triplet Eighth)
    , (Just $ Note Ab 5, Triplet Eighth)
    , (Just $ Note C 5 , Triplet Eighth)
    , (Just $ Note Bb 4, Triplet Eighth)
    , (Just $ Note D 5 , Triplet Eighth)
    , (Just $ Note F 5 , Triplet Eighth)
    , (Just $ Note Bb 5, Triplet Eighth)
    , (Just $ Note D 6 , Triplet Eighth)
    ]

marioCoin :: Song
marioCoin = [(Just $ Note B 5, Eighth), (Just $ Note E 6, Tie (Dot Quarter) Half)]

updateInProgress :: Song
updateInProgress = take 6 $ (, Triplet Eighth) . Just <$> circleOfFifths (Note A 3)

beethoven :: Song
beethoven = run . execWriter $ do
    tell $ replicate 3 (Just $ Note E 5, Eighth)
    tell $ [(Just $ Note C 5, Half)]
    tell $ [(Nothing @Note, Eighth)]
    tell $ replicate 3 (Just $ Note D 5, Eighth)
    tell $ [(Just $ Note B 5, Half)]

restoreActionInProgress :: Song
restoreActionInProgress = take 5 $ (, Triplet Eighth) . Just <$> circleOfFourths (Note C 4)

backupActionInProgress :: [(Maybe Note, TimeSlice)]
backupActionInProgress = reverse restoreActionInProgress
