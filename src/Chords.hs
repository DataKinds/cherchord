{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Chords where

import Data.Data
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

-- ## DECLARATIONS ## --

data Note = Ab | A | Bb | B | C | Db | D | Eb | E | F | Gb | G deriving (Show, Eq, Ord, Data)
data Chord = Chord {
  baseNote :: Note,
  upperNotes :: [Note]
}

instance Enum Note where
  toEnum n =
    let boundedN = n `mod` 12
        constrs = dataTypeConstrs . dataTypeOf $ C
    in
      fromConstr (constrs !! boundedN)
  fromEnum note =
    let constrs = dataTypeConstrs . dataTypeOf $ note
        noteConstr = toConstr note
    in
      head $ elemIndices noteConstr constrs

data Fret = Fret {
  fretZero :: Note,
  fretLength :: Int
}

type Fretboard = [Fret]

data Fingering = Fingering {
  fingerPos :: [Maybe Int],
  fingerFrets :: Fretboard
}

showFingeringRow :: Int -> Fingering -> String
showFingeringRow row (Fingering fingers fretboard) =
  intercalate " " $ (\case
      Just finger | row == 0 -> "-"
      Just finger | finger == row -> "*"
      _ -> "|"
  ) <$> fingers

-- | What are the top and bottom fingers in this chord?
minMaxFingers :: Fingering -> (Int, Int)
minMaxFingers (Fingering fingers fretboard) =
  let
    justFingers = catMaybes fingers
    ms@(mi,ma) = (minimum justFingers, maximum justFingers)
  in
    if (ma - mi) < 4 then (mi, mi + 3) else ms

rightPad2 :: String -> String
rightPad2 = \name -> if (length name == 1) then name ++ " " else name

instance Show Fingering where
  show f@(Fingering fingers fretboard) =
    let
      noteHeader = (rightPad2 . show . fretZero) =<< fretboard
      muteHeader = (rightPad2 . maybe "X" show) =<< fingers
      (minFinger, maxFinger) = minMaxFingers f
      strings = (flip showFingeringRow) f <$> [minFinger..maxFinger]
    in
      unlines (noteHeader:muteHeader:strings)

instance Eq Fingering where
  (==) (Fingering fingers frets) (Fingering fingers' frets') = fingers == fingers'


-- ## CONSTRUCTORS ## --

incNote :: Int -> Note -> Note
incNote n = foldr (.) id $ replicate n succ

maj :: Note -> Chord
maj note = Chord note $ (flip incNote $ note) <$> [0, 4, 7]

min :: Note -> Chord
min note = Chord note $ (flip incNote $ note) <$> [0, 3, 7]

aug :: Note -> Chord
aug note = Chord note $ (flip incNote $ note) <$> [0, 4, 8]

dim :: Note -> Chord
dim note = Chord note $ (flip incNote $ note) <$> [0, 3, 6]


slash :: Note -> Chord -> Chord
slash note (Chord base upper) = Chord base (note:upper)

add :: Int -> Chord -> Chord
add num (Chord base upper) = Chord base (upper ++ [incNote num base])

-- A ---------------*- -> c
-- E --------------    -> E
-- C --------------    -> C
-- G --------------    -> G

-- ## SEARCH FUNCTIONS ## --

-- | Does this fingering produce this chord?
isChord :: Chord -> Fingering -> Bool
isChord chord (Fingering fingers frets) =
  let
    noteWithStatus = zip fingers (fretZero <$> frets)
    maybeNote = (\(finger', base) -> finger' >>= (\finger -> Just $ incNote finger base)) <$> noteWithStatus
  in
    S.fromList (upperNotes chord) == S.fromList (catMaybes maybeNote)

-- | Can a string produce a certain note when a finger only lands within a certain interval?
-- | Note -- this assumes that the string is _not_ muted, it _must_ produce a sound.
okayNote :: Chord -> (Int, Int) -> Fret -> Bool
okayNote chord interval@(mi, ma) fret =
  let
    everyPossibleNote = S.fromList $ (flip incNote $ (fretZero fret)) <$> 0:[mi..ma]
    everyNeededNote = S.fromList (upperNotes chord)
  in
    not . null $ everyNeededNote `S.intersection` everyPossibleNote

-- | Can we produce a chord within some interval of fingering?
isChordPossible :: Chord -> (Int, Int) -> Fretboard -> Int -> Bool
isChordPossible chord interval@(mi, ma) frets maximumMutes = (length $ filter not (okayNote chord interval <$> frets)) <= maximumMutes

-- | If there's a version of `specific` in the list of fingerings
-- | that _isn't_ muted as much, then the more muted version is redundant.
isntRedundant :: [[Maybe Int]] -> [Maybe Int] -> Bool
isntRedundant (c:cartesian) specific = (not $ any (\case { (Just _, Nothing) -> True; _ -> False }) (zip c specific)) && isntRedundant cartesian specific
isntRedundant [] specific = True

-- | Give me all the ways to produce a chord within this interval, including leaving an open string.
cartesianChordOn :: (Int, Int) -> Int -> Chord -> Fretboard -> [Fingering]
cartesianChordOn interval@(mi, ma) maxMutes chord frets =
  let
    -- We leave the opportunity for this string to possibly be muted.
    unorderedIncrementList = (\fret -> (validNoteIncrements chord (fretZero fret))) <$> frets
    -- Then we filter to make sure that _too_ many strings aren't muted.
    -- An interesting edge case -- a completely muted instrument can sound like any chord ;)
    cartesianIncrements = filter (\fs -> (length fs) - (length $ catMaybes fs) <= maxMutes) $ sequence unorderedIncrementList
    -- Now, we remove all the fingerings that have a mute but really shouldn't
    -- This runs in O(n^2) time & space but it shouldn't matter because chords only have ~100 fingerings
    -- If it's ever a problem, though, refactor this!!
    minMuteIncrements = filter (isntRedundant cartesianIncrements) cartesianIncrements
  in
    (\fingers -> Fingering fingers frets) <$> minMuteIncrements
  where
    -- The muted string is _always_ valid
    validNoteIncrements :: Chord -> Note -> [Maybe Int]
    validNoteIncrements chord basenote = Nothing:(Just <$> filter (\inc -> (incNote inc basenote) `elem` (upperNotes chord)) (nub $ 0:[mi..ma]))

-- | Is this fingering kinda too awkward to do?
-- | Right now, the only criteria is that it has a mute surrounded by non-zero fingerings
isAwkward :: Fingering -> Bool
isAwkward (Fingering ((Just f1):Nothing:(Just f2):fs) fb)
  | f1 == 0 || f2 == 0 = isAwkward (Fingering (Nothing:(Just f2):fs) fb)
  | otherwise = True
isAwkward (Fingering (f:fs) fb) = isAwkward (Fingering fs fb)
isAwkward (Fingering [] fb) = False

  
-- | Give me all the ways to produce a chord, when my fingers can only stretch so far.
search :: Chord -> Int -> Fretboard -> [Fingering]
search chord maxInterval frets =
  let
    maxMutes = length frets `div` 4
    -- TODO: CHANGE THIS LINE
    intervals = zip [0 .. ((fretLength $ head frets) - maxInterval)] [maxInterval .. (fretLength $ head frets)]
    okayIntervals = filter (\interval -> isChordPossible chord interval frets maxMutes) $ intervals
  in
    nub $ filter (not . isAwkward) $ (\interval -> cartesianChordOn interval maxMutes chord frets) =<< okayIntervals

guitar :: Fretboard
guitar = ($ 16) <$> [Fret E, Fret A, Fret D, Fret G, Fret B, Fret E]

ukulele :: Fretboard
ukulele = ($ 14) <$> [Fret G, Fret C, Fret E, Fret A]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
