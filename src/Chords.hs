{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Chords where

import Data.Data
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

data Note = Ab | A | Bb | B | C | Db | D | Eb | E | F | Gb | G deriving (Show, Eq, Ord, Data)
type Chord = [Note]

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

incNote :: Int -> Note -> Note
incNote n = foldr (.) id $ replicate n succ

maj :: Note -> Chord
maj note = ($ note) <$> [id, succ . succ . succ . succ, succ . succ . succ . succ . succ . succ . succ]

min :: Note -> Chord
min note = ($ note) <$> [id, succ . succ . succ, succ . succ . succ . succ . succ . succ . succ]

aug :: Note -> Chord
aug note = ($ note) <$> [id, succ . succ . succ . succ, succ . succ . succ . succ . succ . succ . succ . succ]

dim :: Note -> Chord
dim note = ($ note) <$> [id, succ . succ . succ, succ . succ . succ . succ . succ . succ]

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
      Just finger | finger == row -> "*"
      _ -> "|"
  ) <$> fingers


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

-- A ---------------*- -> c
-- E --------------    -> E
-- C --------------    -> C
-- G --------------    -> G

isChord :: Chord -> Fingering -> Bool
isChord chord (Fingering fingers frets) =
  let
    noteWithStatus = zip fingers (fretZero <$> frets)
    maybeNote = (\(finger', base) -> finger' >>= (\finger -> Just $ incNote finger base)) <$> noteWithStatus
  in
    S.fromList chord == S.fromList (catMaybes maybeNote)

isChordPossible :: Chord -> (Int, Int) -> Fretboard -> Bool
isChordPossible chord interval@(mi, ma) frets = and (okayNote chord interval <$> frets)
  
okayNote :: Chord -> (Int, Int) -> Fret -> Bool
okayNote chord interval@(mi, ma) fret =
  let
    everyPossibleNote = S.fromList $ (flip incNote $ (fretZero fret)) <$> [mi..ma]
    everyNeededNote = S.fromList chord
  in
    not . null $ everyNeededNote `S.intersection` everyPossibleNote

cartesianChordOn :: (Int, Int) -> Chord -> Fretboard -> [Fingering]
cartesianChordOn interval@(mi, ma) chord frets =
  let
    unorderedIncrementList = (\fret -> validNoteIncrements interval chord (fretZero fret)) <$> frets
    cartesianIncrements = sequence unorderedIncrementList
  in
    (\fingers -> Fingering (Just <$> fingers) frets) <$> cartesianIncrements
  where
    validNoteIncrements :: (Int, Int) -> Chord -> Note -> [Int]
    validNoteIncrements (mi, ma) chord basenote = filter (\inc -> (incNote inc basenote) `elem` chord) [mi..ma]
  

search :: Chord -> Int -> Fretboard -> [Fingering]
search chord maxInterval frets =
  let
    -- TODO: CHANGE THIS LINE
    okayIntervals = filter (\interval -> isChordPossible chord interval frets) $ zip [0 .. ((fretLength $ head frets) - maxInterval)] [maxInterval .. (fretLength $ head frets)]
  in
    (\interval -> cartesianChordOn interval chord frets) =<< okayIntervals

guitar :: Fretboard
guitar = ($ 16) <$> [Fret E, Fret A, Fret D, Fret G, Fret B, Fret E]

ukulele :: Fretboard
ukulele = ($ 14) <$> [Fret G, Fret C, Fret E, Fret A]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
