module ChordParser where

import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Data.Void
import Chords 
-- TODO: (Note, Chord, Fretboard, maj, min, dim, aug, sus2, sus4)

type InputParser = Parsec Void String

parseNote :: InputParser Note
parseNote = foldr1 (<|>) $ try <$> [
    string "Ab" >> return Ab,
    string "A#" >> return Bb,
    string "A" >> return A,
    string "Bb" >> return Bb,
    string "B#" >> return C,
    string "B" >> return B,
    string "Cb" >> return B,
    string "C#" >> return Db,
    string "C" >> return C,
    string "Db" >> return Db,
    string "D#" >> return Eb,
    string "D" >> return D,
    string "Eb" >> return Eb,
    string "E#" >> return F,
    string "E" >> return E,
    string "Fb" >> return E,
    string "F#" >> return Gb,
    string "F" >> return F,
    string "Gb" >> return Gb,
    string "G#" >> return Ab,
    string "G" >> return G
  ]

parseMod :: InputParser (Note -> Chord)
parseMod = foldr1 (<|>) $ try <$> [
  string "maj" >> pure maj,
  string "min" >> pure Chords.min,
  string "dim" >> pure dim,
  string "aug" >> pure aug,
  string "sus2" >> pure sus2,
  string "sus4" >> pure sus4,
  string ""  >> pure maj]

parseBaseChord :: InputParser Chord
parseBaseChord = do
  note <- parseNote
  mod <- parseMod
  return $ mod note
parseModifiedChord :: InputParser Chord
parseModifiedChord = do
  base <- parseBaseChord
  modifiers <- Text.Megaparsec.many (try parseSlash <|> try parseAdd)
  return $ foldr (.) id modifiers base
  where
    parseSlash :: InputParser (Chord -> Chord)
    parseSlash = do
      string "/"
      note <- parseNote
      pure (`slash` note)
    parseAdd :: InputParser (Chord -> Chord)
    parseAdd = do
      string "add"
      num <- Text.Megaparsec.some digitChar
      return $ add (read num)


parseInstrument :: InputParser Fretboard
parseInstrument = do
    let parseFret = do
            note <- parseNote
            num <- Text.Megaparsec.some digitChar
            optional $ char ','
            return $ Fret note (read num)
    Text.Megaparsec.some parseFret
    
