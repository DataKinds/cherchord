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
  string "A" >> return A,
  string "Bb" >> return Bb,
  string "B" >> return B,
  string "C" >> return C,
  string "Db" >> return Db,
  string "D" >> return D,
  string "Eb" >> return Eb,
  string "E" >> return E,
  string "F" >> return F,
  string "Gb" >> return Gb,
  string "G" >> return G]

parseKey :: InputParser String
parseKey = foldr1 (<|>) $ try <$> [
  string "maj",
  string "min",
  string "dim",
  string "aug",
  string "sus2",
  string "sus4",
  string ""]

parseBaseChord :: InputParser Chord
parseBaseChord = do
  note <- parseNote
  key <- parseKey
  case key of
    "maj" -> return $ maj note
    "" -> return $ maj note
    "min" -> return $ Chords.min note
    "dim" -> return $ dim note
    "aug" -> return $ aug note
    "sus2" -> return $ sus2 note
    "sus4" -> return $ sus4 note

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
      return $ slash note
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
    
