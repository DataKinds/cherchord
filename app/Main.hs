module Main where

import Chords
import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Data.Void
import System.IO

type Parser = Parsec Void String

parseNote :: Parser Note
parseNote = foldr1 (<|>) $ try <$> [
  string "Ab" >> return Ab,
  string "A" >> return A,
  string "Bb" >> return Bb,
  string "C" >> return C,
  string "Db" >> return Db,
  string "D" >> return D,
  string "Eb" >> return Eb,
  string "E" >> return E,
  string "F" >> return F,
  string "Gb" >> return Gb,
  string "G" >> return G]

parseModifier :: Parser String
parseModifier = foldr1 (<|>) $ try <$> [
  string "maj",
  string "min",
  string "dim",
  string "aug"]

parseChord :: Parser Chord
parseChord = do
  note <- parseNote
  modifier <- parseModifier
  case modifier of
    "maj" -> return $ maj note
    "min" -> return $ Chords.min note
    "dim" -> return $ dim note
    "aug" -> return $ aug note
  
main :: IO ()
main = do
  putStr "type a chord> "
  hFlush stdout
  input <- getLine
  case parse parseChord "interactive" input of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right chord -> do
      mapM_ (putStrLn . show) $ search chord 3 guitar
      main
