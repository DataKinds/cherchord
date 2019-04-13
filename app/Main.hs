{-# LANGUAGE ApplicativeDo #-}

module Main where

import Chords
import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Data.Void
import System.IO
import Options.Applicative
import Data.Semigroup ((<>))

type InputParser = Parsec Void String
data AppOptions = AppOptions {
  chordIn :: String,
  fingerStretch :: Int,
  amountToPrint :: Int
}


parseNote :: InputParser Note
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

parseKey :: InputParser String
parseKey = foldr1 (<|>) $ try <$> [
  string "maj",
  string "min",
  string "dim",
  string "aug"]

parseChord :: InputParser Chord
parseChord = do
  note <- parseNote
  key <- parseKey
  case key of
    "maj" -> return $ maj note
    "min" -> return $ Chords.min note
    "dim" -> return $ dim note
    "aug" -> return $ aug note

parseOptions :: Parser AppOptions
parseOptions = AppOptions <$> strArgument (metavar "CHORD") <*> pure 3 <*> pure 1000

parserInfoOptions :: ParserInfo AppOptions
parserInfoOptions = info parseOptions (
  fullDesc <>
  progDesc "Searches for chord fingering on a given instrument." <>
  header "chord-finder -- find your fingers")

main :: IO ()
main = do
  opts <- execParser parserInfoOptions
  case parse parseChord "interactive" (chordIn opts) of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right chord -> do
      mapM_ (putStrLn . show) $ search chord (fingerStretch opts) guitar
