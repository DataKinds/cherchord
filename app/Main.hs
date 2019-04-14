{-# LANGUAGE ApplicativeDo #-}

module Main where

import Chords
import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Data.Void
import Data.List
import Data.List.Split
import System.IO
import Options.Applicative
import Data.Semigroup ((<>))
import System.Console.ANSI

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

parseBaseChord :: InputParser Chord
parseBaseChord = do
  note <- parseNote
  key <- parseKey
  case key of
    "maj" -> return $ maj note
    "min" -> return $ Chords.min note
    "dim" -> return $ dim note
    "aug" -> return $ aug note

parseModifiedChord :: InputParser Chord
parseModifiedChord = do
  base <- parseBaseChord
  modifiers <- Text.Megaparsec.many (try parseSlash <|> try parseAdd)
  return $ (foldr (.) id modifiers) base
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
      
parseOptions :: Parser AppOptions
parseOptions = AppOptions <$>
  strArgument (metavar "CHORD") <*>
  Options.Applicative.option auto (
    long "finger-stretch" <>
    short 'f' <>
    value 3 <>
    showDefault <>
    metavar "FRETS" <>
    help "How far can your fingers stretch?") <*>
  Options.Applicative.option auto (
    long "print-n" <>
    short 'p' <>
    value 10000 <>
    showDefault <>
    metavar "FINGERINGS" <>
    help "How many fingerings to print?")

parserInfoOptions :: ParserInfo AppOptions
parserInfoOptions = info (helper <*> parseOptions) (
  fullDesc <>
  progDesc "Searches for chord fingering on a given instrument." <>
  header "chord-finder -- find your fingers")

horizConcat :: [String] -> String
horizConcat = foldr1 horizConcatOne
  where
    rectangular :: Int -> String -> String
    rectangular height s =
      let
        maxLine = maximum $ length <$> lines s
        heightDiff = max 0 (height - (length . lines $ s))
      in
        unlines $ (\line -> line ++ replicate (maxLine - length line) ' ') <$> (lines s ++ replicate heightDiff "") 
    
    horizConcatOne :: String -> String -> String
    horizConcatOne str1 str2 =
      let
        height = max (length . lines $ str1) (length . lines $ str2)
        str1' = lines . rectangular height $ str1
        str2' = lines . rectangular height $ str2
      in
        unlines $ zipWith (\s1 s2 -> s1 ++ "  " ++ s2) str1' str2'

main :: IO ()
main = do
  opts <- execParser parserInfoOptions
  case parse parseModifiedChord "interactive" (chordIn opts) of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right chord -> do
      let chords = search chord (fingerStretch opts) guitar
      setSGR [SetColor Foreground Dull Blue]
      putStr "found "
      setSGR [SetColor Foreground Vivid Blue]
      putStr . show $ length chords
      setSGR [SetColor Foreground Dull Blue]
      putStr " unique fingerings for the chord "
      setSGR [SetColor Foreground Vivid Blue]
      putStr (chordIn opts)
      putStrLn . (\s -> " (" ++ s ++ ")") . show $ upperNotes chord
      setSGR [SetColor Foreground Dull Blue]
      putStrLn $ "printing out " ++ show (Prelude.min (amountToPrint opts) (length chords)) ++ " of them...\n"
      setSGR [Reset]
      putStrLn . intercalate "\n" . map horizConcat . chunksOf 3 . map show . take (amountToPrint opts) $ chords
