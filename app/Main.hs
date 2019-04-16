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
  amountToPrint :: Int,
  instrument :: Fretboard
}

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

validInstruments :: String -> Either String Fretboard
validInstruments "guitar" = Right guitar
validInstruments "ukulele" = Right ukulele
validInstruments "mandolin" = Right mandolin
validInstruments i =
  case parse parseInstrument "instrument" i of
    Left bundle -> Left "Valid instruments are: guitar, ukulele, mandolin"
    Right fretboard -> Right fretboard
  where
    parseInstrument :: InputParser Fretboard
    parseInstrument = do
      let parseFret = do
            note <- parseNote
            num <- Text.Megaparsec.some digitChar
            optional $ char ','
            return $ Fret note (read num)
      frets <- Text.Megaparsec.some parseFret
      return frets
      
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
    help "How many fingerings to print?") <*>
  Options.Applicative.option (eitherReader validInstruments) (
    long "instrument" <>
    short 'i' <>
    value guitar <>
    showDefault <>
    metavar "INSTRUMENT" <>
    help "What instrument to show chord diagrams for? Valid instruments are: guitar, ukulele, mandolin, or a comma-delimited list of notes followed by numbers.\nExample: a guitar can be defined as E16,A16,D16,G16,B16,E16.")

parserInfoOptions :: ParserInfo AppOptions
parserInfoOptions = info (parseOptions <**> helper) (
  fullDesc <>
  progDesc "Searches for chord fingerings on a given instrument." <>
  header "cherchord -- find your fingers" <>
  footer "cherchord v1.0.0.0 (c) 2019 https://github.com/aearnus/cherchord")

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
  case parse parseModifiedChord "chord" (chordIn opts) of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right chord -> do
      let chords = search chord (fingerStretch opts) (instrument opts)
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
