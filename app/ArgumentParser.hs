module ArgumentParser where

import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Options.Applicative
import NeatInterpolation
import Data.Version (showVersion)

import Paths_cherchord (version)
import Chords (Fretboard, guitar, ukulele, mandolin, bouzouki, baglamas)
import ChordParser (parseInstrument)

data AppOptions = AppOptions {
  chordIn :: String,
  fingerStretch :: Int,
  amountToPrint :: Int,
  amountToPrintInRow :: Int,
  isHorizontal :: Bool,
  instrument :: Fretboard
}

validInstruments :: String -> Either String Fretboard
validInstruments "guitar" = Right guitar
validInstruments "ukulele" = Right ukulele
validInstruments "mandolin" = Right mandolin
validInstruments "bouzouki" = Right bouzouki
validInstruments "baglamas" = Right baglamas
validInstruments i =
  case parse parseInstrument "instrument" i of
    Left bundle -> Left "Valid instruments are: guitar, ukulele, mandolin, bouzouki, baglamas"
    Right fretboard -> Right fretboard

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
    short 'n' <>
    value 10000 <>
    showDefault <>
    metavar "FINGERINGS" <>
    help "How many fingerings to print?") <*>
  Options.Applicative.option auto (
    long "row-size" <>
    short 'r' <>
    value (-1) <>
    showDefault <>
    metavar "COUNT" <>
    help "How many fingerings to put in a row? Lower this number if your terminal is narrow. Set it to -1 to attempt to autosize.") <*>
  Options.Applicative.switch (
    long "horizontal" <>
    help "Should we print the chords horizontally? By default, they are printed vertically.") <*>
  Options.Applicative.option (eitherReader validInstruments) (
    long "instrument" <>
    short 'i' <>
    value guitar <>
    metavar "[INSTRUMENT | INSTRUMENT DEFINITION]" <>
    help (Text.unpack [text|
      Either provide the name of a built-in instrument or create your own from a set of notes.

      The built-in instruments may be referenced by name: guitar, ukulele, mandolin, bouzouki, or baglamas are currently available.

      You may also provide an instrument definition using a comma-delimited list of base note and length pairs. 
      
      The base note indicates what note an open string plays, and the length number indicates how many half steps above each base note may be played on the instrument.
      
      For example, a guitar with drop D tuning can be defined with the flag --instrument D16,A16,D16,G16,B16,E16.

      (default: guitar)
    |]))

parserInfoOptions :: ParserInfo AppOptions
parserInfoOptions = info (parseOptions <**> helper) (
  fullDesc <>
  progDesc "Searches for chord fingerings on a given instrument." <>
  header "cherchord -- find your fingers" <>
  footer ("cherchord v" ++ showVersion version ++ " (c) 2023 https://github.com/DataKinds/cherchord"))

