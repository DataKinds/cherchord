module Main where

import Data.List
import Data.List.Split
import System.IO
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty)
import Data.Semigroup ((<>))
import System.Console.ANSI
import Text.Megaparsec (parse, eof)
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.Text as Text (unpack)

import qualified Chords as C
import qualified ArgumentParser as A
import qualified ChordParser as P

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
  opts <- customExecParser (prefs showHelpOnEmpty) A.parserInfoOptions
  case parse (P.parseModifiedChord <* eof) "chord" (A.chordIn opts) of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right chord -> do
      let chords = C.search chord (A.fingerStretch opts) (A.instrument opts)
          printCount = A.amountToPrint opts
          whichShow = if A.isHorizontal opts then C.showHorizontally else show
          horizontalCount = if A.amountToPrintInRow opts < 0 
            then if A.isHorizontal opts then 6 else 4
            else A.amountToPrintInRow opts
      setSGR [SetColor Foreground Dull Blue]
      putStr "found "
      setSGR [SetColor Foreground Vivid Blue]
      putStr . show $ length chords
      setSGR [SetColor Foreground Dull Blue]
      putStr " unique fingerings for the chord "
      setSGR [SetColor Foreground Vivid Blue]
      putStr (A.chordIn opts)
      putStrLn . (\s -> " (" ++ s ++ ")") . show $ C.upperNotes chord
      setSGR [SetColor Foreground Dull Blue]
      putStrLn $ "printing out " ++ show (min printCount (length chords)) ++ " of them...\n"
      setSGR [Reset]
      putStrLn . intercalate "\n" . map horizConcat . chunksOf horizontalCount . map whichShow . take printCount $ chords
