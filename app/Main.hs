module Main where

import Chords

main :: IO ()
main = mapM_ (putStrLn . show) $ search (maj C) 4 guitar
