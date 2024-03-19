module Main where

import Graphics.Gloss
import System.Environment
import QOI
import System.Exit
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    putStrLn "Expected qoi file as argument"
    exitWith (ExitFailure 1)
  let file = head args
  img <- qoiFromFile file
  let (width, height) = imageDim img
  display (InWindow  ("QOI Viewer - " <> file) (width, height) (0,0)) white $ bitmapOfQOI img
