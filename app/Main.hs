module Main where

import Data.Maybe (fromJust)
import System.Environment

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

import PPM


transformMap :: Map.Map String (PPM -> PPM)
transformMap = Map.fromList
  [ ("flip", flipPPM)
  , ("rotate", rotatePPM)
  , ("sepia", sepiaPPM)
  ]

applyTransform :: PPM -> String -> PPM
applyTransform ppm command = fromJust (Map.lookup command transformMap) ppm

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  let transforms = tail args
  ppmFile <- B.readFile filename
  let ppm = parsePPM ppmFile
  let transformed = foldl applyTransform ppm transforms
  let transformedFilename = "transformed_" ++ filename
  B.writeFile transformedFilename (writePPM transformed)
  putStrLn "Success!"
