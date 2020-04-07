module Main where

import System.Environment

import qualified Data.ByteString.Lazy as B

import PPM


transformDispatch :: String -> (PPM -> PPM)
transformDispatch command = case command of
  "flip" -> flipPPM
  "rotate" -> rotatePPM
  "sepia" -> sepiaPPM
  _ -> id

applyTransform :: PPM -> String -> PPM
applyTransform ppm command = transformDispatch command ppm

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
