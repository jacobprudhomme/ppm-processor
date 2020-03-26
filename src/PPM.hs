module PPM
  ( flipPPM
  , parsePPM
  , rotatePPM
  ) where

import Data.List (transpose)
import Data.Maybe (fromJust)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC


-- Just works with P3 format for now
data Header = Header
  { width :: Int
  , height :: Int
  , maxColourVal :: Int
  } deriving Show

-- Pixel R G B
data Pixel = Pixel Int Int Int deriving Show

type Body = [[Pixel]]

data PPM = PPM Header Body deriving Show


-- Use only if sure that the bytestring value is an integer
toInt :: ByteString -> Int
toInt = fst . fromJust . BC.readInt

parseHeader :: ByteString -> (Header,[ByteString])
parseHeader raw = (header, rest)
  where
    stripComments = BC.unlines . map (BC.takeWhile (/= '#')) . BC.lines
    (_:rawHeader, rest) = splitAt 4 $ BC.words $ stripComments raw
    headerAsInts = map toInt rawHeader
    header = Header (headerAsInts !! 0) (headerAsInts !! 1) (headerAsInts !! 2)

parseBodyRaw :: [ByteString] -> [Pixel]
parseBodyRaw []         = []
parseBodyRaw (r:g:b:xs) = Pixel r' g' b' : parseBodyRaw xs
  where [r',g',b'] = map toInt [r,g,b]

splitIntoRows :: Int -> [a] -> [[a]]
splitIntoRows _ []        = []
splitIntoRows lenOfRow xs =
  take lenOfRow xs : splitIntoRows lenOfRow (drop lenOfRow xs)

parseBody :: Header -> [ByteString] -> Body
parseBody header rest = splitIntoRows w rawBody
  where
    rawBody = parseBodyRaw rest
    w = width header

parsePPM :: ByteString -> PPM
parsePPM raw = PPM header body
  where
    (header, rest) = parseHeader raw
    body = parseBody header rest

flipPPM :: PPM -> PPM
flipPPM (PPM header body) = PPM header $ map reverse body

rotatePPM :: PPM -> PPM
rotatePPM (PPM header body) = PPM header $ reverse (transpose body)
