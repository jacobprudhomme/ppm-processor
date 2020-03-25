module PPM
  ( parseHeader
  ) where

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
data Pixel = Pixel Int Int Int

parseHeader :: ByteString -> (Header,[ByteString])
parseHeader raw = (header, rest)
  where
    stripComments = BC.unlines . map (BC.takeWhile (/= '#')) . BC.lines
    (_:rawHeader, rest) = splitAt 4 $ BC.words $ stripComments raw
    headerAsInts = map (fst . fromJust . BC.readInt) rawHeader
    header = Header (headerAsInts !! 0) (headerAsInts !! 1) (headerAsInts !! 2)

parseBodyRaw :: [ByteString] -> [Pixel]
parseBodyRaw []         = []
parseBodyRaw (r:g:b:xs) = Pixel r' g' b' : parseBodyRaw xs
  where
    toInt = fst . fromJust . BC.readInt
    r' = toInt r
    g' = toInt g
    b' = toInt b
