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

parseHeader :: ByteString -> (Header,ByteString)
parseHeader raw = (header, rest)
  where
    stripComments = BC.unlines . map (BC.takeWhile (/= '#')) . BC.lines
    (_:rawHeader, splitRest) = splitAt 4 $ BC.words $ stripComments raw
    headerAsInts = map (fst . fromJust . BC.readInt) rawHeader
    header = Header (headerAsInts !! 0) (headerAsInts !! 1) (headerAsInts !! 2)
    rest = BC.unwords splitRest
