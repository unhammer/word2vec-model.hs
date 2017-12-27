module Main where

import Data.Word2Vec.Model
import Data.Text (pack)

main :: IO ()
main = do
  w2v <- readWord2VecModel "../../efemerydy/w2v-vectors-raw-fixed.bin"
  print $ getVector w2v (pack "polska")
  print $ findNearestToWord w2v (pack "polska")
  print $ findNearestToWord w2v (pack "komputer")
  print $ findNearestToWord w2v (pack "źdźbło")
