module Main where

import Data.Word2Vec.Model
import Data.Text (pack)

main :: IO ()
main = do
  w2v <- readWord2VecModel "../../efemerydy/w2v-vectors-fixed.bin"
  print $ getVector w2v (pack "polska")
