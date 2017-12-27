{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Word2Vec.Model
    ( readWord2VecModel
    , getVector
    ) where

import qualified Data.HashMap.Strict as DHS
import Data.Text
import Data.Text.Encoding
import Data.Vector.Storable
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
data Word2VecModel = Word2VecModel Int Int !(DHS.HashMap Text (Vector Float))
                     deriving (Eq, Show)

readWord2VecModel :: FilePath -> IO (Word2VecModel)
readWord2VecModel fileName = do
  contents <- BL.readFile fileName
  pure $ processWord2VecBinaryModel contents

getVector :: Word2VecModel -> Text -> Maybe (Vector Float)
getVector (Word2VecModel _ _ h) w = DHS.lookup w h

processWord2VecBinaryModel :: BL.ByteString -> Word2VecModel
processWord2VecBinaryModel contents =
  case AL.parse parseWord2VecBinaryModel contents of
    AL.Fail _ _ _ -> error "WTF?"
    AL.Done _ !m -> m

parseWord2VecBinaryModel :: AP.Parser Word2VecModel
parseWord2VecBinaryModel = do
  nbOfWords <- AP.decimal
  " "
  nbOfDimensions <- AP.decimal
  "\n"
  entries <- AP.many' (parseWord2VecEntry nbOfDimensions <* "\n")
  return $ Word2VecModel nbOfWords nbOfDimensions $ DHS.fromList entries

floatSize :: Int
floatSize = 4

parseWord2VecEntry :: Int -> AP.Parser (Text, Vector Float)
parseWord2VecEntry nbOfDimensions = do
  word <- AP.takeWhile1 (not . AP.isSpace)
  " "
  floatVectorRaw <- AP.take (floatSize * nbOfDimensions)
  return (decodeUtf8 word, buildFloatVector nbOfDimensions floatVectorRaw)

buildFloatVector nbOfDimensions !bs = runGet (replicateM nbOfDimensions getFloathost) $ BL.fromStrict bs
