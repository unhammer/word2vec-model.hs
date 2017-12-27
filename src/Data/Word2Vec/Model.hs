{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Word2Vec.Model
    ( readWord2VecModel
    , getVector
    , cosineSimilarity
    , dotProduct
    , WVector
    ) where

import qualified Data.HashMap.Strict as DHS
import Data.Text
import Data.Text.Encoding
import Data.Vector.Storable as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BS
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754

type WVector = Vector Float

data Word2VecModel = Word2VecModel Int Int !(DHS.HashMap Text WVector)
                     deriving (Eq, Show)

readWord2VecModel :: FilePath -> IO (Word2VecModel)
readWord2VecModel fileName = do
  contents <- BL.readFile fileName
  pure $ processWord2VecBinaryModel contents

getVector :: Word2VecModel -> Text -> Maybe WVector
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

parseWord2VecEntry :: Int -> AP.Parser (Text, WVector)
parseWord2VecEntry nbOfDimensions = do
  word <- AP.takeWhile1 (not . AP.isSpace)
  " "
  floatVectorRaw <- AP.take (floatSize * nbOfDimensions)
  return (decodeUtf8 word, bytesToFloats floatVectorRaw)

bytesToFloats :: BS.ByteString -> WVector
bytesToFloats = unsafeCast . aux . BS.toForeignPtr
  where aux (fp,offset,len) = unsafeFromForeignPtr fp offset len

cosineSimilarity :: WVector -> WVector -> Float
cosineSimilarity veca vecb = (dotProduct veca vecb) / (sqrt ((norm veca) * (norm vecb)))
  where norm v = V.sum $ V.map (**2.0) v

dotProduct :: WVector -> WVector -> Float
dotProduct veca vecb = V.sum $ V.zipWith (*) veca vecb
