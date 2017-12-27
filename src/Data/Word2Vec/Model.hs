{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Word2Vec.Model
    ( readWord2VecModel
    , getVector
    , cosineSimilarity
    , dotProduct
    , WVector
    , findNearestToWord
    , buildWVector
    ) where

import qualified Data.HashMap.Strict as DHS
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BS
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.List (maximumBy)

data WVector = WVector (V.Vector Float) Float
               deriving (Eq, Show)

data Word2VecModel = Word2VecModel Int Int !(DHS.HashMap T.Text WVector)
                     deriving (Eq, Show)

readWord2VecModel :: FilePath -> IO (Word2VecModel)
readWord2VecModel fileName = do
  contents <- BL.readFile fileName
  pure $ processWord2VecBinaryModel contents

getVector :: Word2VecModel -> T.Text -> Maybe WVector
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

parseWord2VecEntry :: Int -> AP.Parser (T.Text, WVector)
parseWord2VecEntry nbOfDimensions = do
  word <- AP.takeWhile1 (not . AP.isSpace)
  " "
  floatVectorRaw <- AP.take (floatSize * nbOfDimensions)
  return (decodeUtf8 word, buildWVector $ bytesToFloats floatVectorRaw)

bytesToFloats :: BS.ByteString -> V.Vector Float
bytesToFloats = V.unsafeCast . aux . BS.toForeignPtr
  where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len

buildWVector :: V.Vector Float -> WVector
buildWVector v = WVector v (normRooted v)

cosineSimilarity :: WVector -> WVector -> Float
cosineSimilarity (WVector veca snorma) (WVector vecb snormb) = (dotProduct veca vecb) / (snorma * snormb)
  where norm v = V.sum $ V.map (\e -> e * e) v

dotProduct :: V.Vector Float -> V.Vector Float -> Float
dotProduct veca vecb = V.sum $ V.zipWith (*) veca vecb

normRooted :: V.Vector Float -> Float
normRooted = sqrt . V.sum . V.map (\e -> e * e)

findNearestToWord :: Word2VecModel -> T.Text -> Maybe (T.Text, Float)
findNearestToWord m@(Word2VecModel _ _ h) w = findNearestToWord' h <$> (getVector m w)
   where findNearestToWord' h v = maximumBy (\(_,p) (_, q) -> p `compare` q)
                                  $ map (\(w',v') -> (w', cosineSimilarity v' v))
                                  $ filter (\(w',_) -> w' /= w) $ DHS.toList h
