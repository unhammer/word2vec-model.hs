{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word2Vec.Model
-- Copyright   :  Filip Graliński 2017
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  filipg@amu.edu.pl
-- Stability   :  experimental
-- Portability :  ?
--
-- Operating on word2vec binary models (generated with the original tool by Mikolov).
--
-----------------------------------------------------------------------------

module Data.Word2Vec.Model
    ( readWord2VecModel

    , numberOfWords
    , numberOfDimensions

    , WVector
    , buildWVector
    , getVector

    , cosineSimilarity
    , dotProduct

    , findNearestToWord
    , findKNearestToWord
    , findKNearestToVector
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
import Data.List (maximumBy, foldl')
import Data.Maybe (catMaybes)

data WVector = WVector (V.Vector Float) Float
               deriving (Eq, Show)

data Word2VecModel = Word2VecModel Int Int !(DHS.HashMap T.Text WVector)
                     deriving (Eq, Show)

readWord2VecModel :: FilePath -> IO (Word2VecModel)
readWord2VecModel fileName = do
  contents <- BL.readFile fileName
  pure $ processWord2VecBinaryModel contents

numberOfWords :: Word2VecModel -> Int
numberOfWords (Word2VecModel n _ _) = n

numberOfDimensions :: Word2VecModel -> Int
numberOfDimensions (Word2VecModel _ d _) = d

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
buildWVector v = WVector v (norm v)

cosineSimilarity :: WVector -> WVector -> Float
cosineSimilarity (WVector veca norma) (WVector vecb normb) = (dotProduct veca vecb) / (norma * normb)
  where norm v = V.sum $ V.map (\e -> e * e) v

dotProduct :: V.Vector Float -> V.Vector Float -> Float
dotProduct veca vecb = V.sum $ V.zipWith (*) veca vecb

norm :: V.Vector Float -> Float
norm = sqrt . V.sum . V.map (\e -> e * e)


findKNearestToWord :: Word2VecModel -> Int -> T.Text -> [(T.Text, Float)]
findKNearestToWord m k w = case getVector m w of
  Just v -> filter (\(t,_) -> t /= w) $ findKNearestToVector m (k+1) v
  Nothing -> []

findKNearestToVector :: Word2VecModel -> Int -> WVector -> [(T.Text, Float)]
findKNearestToVector m@(Word2VecModel _ _ h) k v = reverse $ catMaybes
                                                 $ foldl' step (replicate k Nothing)
                                                 $ map (\(w',v') -> (w', cosineSimilarity v' v)) $ DHS.toList h
  where step [] v = [Just v] -- not really needed, for completeness
        step l@(lowest:theRest) v = if isBetter v lowest
                                    then
                                      insertInto v theRest
                                    else
                                      l
        insertInto v [] = [Just v]
        insertInto v l@(lowest:theRest) = if isBetter v lowest
                                        then
                                          (lowest:insertInto v theRest)
                                        else
                                          (Just v:l)
        isBetter _ Nothing = True
        isBetter (_, s1) (Just (_, s2)) = s1 > s2

findNearestToWord :: Word2VecModel -> T.Text -> Maybe (T.Text, Float)
findNearestToWord m@(Word2VecModel _ _ h) w = findNearestToWord' h <$> (getVector m w)
   where findNearestToWord' h v = maximumBy (\(_,p) (_, q) -> p `compare` q)
                                  $ map (\(w',v') -> (w', cosineSimilarity v' v))
                                  $ filter (\(w',_) -> w' /= w) $ DHS.toList h
