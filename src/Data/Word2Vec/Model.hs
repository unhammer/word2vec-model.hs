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
-- Reading word2vec binary models (generated with the original tool by Mikolov).
--
-- This simple module is only for /reading/ word2vec models (it cannot be used
-- to /generate/ a word2vec model, for this the original word2vec tools should be used).
--
-- Note that word2vec binary format is not a proper serialisation format (as it is mostly
-- a raw dump of C data. /Caveat emptor/, it might be risky to read a model generated
-- on a host with a different architecture.
--
-- Example:
--
-- @
--   model <- readWord2VecModel "binary.bin"
--   let theMostSimilar = findKNearestToWord model 30 "bar"
-- @
--
-----------------------------------------------------------------------------

module Data.Word2Vec.Model
    (
      -- * Main data structure
      Word2VecModel
    , empty

      -- * Basic operations
    , readWord2VecModel

    , numberOfWords
    , numberOfDimensions

      -- * Operations on vectors
    , WVector(..)
    , zero
    , vadd
    , vsubtract
    , buildWVector
    , getVector
    , normalizeVector

      -- * Looking for the nearest vector
      --
      -- (Like the distance/word-analogy tools in the original word2vec.)
    , findNearestToWord
    , findKNearestToWord
    , findKNearestToVector
    , solveWordAnalogy

      -- * Helper functions
    , cosineSimilarity
    , dotProduct
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

-- | Vector stored in a word2vec model with its norm (to speed up
-- calculating cosine similarities).
data WVector = WVector (V.Vector Float) -- vector itself
                       Float            -- norm
               deriving (Eq, Show)

-- | Word2vec Model
data Word2VecModel = Word2VecModel Int -- number of words
                                   Int -- number of dimensions
                                   !(DHS.HashMap T.Text WVector) -- word-to-vector map
                     deriving (Eq, Show)

-- | /O(1)/ Construct an empty model.
empty :: Word2VecModel
empty = Word2VecModel 0 0 mempty

-- | Main function, reading a word2vec binary model into memory.
readWord2VecModel :: FilePath -> IO (Word2VecModel)
readWord2VecModel fileName = do
  contents <- BL.readFile fileName
  pure $ processWord2VecBinaryModel contents

-- | Get the number of words in the model.
numberOfWords :: Word2VecModel -> Int
numberOfWords (Word2VecModel n _ _) = n

-- | Get the number of dimensions.
numberOfDimensions :: Word2VecModel -> Int
numberOfDimensions (Word2VecModel _ d _) = d

-- | /(Practically) O(1)/ Get a vector for a given word.
getVector :: Word2VecModel -> T.Text -> Maybe WVector
getVector (Word2VecModel _ _ h) w = DHS.lookup w h

-- | Construct a word vector with all zeros, of the right dimension for this model.
--
-- Useful as identity when summing word vectors, e.g. 'foldr vadd zero someVectors'
zero :: Word2VecModel -> WVector
zero (Word2VecModel _ dim _) = buildWVector (V.replicate dim 0)

processWord2VecBinaryModel :: BL.ByteString -> Word2VecModel
processWord2VecBinaryModel contents =
  case AL.parse parseWord2VecBinaryModel contents of
    AL.Fail _ _ _ -> error "does not look like a word2vec binary model"
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

-- see https://stackoverflow.com/questions/20912582/haskell-bytestring-to-float-array
bytesToFloats :: BS.ByteString -> V.Vector Float
bytesToFloats = V.unsafeCast . aux . BS.toForeignPtr
  where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len

-- | Build a word2vec vector from a raw float vector
--
-- (Just calculates its norm.)
buildWVector :: V.Vector Float -> WVector
buildWVector v = WVector v (norm v)

-- | Normalise a vector with its norm
normalizeVector :: WVector -> WVector
normalizeVector (WVector v n) = buildWVector (V.map (/ n) v)

-- | Calculate cosine similarity between two word2vec vectors.
--
-- Note that it was called wrongly /cosine distance/ in the original word2vec.
cosineSimilarity :: WVector -> WVector -> Float
cosineSimilarity (WVector veca norma) (WVector vecb normb) = (dotProduct veca vecb) / (norma * normb)

-- | Calculate dot product between two word2vec vectors
dotProduct :: V.Vector Float -> V.Vector Float -> Float
dotProduct veca vecb = V.sum $ V.zipWith (*) veca vecb

norm :: V.Vector Float -> Float
norm = sqrt . V.sum . V.map (\e -> e * e)

-- | /O(n) where n is the number of words, assuming k is small/ Find
-- the top-k most similar words for a given word. (The queried word is
-- excluded.)
findKNearestToWord :: Word2VecModel -> Int -> T.Text -> [(T.Text, Float)]
findKNearestToWord m k w = case getVector m w of
  Just v -> filter (\(t,_) -> t /= w) $ findKNearestToVector m (k+1) v
  Nothing -> []

-- | /O(n) where n is the number of words, assuming k is small/ Find
-- the top-k most similar words for a given vector.
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

-- | /O(n) where n is the number of words/ Find the word which is the
-- most similar to a given word. (The queried word is excluded.)
findNearestToWord :: Word2VecModel -> T.Text -> Maybe (T.Text, Float)
findNearestToWord m@(Word2VecModel _ _ h) w = findNearestToWord' h <$> (getVector m w)
   where findNearestToWord' h v = maximumBy (\(_,p) (_, q) -> p `compare` q)
                                  $ map (\(w',v') -> (w', cosineSimilarity v' v))
                                  $ filter (\(w',_) -> w' /= w) $ DHS.toList h

solveWordAnalogy :: Word2VecModel -> Int -> T.Text -> T.Text -> T.Text -> [(T.Text, Float)]
solveWordAnalogy m k a1 a2 b1 = case targetVector of
  Just v -> findKNearestToVector m k v
  Nothing -> []
  where targetVector = getVectorByAnalogy <$>
                                (getVector m a1) <*> (getVector m a2) <*> (getVector m b1)
        getVectorByAnalogy v1 v2 u1 = vadd (normalizeVector u1) (vsubtract (normalizeVector v2)
                                                                           (normalizeVector v1))

pointWiseOperation :: (Float -> Float -> Float) -> WVector -> WVector -> WVector
pointWiseOperation fun (WVector v1 _) (WVector v2 _) =
  buildWVector $ V.zipWith fun v1 v2

vadd :: WVector -> WVector -> WVector
vadd = pointWiseOperation (+)

vsubtract :: WVector -> WVector -> WVector
vsubtract = pointWiseOperation (-)
