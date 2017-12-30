{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Word2Vec.Model
import Data.Text as T

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Data.Conduit
import Data.Conduit.Combinators as C

import System.Environment

main :: IO ()
main = do
  (arg:_) <- getArgs
  w2v <- readWord2VecModel arg
  runConduitRes $ stdin .| CT.decodeUtf8Lenient .| CT.lines .|  C.map (getSimilarOnes w2v)
                                                .| CT.encode CT.utf8 .| stdout

getSimilarOnes :: Word2VecModel -> Text -> Text
getSimilarOnes w2v w = T.unlines $ Prelude.map (format w) similarOnes
  where similarOnes = findKNearestToWord w2v 30 w
        format w (w', d) = T.intercalate "\t" [w, w', (T.pack $ show d)]
