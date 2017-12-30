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
  runConduitRes $ stdin .| CT.decodeUtf8Lenient .| CT.lines .|  C.map (solveAnalogy w2v)
                                                .| CT.encode CT.utf8 .| stdout

solveAnalogy :: Word2VecModel -> Text -> Text
solveAnalogy w2v line = T.unlines $ Prelude.map (format a1 a2 b1) similarOnes
  where [a1, a2, b1] = T.words line
        similarOnes = solveWordAnalogy w2v 30 a1 a2 b1
        format a1 a2 b1 (w', d) = T.intercalate "\t" [a1, a2, b1, w', (T.pack $ show d)]
