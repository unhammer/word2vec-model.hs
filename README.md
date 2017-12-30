# word2vec-model

Reading word2vec binary models (generated with the original tool by Mikolov).

This simple module is only for *reading* word2vec models (it cannot be used
to *generate* a word2vec model, for this the original word2vec tools should be used).

Note that word2vec binary format is not a proper serialisation format (as it is mostly
a raw dump of C data. *Caveat emptor*, it might be risky to read a model generated
on a host with a different architecture.

Example:

    {-# LANGUAGE OverloadedStrings #-}
    model <- readWord2VecModel "binary.bin"
    let theMostSimilar = findKNearestToWord w2v 30 "polska"
