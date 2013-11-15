module Clumps (
    KMer
  , clumps
  , kmers
  ) where


import qualified Data.Text as T
import Data.Maybe

type KMer = T.Text

-- Lemma: in any (L,t)-clump not started by the k-mer itself, there is an
--   (L,t)-clump that _is_ started by the k-mer.
-- Proof: Informally, L is a fixed-length sliding window. Any k-mer not at
-- the start that has t copies within L also has at least t copies within
-- L when it's the first k-mer in L.
--
-- This means that we can assume that every k-mer might be the start of an
-- (L,t)-clump, and it just comes down to checking whether that's true for
-- every k-mer we have on hand
clumps :: T.Text -> Int -> Int -> Int -> [KMer]
clumps s k l t =
    mapMaybe process_segment ls

  where
    ls :: [T.Text]
    ls = [ T.take l $ T.drop x s | x <- [0..(T.length s - l)] ]

    process_segment :: T.Text -> Maybe KMer
    process_segment l =
      let kmer = T.take k l in
      if T.count kmer l >= t then Just kmer else Nothing

-- This is naively O(n^2) due to the implementation of Data.Text
-- Here for reference only.
kmers :: T.Text -> Int -> [KMer]
kmers s k = [ T.take k $ T.drop x s | x <- [0..(T.length s - k)] ]
