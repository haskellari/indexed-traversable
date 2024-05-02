{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf)

import qualified Data.Foldable     as F
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map          as Map
import qualified Data.Sequence     as Seq
import qualified Data.Vector       as V

import Data.Foldable.WithIndex
import Data.Functor.WithIndex.Instances ()

main :: IO ()
main = defaultMain
  [ bgroup "vector"
    [ bgroup "itoList"
      [ bench "native"     $ nf (V.toList . V.indexed) v
      , bench "itoList"    $ nf itoList v
      ]
    ]
  , bgroup "sequence"
    [ bgroup "itoList"
      [ bench "native"     $ nf (F.toList . Seq.mapWithIndex (,)) s
      , bench "itoList"    $ nf itoList s
      ]
    ]
  , bgroup "list"
    [ bgroup "itoList"
      [ bench "native"     $ nf (zip [(0::Int)..]) l
      , bench "itoList"    $ nf itoList l
      ]
    ]
  , bgroup "map"
    [  bgroup "itoList"
      [ bench "native"     $ nf Map.toList m
      , bench "itoList"    $ nf itoList m
      ]
    ]
  , bgroup "hashmap"
    [ bgroup "itoList"
      [ bench "native"     $ nf HM.toList h
      , bench "itoList"    $ nf itoList h
      ]
    ]
  ]

l :: [Int]
l = [0..10000]
{-# NOINLINE l #-}

h :: HM.HashMap Int Int
h = HM.fromList $ zip l l
{-# NOINLINE h #-}

m :: Map.Map Int Int
m = Map.fromList $ zip l l
{-# NOINLINE m #-}

s :: Seq.Seq Int
s = Seq.fromList l
{-# NOINLINE s #-}

v :: V.Vector Int
v = V.fromList l
{-# NOINLINE v #-}
