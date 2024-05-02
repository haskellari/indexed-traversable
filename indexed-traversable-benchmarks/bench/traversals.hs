{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Map          as Map
import qualified Data.Sequence     as Seq
import qualified Data.Vector       as V

import Data.Functor.WithIndex           (imap)
import Data.Functor.WithIndex.Instances ()
import Data.Traversable.WithIndex       (imapDefault)

main :: IO ()
main = defaultMain
  [ bgroup "vector"
    [ bgroup "imap"
      [ bench "native"   $ nf (V.imap           (\i x -> x + i + 100)) v
      , bench "imap"     $ nf (imap             (\i x -> x + i + 100)) v
      , bench "default"  $ nf (imapDefault      (\i x -> x + i + 100)) v
      ]
    ]
  , bgroup "sequence"
    [  bgroup "imap"
      [ bench "native"   $ nf (Seq.mapWithIndex (\i x -> x + i + 100)) s
      , bench "imap"     $ nf (imap             (\i x -> x + i + 100)) s
      , bench "default"  $ nf (imapDefault      (\i x -> x + i + 100)) s
      ]
    ]
  , bgroup "list"
    [ bgroup "imap"
      [ bench "native"   $ nf (zipWith          (\i x -> x + i + 100) [0..]) l
      , bench "imap"     $ nf (imap             (\i x -> x + i + 100))       l
      , bench "default"  $ nf (imapDefault      (\i x -> x + i + 100))       l
      ]
    ]
  , bgroup "map"
    [ bgroup "imap"
      [ bench "native"   $ nf (Map.mapWithKey   (\i x -> x + i + 100)) m
      , bench "imap"     $ nf (imap             (\i x -> x + i + 100)) m
      , bench "default"  $ nf (imapDefault      (\i x -> x + i + 100)) m
      ]
    ]
  , bgroup "hashmap"
    [ bgroup "imap"
      [ bench "native"   $ nf (HM.mapWithKey    (\i x -> x + i + 100)) h
      , bench "imap"     $ nf (imap             (\i x -> x + i + 100)) h
      , bench "default"  $ nf (imapDefault      (\i x -> x + i + 100)) h
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
