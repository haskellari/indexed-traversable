{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Main (main) where

import Data.Monoid      (Sum(..))
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)

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
    , bench "ifoldMap ([])"     $ nf (ifoldMap (\i n -> [(i, n)])) v
    , bench "ifoldMap (vector)" $ nf (ifoldMap (\i n -> V.singleton (i, n))) v
    , bench "ifoldMap' (sum)"   $ nf (ifoldMap' (\i n -> Sum $ i + n)) v
    , bench "ifoldr"            $ nf (ifoldr (\i n acc -> (i, n) : acc) []) v
    , bench "ifoldl"            $ nf (ifoldl (\i acc n -> (i, n) : acc) []) v
    , bench "ifoldr'"           $ nf (ifoldr' (\i n acc -> i + n + acc) 0) v
    , bench "ifoldl'"           $ nf (ifoldl' (\i n acc -> i + n + acc) 0) v
    ]
  , bgroup "sequence"
    [ bgroup "itoList"
      [ bench "native"     $ nf (F.toList . Seq.mapWithIndex (,)) s
      , bench "itoList"    $ nf itoList s
      ]
    , bench "ifoldMap ([])"     $ nf (ifoldMap (\i n -> [(i, n)])) s
    , bench "ifoldMap (vector)" $ nf (ifoldMap (\i n -> V.singleton (i, n))) s
    , bench "ifoldMap' (sum)"   $ nf (ifoldMap' (\i n -> Sum $ i + n)) s
    , bench "ifoldr"            $ nf (ifoldr (\i n acc -> (i, n) : acc) []) s
    , bench "ifoldl"            $ nf (ifoldl (\i acc n -> (i, n) : acc) []) s
    , bench "ifoldr'"           $ nf (ifoldr' (\i n acc -> i + n + acc) 0) s
    , bench "ifoldl'"           $ nf (ifoldl' (\i n acc -> i + n + acc) 0) s
    ]
  , bgroup "list"
    [ bgroup "itoList"
      [ bench "native"     $ nf (zip [(0::Int)..]) l
      , bench "itoList"    $ nf itoList l
      ]
    , bench "ifoldMap ([])"     $ nf (ifoldMap (\i n -> [(i, n)])) l
    , bench "ifoldMap (vector)" $ nf (ifoldMap (\i n -> V.singleton (i, n))) l
    , bench "ifoldMap' (sum)"   $ nf (ifoldMap' (\i n -> Sum $ i + n)) l
    , bench "ifoldr"            $ nf (ifoldr (\i n acc -> (i, n) : acc) []) l
    , bench "ifoldl"            $ nf (ifoldl (\i acc n -> (i, n) : acc) []) l
    , bench "ifoldr'"           $ nf (ifoldr' (\i n acc -> i + n + acc) 0) l
    , bench "ifoldl'"           $ nf (ifoldl' (\i n acc -> i + n + acc) 0) l
    ]
  , bgroup "map"
    [  bgroup "itoList"
      [ bench "native"     $ nf Map.toList m
      , bench "itoList"    $ nf itoList m
      ]
    , bench "ifoldMap ([])"     $ nf (ifoldMap (\i n -> [(i, n)])) m
    , bench "ifoldMap (vector)" $ nf (ifoldMap (\i n -> V.singleton (i, n))) m
    , bench "ifoldMap' (sum)"   $ nf (ifoldMap' (\i n -> Sum $ i + n)) m
    , bench "ifoldr"            $ nf (ifoldr (\i n acc -> (i, n) : acc) []) m
    , bench "ifoldl"            $ nf (ifoldl (\i acc n -> (i, n) : acc) []) m
    , bench "ifoldr'"           $ nf (ifoldr' (\i n acc -> i + n + acc) 0) m
    , bench "ifoldl'"           $ nf (ifoldl' (\i n acc -> i + n + acc) 0) m
    ]
  , bgroup "hashmap"
    [ bgroup "itoList"
      [ bench "native"     $ nf HM.toList h
      , bench "itoList"    $ nf itoList h
      ]
    , bench "ifoldMap ([])"     $ nf (ifoldMap (\i n -> [(i, n)])) h
    , bench "ifoldMap (vector)" $ nf (ifoldMap (\i n -> V.singleton (i, n))) h
    , bench "ifoldMap' (sum)"   $ nf (ifoldMap' (\i n -> Sum $ i + n)) h
    , bench "ifoldr"            $ nf (ifoldr (\i n acc -> (i, n) : acc) []) h
    , bench "ifoldl"            $ nf (ifoldl (\i acc n -> (i, n) : acc) []) h
    , bench "ifoldr'"           $ nf (ifoldr' (\i n acc -> i + n + acc) 0) h
    , bench "ifoldl'"           $ nf (ifoldl' (\i n acc -> i + n + acc) 0) h
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
