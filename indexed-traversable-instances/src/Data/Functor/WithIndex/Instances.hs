{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy            #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds              #-}
#endif
-- | Extra instances for 'FunctorWithIndex', 'FoldableWithIndex',
-- and 'TraversableWithIndex' type classes.
module Data.Functor.WithIndex.Instances () where

import Prelude (Int, flip, ($), (.))

import Control.Applicative ((<$>))
import Data.HashMap.Lazy   (HashMap)
import Data.Tagged         (Tagged (..))
import Data.Vector         (Vector)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector       as V

import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance FunctorWithIndex () (Tagged a) where
  imap f (Tagged a) = Tagged (f () a)
  {-# INLINE imap #-}

instance FoldableWithIndex () (Tagged a) where
  ifoldMap f (Tagged a) = f () a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex () (Tagged a) where
  itraverse f (Tagged a) = Tagged <$> f () a
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance FunctorWithIndex Int Vector where
  imap = V.imap
  {-# INLINE imap #-}
instance FoldableWithIndex Int Vector where
  ifoldr = V.ifoldr
  {-# INLINE ifoldr #-}
  ifoldl = V.ifoldl . flip
  {-# INLINE ifoldl #-}
  ifoldr' = V.ifoldr'
  {-# INLINE ifoldr' #-}
  ifoldl' = V.ifoldl' . flip
  {-# INLINE ifoldl' #-}
instance TraversableWithIndex Int Vector where
  itraverse f v =
    let !n = V.length v in V.fromListN n <$> itraverse f (V.toList v)
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance FunctorWithIndex k (HashMap k) where
  imap = HM.mapWithKey
  {-# INLINE imap #-}
instance FoldableWithIndex k (HashMap k) where
  ifoldr  = HM.foldrWithKey
  ifoldl' = HM.foldlWithKey' . flip
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}
instance TraversableWithIndex k (HashMap k) where
  itraverse = HM.traverseWithKey
  {-# INLINE itraverse #-}
