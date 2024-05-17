{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Indexed Foldables.
module Data.Foldable.WithIndex (
    -- * Indexed Foldables
    FoldableWithIndex (..),
    -- ** Indexed Foldable Combinators
    iany,
    iall,
    inone, none,
    itraverse_,
    ifor_,
    imapM_,
    iforM_,
    iconcatMap,
    ifind,
    ifoldrM,
    ifoldlM,
    itoList,
) where


import Prelude (Bool, Maybe (..), Monad (..), flip, not, (.), curry)

import Control.Applicative (Applicative (..))
import Control.Monad       (liftM, void)
import Data.Foldable       (Foldable, any)
import Data.Monoid         (All (..), Any (..))

import CoerceCompat ((#.), (#..))
import GhcList      (build)

import WithIndex

-- | Return whether or not any element in a container satisfies a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'any' is more flexible in what it accepts.
--
-- @
-- 'any' ≡ 'iany' '.' 'Data.Function.const'
-- @
iany :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Bool
iany f = getAny #. ifoldMap (Any #.. f)
{-# INLINE iany #-}

-- | Return whether or not all elements in a container satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'Data.Foldable.all' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.all' ≡ 'iall' '.' 'Data.Function.const'
-- @
iall :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Bool
iall f = getAll #. ifoldMap (All #.. f)
{-# INLINE iall #-}

-- | Return whether or not none of the elements in a container satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'none' is more flexible in what it accepts.
--
-- @
-- 'none' ≡ 'inone' '.' 'Data.Function.const'
-- 'inone' f ≡ 'not' '.' 'iany' f
-- @
inone :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Bool
inone f = not . iany f
{-# INLINE inone #-}

-- | Determines whether no elements of the structure satisfy the predicate.
--
-- @
-- 'none' f ≡ 'not' '.' 'any' f
-- @
none :: Foldable f => (a -> Bool) -> f a -> Bool
none f = not . any f
{-# INLINE none #-}

-- | Traverse elements with access to the index @i@, discarding the results.
--
-- When you don't need access to the index then 'Data.Foldable.traverse_' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.traverse_' l = 'itraverse' '.' 'Data.Function.const'
-- @
itraverse_ :: (FoldableWithIndex i t, Applicative f) => (i -> a -> f b) -> t a -> f ()
itraverse_ f = void . getTraversed #. ifoldMap (Traversed #.. f)
{-# INLINE itraverse_ #-}

-- | Traverse elements with access to the index @i@, discarding the results (with the arguments flipped).
--
-- @
-- 'ifor_' ≡ 'flip' 'itraverse_'
-- @
--
-- When you don't need access to the index then 'Data.Foldable.for_' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.for_' a ≡ 'ifor_' a '.' 'Data.Function.const'
-- @
ifor_ :: (FoldableWithIndex i t, Applicative f) => t a -> (i -> a -> f b) -> f ()
ifor_ = flip itraverse_
{-# INLINE ifor_ #-}

-- | Run monadic actions for each target of an 'ControlLens.Fold.IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'Data.Foldable.mapM_' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.mapM_' ≡ 'imapM_' '.' 'Data.Function.const'
-- @
imapM_ :: (FoldableWithIndex i t, Monad m) => (i -> a -> m b) -> t a -> m ()
imapM_ f = liftM skip . getSequenced #. ifoldMap (Sequenced #.. f)
{-# INLINE imapM_ #-}

-- | Run monadic actions for each target of an 'Control.Lens.Fold.IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with
-- access to the index,
-- discarding the results (with the arguments flipped).
--
-- @
-- 'iforM_' ≡ 'flip' 'imapM_'
-- @
--
-- When you don't need access to the index then 'Control.Monad.forM_' is more flexible in what it accepts.
--
-- @
-- 'Control.Monad.forM_' a ≡ 'iforM_' a '.' 'Data.Function.const'
-- @
iforM_ :: (FoldableWithIndex i t, Monad m) => t a -> (i -> a -> m b) -> m ()
iforM_ = flip imapM_
{-# INLINE iforM_ #-}

-- | Concatenate the results of a function of the elements of an indexed container with access to the index.
--
-- When you don't need access to the index then 'Data.Foldable.concatMap' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.concatMap' ≡ 'iconcatMap' '.' 'Data.Function.const'
-- 'iconcatMap' ≡ 'ifoldMap'
-- @
iconcatMap :: FoldableWithIndex i f => (i -> a -> [b]) -> f a -> [b]
iconcatMap = ifoldMap
{-# INLINE iconcatMap #-}

-- | Searches a container with a predicate that is also supplied the index, returning the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'Data.Foldable.find' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.find' ≡ 'ifind' '.' 'Data.Function.const'
-- @
ifind :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Maybe (i, a)
ifind p = ifoldr (\i a y -> if p i a then Just (i, a) else y) Nothing
{-# INLINE ifind #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'Data.Foldable.foldrM' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.foldrM' ≡ 'ifoldrM' '.' 'Data.Function.const'
-- @
ifoldrM :: (FoldableWithIndex i f, Monad m) => (i -> a -> b -> m b) -> b -> f a -> m b
ifoldrM f z0 xs = ifoldl f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrM #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'Data.Foldable.foldlM' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.foldlM' ≡ 'ifoldlM' '.' 'Data.Function.const'
-- @
ifoldlM :: (FoldableWithIndex i f, Monad m) => (i -> b -> a -> m b) -> b -> f a -> m b
ifoldlM f z0 xs = ifoldr f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlM #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'Data.Foldable.toList' is more flexible in what it accepts.
--
-- @
-- 'Data.Foldable.toList' ≡ 'Data.List.map' 'Data.Tuple.snd' '.' 'itoList'
-- @
itoList :: FoldableWithIndex i f => f a -> [(i,a)]
itoList xs = build (\c n -> ifoldr (curry c) n xs)
{-# INLINE itoList #-}
