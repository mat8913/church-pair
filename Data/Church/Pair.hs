{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Church.Pair
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : BSD3
-- Maintainer  : mbekkema97@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- The church encoded pair type and associated operations.
--
-----------------------------------------------------------------------------

module Data.Church.Pair
( Pair(Pair)
, mkPair
, pair
) where

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Semigroup (Semigroup ((<>), stimes))
import Data.Functor.Classes (Eq2 (liftEq2), Eq1 (liftEq), Ord2 (liftCompare2),
                             Ord1 (liftCompare), eq2, compare2)


-- | The church encoded pair
newtype Pair a b = Pair { runPair :: forall r. (a -> b -> r) -> r }

instance Bifunctor Pair where
    bimap fx fy (Pair p) = Pair $ \r -> p $ \x y -> r (fx x) (fy y)

instance Functor (Pair a) where
    fmap = second

instance Eq2 Pair where
    liftEq2 f1 f2 (Pair px) (Pair py) = px $ \x1 x2 -> py $ \y1 y2 ->
        f1 x1 y1 && f2 x2 y2

instance Ord2 Pair where
    liftCompare2 f1 f2 (Pair px) (Pair py) = px $ \x1 x2 -> py $ \y1 y2 ->
        case f1 x1 y1 of
            EQ -> f2 x2 y2
            oo -> oo

instance Eq a => Eq1 (Pair a) where
    liftEq = liftEq2 (==)

instance Ord a => Ord1 (Pair a) where
    liftCompare = liftCompare2 compare

instance (Eq a, Eq b) => Eq (Pair a b) where
    (==) = eq2

instance (Ord a, Ord b) => Ord (Pair a b) where
    compare = compare2

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
    (<>) = liftMappend2 (<>) (<>)
    stimes n (Pair p) = Pair $ \r -> p $ \x y -> r (stimes n x) (stimes n y)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = mkPair mempty mempty
    mappend = liftMappend2 mappend mappend

instance Monoid a => Applicative (Pair a) where
    pure = mkPair mempty
    Pair pf <*> Pair px = Pair $ \r -> pf $ \u f -> px $ \v x ->
        r (mappend u v) (f x)

instance Foldable (Pair a) where
    foldr f z (Pair p) = p $ \_ y -> f y z
    foldMap f (Pair p) = p $ \_ y -> f y

instance Traversable (Pair a) where
    traverse f (Pair p) = p $ \x y -> mkPair x <$> f y

instance Monoid a => Monad (Pair a) where
    return = pure
    (Pair p) >>= k = Pair $ \r -> p $ \u a -> runPair (k a) $ r . mappend u


liftMappend2 :: (a -> a -> a)
             -> (b -> b -> b)
             -> (Pair a b -> Pair a b -> Pair a b)
liftMappend2 f1 f2 (Pair px) (Pair py) = Pair $ \r -> px $ \x1 x2 -> py $ \y1 y2 ->
    r (f1 x1 y1) (f2 x2 y2)

-- | Case analysis on `Pair`
pair :: (a -> b -> r) -> Pair a b -> r
pair = flip runPair

-- | Construct a `Pair` from two values
mkPair :: a -> b -> Pair a b
mkPair x y = Pair $ \r -> r x y
