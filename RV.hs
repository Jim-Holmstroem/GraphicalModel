{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}  -- FIXME possibly discouraged
{-# LANGUAGE PolyKinds #-}  -- FIXME possibly discouraged
import Control.Monad
import Control.Monad.Primitive (PrimMonad,PrimState)
import System.Random.MWC (Gen,uniform,createSystemRandom)

import Statistics.Distribution (Distribution,ContDistr,genContinous)
import Statistics.Distribution.Uniform (UniformDistribution,uniformDistr)
import Statistics.Distribution.Normal (NormalDistribution,normalDistr)

import Graphics.Histogram

plotRV :: (RV rv) => Int -> rv -> IO ()
plotRV count rv = do
    random <- createSystemRandom
    samples <- replicateM count $ sample rv random
    plot "" $ histogram binSturges samples
    return ()

-- instance Functor RV ?
--
-- instance Num RV ? to be able to use + and * etc
--
-- Applicative RV
-- (+) <$> u01 <*> u01
--
-- RV returning Integers
--
-- Sum_{i=1..n} X_i where n \in N
--
-- How many flips until I reach N (just another distribution)
--
-- Distributions with RV as parameter
--
--
-- What is X < Y

class RV rv where
    sample :: (PrimMonad m) => rv -> (Gen (PrimState m)) -> m Double

instance RV UniformDistribution where
    sample d random = genContinous d random

instance RV NormalDistribution where
    sample d random = genContinous d random


-- data RV = Add | Sub | Mul ... instead ?
data Add a b = Add a b deriving (Show)
data Sub a b = Sub a b deriving (Show)
data Mul a b = Mul a b deriving (Show)
data Div a b = Div a b deriving (Show)
data Pow a b = Pow a b deriving (Show)
data Neg a = Neg a deriving (Show)
data Apply a = Apply (Double -> Double) a
data Condition a = Condition (Double -> Bool) a
data BCondition a b = BCondition (Double -> Double -> Bool) a b


(.+) :: (RV rva, RV rvb) => rva -> rvb -> Add rva rvb
(.+) = Add

(.-) :: (RV rva, RV rvb) => rva -> rvb -> Sub rva rvb
(.-) = Sub

(.*) :: (RV rva, RV rvb) => rva -> rvb -> Mul rva rvb
(.*) = Mul

(./) :: (RV rva, RV rvb) => rva -> rvb -> Div rva rvb
(./) = Div

(.**) :: (RV rva, RV rvb) => rva -> rvb -> Pow rva rvb
(.**) = Pow

(.$) :: (RV rv) => (Double -> Double) -> rv -> Apply rv
(.$) = Apply

-- X < Y "returns" the X, Y can be a constant (X as well, but it means it will return the constant all the time)
-- what should be the natural "return value" of this
(.<) :: (RV rva, RV rvb) => rva -> rvb -> BCondition rva rvb
(.<) = BCondition (<)
(.>) :: (RV rva, RV rvb) => rva -> rvb -> BCondition rva rvb
(.>) = BCondition (>)

--(.<) :: (RV rv) => Double -> rv -> Condition rv
--a .< b = Condition (>a) b


--z :: (PrimMonad m, RV rv) => (Double -> Bool) -> rv -> Gen (PrimState m) -> m Double
--z c random = do
--    a' <-

-- FIXME alot of copy paste


-- FIXME decide on RV a or RV rva


instance (RV a, RV b) => RV (BCondition a b) where
    sample bcond@(BCondition c a b) random = do
        a' <- sample a random
        b' <- sample b random
        if c a' b' then return a' else sample bcond random


instance (RV a) => RV (Condition a) where
    sample cond@(Condition c a) random = do
        a' <- sample a random
        if c a' then return a' else sample cond random

instance (RV a) => RV (Apply a) where
    sample (Apply f a) random = do
        a' <- sample a random
        return $ f a'

instance (RV a, RV b) => RV (Add a b) where
    sample (Add a b) random = do
        a' <- sample a random
        b' <- sample b random
        return $ a' + b'

instance (RV a, RV b) => RV (Sub a b) where
    sample (Sub a b) random = do
        a' <- sample a random
        b' <- sample b random
        return $ a' - b'

instance (RV a, RV b) => RV (Mul a b) where
    sample (Mul a b) random = do
        a' <- sample a random
        b' <- sample b random
        return $ a' * b'

instance (RV a, RV b) => RV (Div a b) where
    sample (Div a b) random = do
        a' <- sample a random
        b' <- sample b random
        return $ a' / b'

instance (RV a, RV b) => RV (Pow a b) where
    sample (Pow a b) random = do
        a' <- sample a random
        b' <- sample b random
        return $ a' ** b'

instance (RV a) => RV (Neg a) where
    sample (Neg a) random = do
        a' <- sample a random
        return $ (-a')


instance RV Double where
    sample d _ = return d


main = print "hello"
