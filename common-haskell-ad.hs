{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Common_Haskell_AD where

import Numeric.AD
import qualified Numeric.AD.Mode.Forward
import Numeric.AD.Mode.Forward (Forward)
import qualified Numeric.AD.Mode.Reverse
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Tape, Reverse)
import qualified Numeric.AD.Internal.Reverse (primal)
import qualified Numeric.AD.Internal.Type (runAD)
import qualified Numeric.AD.Internal.Forward (primal)

-- *** FORWARD *** Specialized lift
lift_F :: Num a => a -> AD s (Forward a)
lift_F = auto
-- *** Reverse *** Specialized lift
lift_R :: (Num a, Reifies s Tape) => a -> Reverse s a
lift_R = auto

prim_F = Numeric.AD.Internal.Forward.primal . Numeric.AD.Internal.Type.runAD
-- prim_R = Numeric.AD.Internal.Reverse.primal . Numeric.AD.Internal.Type.runAD

-- *** FORWARD ***
derivative_F = Numeric.AD.Mode.Forward.diff
-- *** REVERSE ***
derivative_R = Numeric.AD.Mode.Reverse.diff

sqr x = x * x

vplus = zipWith (+)

vminus = zipWith (-)

ktimesv k = map (k *)

magnitude_squared x = foldl (+) 0 (map sqr x)

magnitude = sqrt . magnitude_squared

distance_squared u v = magnitude_squared (vminus u v)

distance u v = sqrt (distance_squared u v)

replace_ith (x : xs) 0 xi = (xi : xs)
replace_ith (x : xs) i xi = (x : (replace_ith xs (i - 1) xi))

-- *** FORWARD ***
gradient_F :: (Num a) => (forall s. [AD s (Forward a)] -> AD s (Forward a)) -> [a] -> [a]
gradient_F = Numeric.AD.Mode.Forward.grad
-- *** REVERSE ***
gradient_R :: (Num a) => (forall s. Reifies s Tape => [Reverse s a] -> Reverse s a) -> [a] -> [a]
gradient_R = Numeric.AD.Mode.Reverse.grad

-- *** FORWARD ***
lower_fs_F :: (Num a) => (forall s. [AD s (Forward a)] -> AD s (Forward a)) -> [a] -> a
lower_fs_F f = prim_F . f . map lift_F
-- *** Reverse ***
lower_fs_R :: (Num a) => (forall s. Reifies s Tape => [Reverse s a] -> Reverse s a) -> [a] -> a
-- lower_fs_R f = Numeric.AD.Internal.Reverse.primal . f . map lift_R
lower_fs_R f = fst . Numeric.AD.Mode.Reverse.grad' f


multivariate_argmin_F, multivariate_argmax_F ::
  (Num a, Fractional a, Ord a, Floating a)
  => (forall s. [AD s (Forward a)] -> AD s (Forward a)) -> [a] -> [a]

multivariate_max_F ::
  (Num a, Fractional a, Ord a, Floating a)
  => (forall s. [AD s (Forward a)] -> AD s (Forward a)) -> [a] -> a

multivariate_argmin_F f x =
    let
        loop x fx gx eta i =
            if (magnitude gx) <= 1e-5
            then x
            else if i == 10
                 then loop x fx gx (2 * eta) 0
                 else let x_prime = vminus x (ktimesv eta gx)
                      in if (distance x x_prime) <= 1e-5
                         then x
                         else let fx_prime = lower_fs_F f x_prime
                              in if fx_prime < fx
                                 then
                                 loop
                                 x_prime fx_prime (gradient_F f x_prime) eta       (i + 1)
                                 else
                                 loop
                                 x       fx       gx          (eta / 2) 0
    in loop x (lower_fs_F f x) (gradient_F f x) 1e-5 0

multivariate_argmax_F f x = multivariate_argmin_F (\ x -> - (f x)) x

multivariate_max_F f x = (lower_fs_F f) (multivariate_argmax_F f x)

multivariate_argmin_R, multivariate_argmax_R ::
  (Num a, Fractional a, Ord a, Floating a)
  => (forall s. Reifies s Tape => [Reverse s a] -> Reverse s a) -> [a] -> [a]

multivariate_max_R ::
  (Num a, Fractional a, Ord a, Floating a)
  => (forall s. Reifies s Tape => [Reverse s a] -> Reverse s a) -> [a] -> a

multivariate_argmin_R f x =
    let
        loop x fx gx eta i =
            if (magnitude gx) <= 1e-5
            then x
            else if i == 10
                 then loop x fx gx (2 * eta) 0
                 else let x_prime = vminus x (ktimesv eta gx)
                      in if (distance x x_prime) <= 1e-5
                         then x
                         else let fx_prime = lower_fs_R f x_prime
                              in if fx_prime < fx
                                 then
                                 loop
                                 x_prime fx_prime (gradient_R f x_prime) eta       (i + 1)
                                 else
                                 loop
                                 x       fx       gx          (eta / 2) 0
    in loop x (lower_fs_R f x) (gradient_R f x) 1e-5 0

multivariate_argmax_R f x = multivariate_argmin_R (\ x -> - (f x)) x

multivariate_max_R f x = (lower_fs_R f) (multivariate_argmax_R f x)

gradient_ascent_F ::
  (Num a, Fractional a, Ord a, Floating a)
  => (forall s. [AD s (Forward a)] -> AD s (Forward a)) -> [a] -> Int -> a -> ([a],a,[a])

gradient_ascent_F f x0 n eta =
    if n==0
    then (x0, (lower_fs_F f x0), (gradient_F f x0))
    else gradient_ascent_F
         f (vplus x0 (ktimesv eta (gradient_F f x0))) (n-1) eta

gradient_ascent_R ::
  (Num a, Fractional a, Ord a, Floating a)
  => (forall s. Reifies s Tape => [Reverse s a] -> Reverse s a) -> [a] -> Int -> a -> ([a],a,[a])

gradient_ascent_R f x0 n eta =
    if n==0
    then (x0, (lower_fs_R f x0), (gradient_R f x0))
    else gradient_ascent_R
         f (vplus x0 (ktimesv eta (gradient_R f x0))) (n-1) eta

{- *** This belongs in its own file, Crumple.hs *** -}

-- crumple and uncrumple -- reshapes elements of xs per leaves of template

{- module Crumple (crumple1, crumple2, uncrumple1, uncrumple2) where -}

-- user parser

crumple1 :: [[b]] -> [a] -> [[a]]
crumple1 template xs = tree
  where ([],tree) = crumple1_parse template xs

crumple2 :: [[[b]]] -> [a] -> [[[a]]]
crumple2 template xs = tree
  where ([],tree) = crumple2_parse template xs

-- Simple parser-like algorithm.  Traverse input stream (xs) spitting
-- out pair of remaining stream and substituted-into template.

crumple_parse subparser template xs = aux template xs
  where
    aux [] xs = (xs, [])
    aux (t:ts) xs = (xs2,(u1:u2))
      where
        (xs1,u1) = subparser t xs
        (xs2,u2) = aux ts xs1

crumple0_parse = crumple_parse (\_ (x:xs) -> (xs, x))
crumple1_parse = crumple_parse crumple0_parse
crumple2_parse = crumple_parse crumple1_parse

-- Flatten out.  Thing being flattened must match template.

uncrumple1 :: [[b]] -> [[a]] -> [a]
uncrumple1 = uncrumple1_unparse []
uncrumple2 :: [[[b]]] -> [[[a]]] -> [a]
uncrumple2 = uncrumple2_unparse []

uncrumple_unparse sub_uncrumple_unparse rest template tree = aux rest template tree
  where
    aux rest [] [] = rest
    aux rest (t:ts) (x:xs) =
      sub_uncrumple_unparse (aux rest ts xs) t x

uncrumple0_unparse = uncrumple_unparse (\rest ts xs -> xs:rest)
uncrumple1_unparse = uncrumple_unparse uncrumple0_unparse
uncrumple2_unparse = uncrumple_unparse uncrumple1_unparse
