{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Common_Haskell_AD
-- import Crumple (crumple2, uncrumple2)
import Numeric.AD (AD)
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Tape, Reverse)

sum_activities activities (bias:ws) =
  foldl (+) bias (zipWith (*) ws activities)

sum_layer activities ws_layer = map (sum_activities activities) ws_layer

sigmoid x = 1/(exp (0-x) + 1)

forward_pass [] in1 = in1
forward_pass (ws_layer:ws_layers) in1 =
  forward_pass ws_layers (map sigmoid (sum_layer in1 ws_layer))

error_on_dataset dataset ws_layers =
  foldl (+)
  0
  (map (\ (in1, target) ->
           0.5 *
           (magnitude_squared
             (vminus (forward_pass ws_layers in1) target)))
    dataset)

s_kstar ws k y =
  zipWith (\ l y ->
              zipWith (\ u y ->
                          zipWith (\ w y -> w-k*y)
                          u y)
              l y)
  ws y

weight_gradient ::
  Num a =>
  (forall s. Reifies s Tape => [[[Reverse s a]]] -> Reverse s a)
  -> [[[a]]] -> [[[a]]]

weight_gradient f ws =
  crumple2 ws $ gradient_R (f . crumple2 ws) (uncrumple2 ws ws)

vanilla ::
  Num a =>
  (forall s. Reifies s Tape => [[[Reverse s a]]] -> Reverse s a)
  -> [[[a]]] -> Int -> a -> a

vanilla f w0 n eta =
    if n==0
    then lower_fs_R (f . crumple2 w0) (uncrumple2 w0 w0)
    else vanilla f (s_kstar w0 eta (weight_gradient f w0)) (n-1) eta

xor_ws0 = [[[0, -0.284227, 1.16054],
            [0, 0.617194, 1.30467]],
           [[0, -0.084395, 0.648461]]]

xor_data = [([0, 0], [0]),
            ([0, 1], [1]),
            ([1, 0], [1]),
            ([1, 1], [0])]

run = vanilla (error_on_dataset xor_data)
      xor_ws0
      1000000
      0.3

main = print run
