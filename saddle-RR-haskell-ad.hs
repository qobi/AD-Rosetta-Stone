import Common_Haskell_AD

run = let start = [1, 1]
          f x1 y1 x2 y2 = ((sqr x1) + (sqr y1)) - ((sqr x2) + (sqr y2))
          [x1_star, y1_star] =
              multivariate_argmin_R
              (\ [x1, y1] ->
                   multivariate_max_R
                   (\ [x2, y2] -> f (lift_R x1) (lift_R y1) x2 y2)
                   (map lift_R start))
              start
          [x2_star, y2_star] =
              multivariate_argmax_R
              (\ [x2, y2] -> f (lift_R x1_star) (lift_R y1_star) x2 y2)
              start
       in [[x1_star, y1_star], [x2_star, y2_star]]

main = print run
