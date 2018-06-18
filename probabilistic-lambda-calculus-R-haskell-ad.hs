import Common_Haskell_AD

data Value a = Number Int
             | TrueV | FalseV
             | Closure ([(a, [(Int, Value a)], Value a)] -> [(a, [(Int, Value a)], Value a)])

data Expression a = Constant (Value a)
                  | Variable Int
                  | Lambda (Int, Expression a)
                  | Application (Expression a, Expression a)

-- undecidable :: Exception

make_ignore = 0

make_if_procedure = 1

make_x0 = 2

make_x1 = 3

binds [] _ = False
binds ((variable1, _):environment) variable2 =
  variable1==variable2 || binds environment variable2

lookup_value variable1 ((variable2, value):environment) =
  if variable1==variable2 then value else lookup_value variable1 environment

same (Number m) (Number n) = m==n
same TrueV TrueV = True
same FalseV FalseV = True
same (Closure _) (Closure _) = error "undecidable" -- raise undecidable
same _ _ = False

merge_environments [] environment2 = Just environment2
merge_environments ((variable1, value1):environment1) environment2 =
  case merge_environments environment1 environment2 of
    Nothing -> Nothing
    Just environment ->
        if binds environment variable1
        then if same (lookup_value variable1 environment) value1
             then Just environment
             else Nothing
        else Just ((variable1, value1):environment)

singleton_tagged_distribution value = [(1.0, [], value)]

boolean_distribution p variable =
    [((1.0-p), [(variable, FalseV)], FalseV),
     (p,       [(variable, TrueV )], TrueV )]

normalize_tagged_distribution tagged_distribution =
  let n = let loop [] = 0.0
              loop ((p, _, _):tagged_distribution) =
                p+(loop tagged_distribution)
          in loop tagged_distribution
  in map (\ (p, environment, value) -> ((p/n), environment, value))
     tagged_distribution

remove_inconsistent_triples [] = []
remove_inconsistent_triples ((_, Nothing, _):tagged_distribution) =
    remove_inconsistent_triples tagged_distribution
remove_inconsistent_triples ((p, (Just environment), value):tagged_distribution) =
  ((p, environment, value):
   (remove_inconsistent_triples tagged_distribution))

map_tagged_distribution f tagged_distribution =
  normalize_tagged_distribution (
         let loop [] = []
             loop ((p, environment, value):tagged_distribution) =
                 (remove_inconsistent_triples
                      (map (\ (p1, environment1, value1) ->
                               ((p*p1),
                                (merge_environments environment environment1),
                                value1))
                        ((f value)))) ++
                 (loop tagged_distribution)
         in loop tagged_distribution )

evaluate (Constant value) environment =
  singleton_tagged_distribution value
evaluate (Variable variable) environment =
  lookup_value variable environment
evaluate (Lambda (variable, body)) environment =
  singleton_tagged_distribution
  (Closure (\ tagged_distribution ->
               evaluate body
               ((variable, tagged_distribution):environment)))
evaluate (Application (callee, argument)) environment =
  let tagged_distribution = evaluate argument environment
  in map_tagged_distribution (\ value ->
                                 case value of
                                   Closure f -> f tagged_distribution)
     (evaluate callee environment)

likelihood value [] = 0.0
likelihood value1 ((p, _, value2):tagged_distribution) =
  (if same value1 value2 then p else 0.0) +
  (likelihood value1 tagged_distribution)

make_if antecedent consequent alternate =
  (Application
    ((Application
      ((Application ((Variable make_if_procedure), antecedent)),
       (Lambda (make_ignore, consequent)))),
     (Lambda (make_ignore, alternate))))

example _ =
  (gradient_ascent_R
         (\ [p0, p1] ->
             let tagged_distribution =
                   evaluate
                         (make_if (Variable make_x0)
                                  (Constant (Number 0))
                                  (make_if (Variable make_x1)
                                           (Constant (Number 1))
                                           (Constant (Number 2))))
                         [(make_x0, (boolean_distribution p0 make_x0)),
                          (make_x1, (boolean_distribution p1 make_x1)),
                          (make_if_procedure,
                           (singleton_tagged_distribution
                             (Closure
                               (\ x ->
                                   (singleton_tagged_distribution
                                     (Closure
                                       (\ y ->
                                           (singleton_tagged_distribution
                                             (Closure
                                               (\ z ->
                                                   (map_tagged_distribution
                                                     (\ xe ->
                                                         (map_tagged_distribution
                                                           (\ ye ->
                                                               case ye of
                                                                 Closure yf ->
                                                                   (map_tagged_distribution
                                                                     (\ ze ->
                                                                         case ze of
                                                                           Closure zf ->
                                                                             case xe of
                                                                               Number _ ->  (yf (singleton_tagged_distribution FalseV))
                                                                               TrueV    ->  (yf (singleton_tagged_distribution FalseV))
                                                                               FalseV   ->  (zf (singleton_tagged_distribution FalseV))
                                                                               Closure _ -> (yf (singleton_tagged_distribution FalseV)))
                                                                     z))
                                                           y))
                                                     x)))))))))))]
             in foldl (*)
                1.0
                (map (\ observation ->
                         likelihood observation tagged_distribution)
                  [(Number 0), (Number 1), (Number 2), (Number 2)]))
         [0.5, 0.5]
         1000
         0.1)

run =
  let loop i result =
        if i==0
        then result
        else let ([p0, p1], _, _) = example ()
             in loop (i-1) [p0, p1]
  in loop 1000 [0.0, 0.0]

main = print run
