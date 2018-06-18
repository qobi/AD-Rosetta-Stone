import Common_Haskell_AD

data Term a = Constant Int | Variable a | Compound Int [Term a]
  deriving (Eq, Show)

make_p = 0

make_q = 1

make_x = 2.0

same (Variable variable1) (Variable variable2) =
    variable1<=variable2 && variable2<=variable1

binds [] _ = False
binds ((variable1, _):substitution) variable2 =
  if same variable1 variable2 then True else binds substitution variable2

lookup_value variable1 ((variable2, value):substitution) =
  if same variable1 variable2
  then value
  else lookup_value variable1 substitution

apply_substitution substitution (term@(Constant _)) = term
apply_substitution substitution (term@(Variable _)) =
    if binds substitution term then lookup_value term substitution else term
apply_substitution substitution (Compound function terms) =
    Compound function (map (\ term -> apply_substitution substitution term)
                           terms)

apply_substitution_to_clause substitution (p, term, terms) =
    (p,
     (apply_substitution substitution term),
     (map (\ term -> apply_substitution substitution term) terms))

member variable [] = False
member variable1 (variable2:set) =
  -- if same variable1 variable2 then True else member variable1 set
  same variable1 variable2 || member variable1 set

union [] set = set
union (element:set1) set2 =
    if member element set2
    then union set1 set2
    else element:union set1 set2

variables_in (Constant _) = []
variables_in (term@(Variable _)) = [term]
variables_in (Compound function terms) =
  foldl union [] (map variables_in terms)

variables_in_clause (p, term, terms) =
  union (variables_in term) (foldl union [] (map variables_in terms))

occurs variable term = member variable (variables_in term)

unify (Constant constant1) (Constant constant2) =
  if constant1==constant2 then Just [] else Nothing
unify (term1@(Constant _)) (term2@(Variable _)) =
  Just [(term2, term1)]
unify (Constant _) (Compound _ _) = Nothing
unify (term1@(Variable _)) (term2@(Constant _)) =
  Just [(term1, term2)]
unify (term1@(Variable _)) (term2@(Variable _)) =
  Just [(term1, term2)]
unify (term1@(Variable _)) (term2@(Compound _ _)) =
  if occurs term1 term2 then Nothing else Just [(term1, term2)]
unify (Compound _ _) (Constant _) = Nothing
unify (term1@(Compound _ _)) (term2@(Variable _)) =
  if occurs term2 term1 then Nothing else Just [(term2, term1)]
unify (Compound function1 terms1) (Compound function2 terms2) =
  if function1==function2
  then let loop [] [] substitution = Just substitution
           loop (term1:terms1) (term2:terms2) substitution =
             case unify (apply_substitution substitution term1)
                  (apply_substitution substitution term2) of
               Nothing -> Nothing
               Just substitution1 ->
                 loop terms1 terms2 (substitution1++substitution)
           loop _ _ _ = Nothing
       in loop terms1 terms2 []
  else Nothing

map_indexed f l =
    let loop [] i = []
        loop (h:t) i = f h i:loop t (i+1.0)
    in loop l 0.0

alpha_rename clause offset =
    apply_substitution_to_clause
        (map_indexed
             (\ variable -> (\ i -> (variable, (Variable (offset+i)))))
             (variables_in_clause clause))
        clause

proof_distribution term clauses =
    let offset = foldl (\ x y -> if x<=y then y else x)
                 0.0
                 (map (\ (Variable variable) -> variable)
                   (foldl union [] (map variables_in_clause clauses)))
    in foldl (++)
             []
             (map (\ clause ->
                      let (p, term1, terms) = alpha_rename clause offset
                      in let loop p substitution terms =
                                 case substitution of
                                   Nothing -> []
                                   Just substitution ->
                                     case terms of
                                         [] -> [(p, substitution)]
                                         (term:terms) ->
                                           foldl (++) []
                                           (map (\ (p1, substitution1) ->
                                                    loop (p*p1)
                                                    (Just (substitution++substitution1))
                                                    terms)
                                             (proof_distribution
                                               (apply_substitution substitution term)
                                               clauses))
                         in loop p (unify term term1) terms)
                  clauses)

likelihood substitution_distribution =
    foldl (+) 0.0 (map (\ (p, _) -> p) substitution_distribution)

example _ =
    gradient_ascent_F
         (\ [p0, p1] ->
             let clauses = [(p0, (Compound make_p [(Constant 0)]), []),
                            ((1.0-p0),
                             (Compound make_p [(Variable make_x)]),
                             [(Compound make_q [(Variable make_x)])]),
                            (p1, (Compound make_q [(Constant 1)]), []),
                            ((1.0-p1),
                             (Compound make_q [(Constant 2)]),
                             [])]
             in foldl (*)
                      1.0
                      (map (\ observation ->
                               likelihood
                                   (proof_distribution
                                        (Compound make_p
                                                   [(Constant observation)])
                                        clauses))
                           [0, 1, 2, 2]))
         [0.5, 0.5]
         1000
         0.1

run =
  map (\_ -> fst3 $ example ()) [1..1000]
  where fst3 (a,_,_) = a

main = mapM_ print run
