from common_pytorch import gradient_ascent_R

def make_constant(value):
        return [0, value]

def constant_q(thing):
	return thing[0]==0

def make_variable(value):
	return [1, value]

def variable_q(thing):
	return thing[0]==1

def variable_symbol(variable):
	return variable[1]

def make_compound_term(functor, terms):
	return [2, functor, terms]

def compound_term_q(thing):
	return thing[0]==2

def compound_term_functor(thing):
	return thing[1]

def compound_term_terms(thing):
	return thing[2]

def make_clause(p, term, terms):
	return [4, p, term, terms]

def clause_q(thing):
	return thing[0]==4

def clause_p(clause):
	return clause[1]

def clause_term(clause):
	return clause[2]

def clause_terms(clause):
	return clause[3]

def make_p():
	return 0

def make_q():
	return 1

def make_x():
	return 2

def make_binding(variable, value):
	return [variable, value]

def binding_variable(binding):
	return binding[0]

def binding_value(binding):
	return binding[1]

def make_double(p, substitution):
	return [p, substitution]

def double_p(double):
	return double[0]

def double_substitution(double):
	return double[1]

def binds_q(substitution, variable):
	for binding in substitution:
		if binding_variable(binding)==variable:
			return True
        return False

def lookup_value(variable, substitution):
	for binding in substitution:
		if binding_variable(binding)==variable:
			return binding_value(binding)
	return []

def apply_substitution(substitution,  thing):
	if clause_q(thing):
		return make_clause(clause_p(thing),
                                   apply_substitution(substitution,
                                                      clause_term(thing)),
                                   [apply_substitution(substitution, term)
                                    for term in clause_terms(thing)])
	elif compound_term_q(thing):
		return make_compound_term(compound_term_functor(thing),
                                          [apply_substitution(substitution,
                                                              term)
                                           for term in compound_term_terms(thing)])
	elif variable_q(thing) and binds_q(substitution, thing):
		return lookup_value(thing, substitution)
	else:
		return thing

def union(set1, set2):
	set_union = []
	for element in set1:
		if element in set_union:
			continue
		else:
			set_union.append(element)
	for element in set2:
		if element in set_union:
			continue
		else:
			set_union.append(element)
	return set_union

def variables_in(thing):
	if clause_q(thing):
                variables = variables_in(clause_term(thing))
                for term in clause_terms(thing):
                        variables = union(variables, variables_in(term))
		return variables
	elif compound_term_q(thing):
                variables = []
                for term in compound_term_terms(thing):
                        variables = union(variables, variables_in(term))
		return variables
	elif variable_q(thing):
		return [thing]
	else:
		return []

def occurs_q(variable, term):
	return variable in variables_in(term)

def unify(term1, term2):
	if compound_term_q(term1):
		if compound_term_q(term2):
			if compound_term_functor(term1)!=compound_term_functor(term2):
                                return False
			terms1 = compound_term_terms(term1)
			terms2 = compound_term_terms(term2)
			substitution = []
                        if len(terms1)!=len(terms2):
                                return False
                        for i in range(len(terms1)):
                                substitution1 = unify(apply_substitution(substitution,
                                                                         terms1[i]),
                                                      apply_substitution(substitution,
                                                                         terms2[i]))
                                if substitution1==False:
                                       return False
                                substitution.extend(substitution1)
                        return substitution
		elif variable_q(term2):
			if occurs_q(term2, term1):
				return False
			else:
				return [make_binding(term2, term1)]
		else:
			return False
	elif variable_q(term1):
		if compound_term_q(term2):
			if occurs_q(term1, term2):
				return False
			else:
				return [make_binding(term1, term2)]
		elif variable_q(term2):
			return [make_binding(term1, term2)]
		else:
			return [make_binding(term1, term2)]
	else:
		if compound_term_q(term2):
			return False
		elif variable_q(term2):
			return [make_binding(term2, term1)]
		else:
			if term1==term2:
				return []
			else:
				return False

def alpha_rename(clause, offset):
        substitution = []
        for i, variable in enumerate(variables_in(clause)):
                substitution.append(make_binding(variable,
                                                 make_variable(offset+i)))
	return apply_substitution(substitution, clause)

def inner_loop(p, substitution, terms, clauses):
	if substitution==False:
                return []
        if len(terms)==0:
                return [make_double(p, substitution)]
        result = []
        for double in proof_distribution(apply_substitution(substitution,
                                                            terms[0]),
                                         clauses):
                result.extend(inner_loop(p*double_p(double),
                                         substitution+double_substitution(double),
                                         terms[1:],
                                         clauses))
        return result

def proof_distribution(term, clauses):
        offset = 0
        for variable in variables_in(term):
                offset = max(offset, variable_symbol(variable))
        for clause in clauses:
                for variable in variables_in(clause):
                        offset = max(offset, variable_symbol(variable))
        result = []
        for clause in clauses:
                clause = alpha_rename(clause, offset+1)
                result.extend(inner_loop(clause_p(clause),
                                         unify(term, clause_term(clause)),
                                         clause_terms(clause),
                                         clauses))
        return result

def likelihood(substitution_distribution):
        result = 0.0
        for double in substitution_distribution:
                result = result+double_p(double)
        return result

def objective(p):
	clauses = [make_clause(p[0],
                               make_compound_term(make_p(), [make_constant(0)]),
                               []),
	           make_clause(1-p[0],
                               make_compound_term(make_p(),
                                                  [make_variable(make_x())]),
                               [make_compound_term(make_q(),
                                                   [make_variable(make_x())])]),
	           make_clause(p[1],
                               make_compound_term(make_q(), [make_constant(1)]),
                               []),
	           make_clause(1-p[1],
                               make_compound_term(make_q(), [make_constant(2)]),
                               [])]
	observations = [0, 1, 2, 2]
        result = 1.0
	for observation in observations:
                result = result*likelihood(proof_distribution(make_compound_term(make_p(), [make_constant(observation)]), clauses))
        return result

for i in range(10):
	x = gradient_ascent_R(objective, [0.5, 0.5], 1000, 0.1)
	print x[0], x[1]
