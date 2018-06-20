from common_pytorch import gradient_ascent_R

def make_constant_expression(value):
	return [0, value]

def contant_expression_q(expression):
	return expression[0]==0

def constant_expression_value(expression):
	return expression[1]

def make_variable_access_expression(variable):
	return [1, variable]

def variable_access_expression_q(expression):
	return expression[0]==1

def variable_access_expression_variable(expression):
	return expression[1]

def make_lambda_expression(variable, body):
	return [2, variable, body]

def lambda_expression_q(expression):
	return expression[0]==2

def lambda_expression_variable(expression):
	return expression[1]

def lambda_expression_body(expression):
	return expression[2]

def make_application(callee, argument):
	return [3, callee, argument]

def application_callee(expression):
	return expression[1]

def application_argument(expression):
	return expression[2]

def make_ignore():
	return 0

def make_if_procedure():
	return 1

def make_x0():
	return 2

def make_x1():
	return 3

def make_binding(variable, value):
	return [variable, value]

def binding_variable(binding):
	return binding[0]

def binding_value(binding):
	return binding[1]

def make_triple(p, environment, value):
	return [p, environment, value]

def triple_p(triple):
	return triple[0]

def triple_environment(triple):
	return triple[1]

def triple_value(triple):
	return triple[2]

def binds_q(environment, variable):
	for binding in environment:
		if binding_variable(binding)==variable:
			return True
        return False

def lookup_value(variable, environment):
	for binding in environment:
		if binding_variable(binding)==variable:
			return binding_value(binding)
	return []

def merge_environments(environment1, environment2):
	if environment1==[]:
		return environment2
	else:
		environment = merge_environments(environment1[1:], environment2)
		if environment==False:
			return False
		else:
			if binds_q(environment, binding_variable(environment1[0])):
				if lookup_value(binding_variable(environment1[0]), environment)==binding_value(environment1[0]):
					return environment
				else:
					return False
			else:
				return [environment1[0]]+environment

def singleton_tagged_distribution(value):
	return [make_triple(1.0, [], value)]

def boolean_distribution(p, variable):
	return [make_triple(1.0-p, [make_binding(variable, False)], False),
                make_triple(p, [make_binding(variable, True)], True)]

def normalize_tagged_distribution(tagged_distribution):
        n = 0.0
        for triple in tagged_distribution:
                n = n+triple_p(triple)
        return [make_triple(triple_p(triple)/n,
                            triple_environment(triple),
                            triple_value(triple)) for triple in tagged_distribution]

def map_tagged_distribution(f, tagged_distribution):
        result = []
        for triple in tagged_distribution:
                triples = []
                for triple1 in [make_triple(triple_p(triple)*triple_p(triple1),
                                             merge_environments(triple_environment(triple),
                                                                triple_environment(triple1)),
                                             triple_value(triple1))
                                 for triple1 in f(triple_value(triple))]:
                        if triple_environment(triple1)!=False:
                                triples.append(triple1)
                result = result+triples
        return normalize_tagged_distribution(result)

def evaluate(expression, environment):
	if contant_expression_q(expression):
		return singleton_tagged_distribution(constant_expression_value(expression))
	elif variable_access_expression_q(expression):
		return lookup_value(variable_access_expression_variable(expression), environment)
	elif lambda_expression_q(expression):
		return singleton_tagged_distribution(lambda tagged_distribution: evaluate(lambda_expression_body(expression), [make_binding(lambda_expression_variable(expression), tagged_distribution)]+environment))
	else:
		tagged_distribution = evaluate(application_argument(expression), environment)
		return map_tagged_distribution(lambda value: value(tagged_distribution), evaluate(application_callee(expression), environment))

def likelihood(value, tagged_distribution):
        result = 0.0
        for triple in tagged_distribution:
                if value==triple_value(triple):
                        result = result+triple_p(triple)
        return result

def make_if(antecedent, consequent, alternate):
	return make_application(
                make_application(
                        make_application(
                                make_variable_access_expression(
                                        make_if_procedure()),
                                antecedent),
                        make_lambda_expression(make_ignore(), consequent)),
                make_lambda_expression(make_ignore(), alternate))

def objective(p):
	expression = make_if(make_variable_access_expression(make_x0()),
                             make_constant_expression(0),
                             make_if(make_variable_access_expression(make_x1()),
                                     make_constant_expression(1),
                                     make_constant_expression(2)))
	def lambda_x(x):
		def lambda_y(y):
			def lambda_z(z):
				def lambda_xe(xe):
					def lambda_ye(ye):
						def lambda_ze(ze):
							if xe:
								return ye(False)
							else:
								return ze(False)
						return map_tagged_distribution(lambda_ze, z)
					return map_tagged_distribution(lambda_ye, y)
				return map_tagged_distribution(lambda_xe, x)
			return singleton_tagged_distribution(lambda_z)
		return singleton_tagged_distribution(lambda_y)
	environment = [make_binding(make_x0(),
                                    boolean_distribution(p[0], make_x0())),
                       make_binding(make_x1(),
                                    boolean_distribution(p[1], make_x1())),
                       make_binding(make_if_procedure(),
                                    singleton_tagged_distribution(lambda_x))]
	tagged_distribution = evaluate(expression, environment)
	observations = [0, 1, 2, 2]
	result = 1.0
	for observation in observations:
		result = result*likelihood(observation, tagged_distribution)
	return result

if __name__=="__main__":
	for i in range(10):
		x = gradient_ascent_R(objective, [0.5, 0.5], 1000, 0.1)
		print x[0], x[1]
