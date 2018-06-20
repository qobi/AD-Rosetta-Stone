t = require 'torch'
grad = require 'autograd'

remove_if = function(p,l)
   local tmp = {}
   if next(l) == nil then
      return {}
   end
   for k,v in pairs(l) do
      if not p(v) then
         table.insert(tmp,v)
      end
   end
   return tmp
end

length = function(x)
   local i = 0
   for k,v in pairs(x) do
      i = i+1
   end
   return i
end

equal = function(x,y)
   if type(x)~=type(y) then
      return false
   end
   if type(x) == "table" then
      if length(x) ~= length(y) then
         return false
      end
      local tmp = true
      for k,v in pairs(x) do
         tmp = tmp and equal(v,y[k])
      end
      return tmp
   else
      return x==y
   end
end

map = function(f, x)
   local tmp = {}
   for k,v in pairs(x) do
      table.insert(tmp,f(v))
   end
   return tmp
end

rest = function(x)
   local tmp = {}
   local count = 1
   for k,v in pairs(x) do
      if count ~= 1 then
         table.insert(tmp,v)
      end
      count = count+1
   end
   return tmp
end

cons = function(x1,x2)
   local tmp = {}
   table.insert(tmp,x1)
   for k,v in pairs(x2) do
      table.insert(tmp,v)
   end
   return tmp
end

append = function(x1,x2)
   local tmp = {}
   for k,v in pairs(x1) do
      table.insert(tmp,v)
   end
   for k,v in pairs(x2) do
      table.insert(tmp,v)
   end
   return tmp
end

make_constant_expression = function(value)
   return {0,value}
end

constant_expression_q = function(expression)
   return expression[1]==0
end

constant_expression_value = function(expression)
   return expression[2]
end

make_variable_access_expression = function(variable)
   return {1,variable}
end

variable_access_expression_q = function(expression)
   return expression[1]==1
end

variable_access_expression_variable = function(expression)
   return expression[2]
end

make_lambda_expression = function(variable,body) 
   return {2, variable,body}
end

lambda_expression_q = function(expression)
   return expression[1]==2
end

lambda_expression_variable = function(expression)
   return expression[2]
end

lambda_expression_body = function(expression)
   return expression[3]
end

make_application = function(callee,argument)
   return {3,callee,argument}
end

application_callee = function(expression)
   return expression[2]
end

application_argument = function(expression)
   return expression[3]
end

make_ignore = 0

make_if_procedure = 1

make_x0 = 2

make_x1 = 3

make_binding = function(variable,value) 
   return {variable,value}
end

binding_variable = function(binding)
   return binding[1]
end

binding_value = function(binding)
   return binding[2]
end

make_triple = function(p,environment,value)
   return {p,environment,value}
end

triple_p = function(triple)
   return triple[1]
end

triple_environment = function(triple)
   return triple[2]
end
   
triple_value = function(triple)
   return triple[3]
end

binds_q = function(environment,variable)
   return (not (next(environment)==nil)) 
           and 
           (equal(variable, binding_variable(environment[1])) or binds_q(rest(environment),variable))
end

lookup_value = function(variable,environment)
   for k,v in pairs(environment) do
      if equal(variable,binding_variable(v)) then
         return binding_value(v)
      end
   end
end

merge_environments = function(environment1,environment2)
  if next(environment1) == nil then
     return environment2
  else
     environment_merge =  merge_environments(rest(environment1),environment2)
     if type(environment_merge) == "boolean" then
	     return false
	 else
	    if binds_q(environment_merge,binding_variable(environment1[1])) then
	       if equal(lookup_value(binding_variable(environment1[1]),environment_merge),binding_value(environment1[1])) then
		  return environment_merge
	       else
		  return false
	       end
	    else
               return cons(environment1[1],environment_merge)
            end
	 end
  end
end

singleton_tagged_distribution = function(value)
   return {make_triple(1.0,{},value)}
end

boolean_distribution = function(p,variable)
    return {make_triple(1.0-p,{make_binding(variable,false)},false),
            make_triple(p,{make_binding(variable,true)},true)}
end

normalize_tagged_distribution = function(tagged_distribution)
   local n = 0.0
   for k,v in pairs(tagged_distribution) do
      n = n+triple_p(v)
   end
   lambda_nor = function(triple)
      if type(n) == "number" then
         return make_triple(triple_p(triple)/n,triple_environment(triple),triple_value(triple))
      else
         return make_triple(triple_p(triple)/n[1],triple_environment(triple),triple_value(triple))
      end
   end
   return map(lambda_nor,tagged_distribution)
end

map_tagged_distribution = function(f,tagged_distribution)
   local tmp = {}
   for k,v in pairs(tagged_distribution) do
      lambda1 = function(triple)
	 return type(triple_environment(triple))=="boolean"
      end
      lambda2 = function(triple)
	 return make_triple(triple_p(v)*triple_p(triple),merge_environments(triple_environment(v),triple_environment(triple)),triple_value(triple))
      end
      local a = (map(lambda2,f(triple_value(v))))
      local b = remove_if(lambda1,a)
      tmp = append(tmp,b)
   end
   return normalize_tagged_distribution(tmp)
end

evaluate = function(expression,environment)
   if constant_expression_q(expression) then
      return singleton_tagged_distribution(constant_expression_value(expression))
   end
   if variable_access_expression_q(expression) then
      return lookup_value(variable_access_expression_variable(expression),environment)
   end
   if lambda_expression_q(expression) then
      lambda1_evl= function(tagged_distribution_evl)
	 return evaluate(lambda_expression_body(expression),cons(make_binding(lambda_expression_variable(expression),tagged_distribution_evl),environment))
      end
      return singleton_tagged_distribution(lambda1_evl)
   else
      local tagged_distribution_evl = evaluate(application_argument(expression),environment)
      local lambda2_evl = function(value)
	 return value(tagged_distribution_evl)
      end
      local a = evaluate(application_callee(expression),environment)

      return map_tagged_distribution(lambda2_evl,a)
   end
end

likelihood = function(value,tagged_distribution)
   local tmp = 0.0
   for k,v in pairs(tagged_distribution) do
      if value == triple_value(v) then
         tmp = tmp + triple_p(v)
      end
   end
   return tmp
end

make_if = function(antecedent,consequent,alternate)
   return make_application
          (make_application
           (make_application(make_variable_access_expression(make_if_procedure),antecedent),
            make_lambda_expression(make_ignore,consequent)),
           make_lambda_expression(make_ignore,alternate))
end

example = function(p)
   lambdax = function(x)
      lambday = function(y)
	 lambdaz = function(z)
	    lambdaxe = function(xe)
	       lambdaye = function(ye)
	          lambdaze = function(ze)
		     if xe then
			return ye(false)
		     else
			return ze(false)
		     end

	          end
		  return map_tagged_distribution(lambdaze,z)
	       end
	       return map_tagged_distribution(lambdaye,y)
	    end
            return map_tagged_distribution(lambdaxe,x)
	 end
	 return singleton_tagged_distribution(lambdaz)
      end
      return singleton_tagged_distribution(lambday)
   end
      

   expression = make_if(make_variable_access_expression(make_x0),
		    make_constant_expression(0),
		    make_if(make_variable_access_expression(make_x1),
			    make_constant_expression(1),
			    make_constant_expression(2)))
   environment  = {make_binding(make_x0,boolean_distribution(p.p0,make_x0)),
	     make_binding(make_x1,boolean_distribution(p.p1,make_x1)),
	     make_binding(make_if_procedure,singleton_tagged_distribution(lambdax))}

   tagged_distribution = evaluate(expression,environment)

   lambda_observation = function(observation)
      return likelihood(observation,tagged_distribution)
   end
   
   observations = {0,1,2,2}
   initial_p = 1.0
   for k,v in pairs(observations) do
      initial_p = initial_p*lambda_observation(v)
   end
   if type(initial_p) == "number" then
      return initial_p
   else
      return initial_p[1]
   end
end

run = function()
	for k=1,10 do
		p = {
		   p0 = t.Tensor({0.5}),
		   p1 = t.Tensor({0.5})
		}

		p_tmp = {
		   p0 = t.Tensor({0.5}),
		   p1 = t.Tensor({0.5})
		}
		eta = 0.1
		g_f = grad(example)

		for i=1,1000 do

	   		local grads,loss = g_f(p)
	   		p_tmp.p0 = p_tmp.p0+eta*grads.p0
	   		p_tmp.p1 = p_tmp.p1+eta*grads.p1
	   		p = {
	      		p0 = p_tmp.p0,
	      		p1 = p_tmp.p1,
			}
		end
		print("p0, p1: ",p.p0[1],p.p1[1])
	end
end
run()
