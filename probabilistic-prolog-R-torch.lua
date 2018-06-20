t = require 'torch'
grad = require 'autograd'

add = function(x1,x2)
   local tmp = {}
   for k,v in pairs(x1) do
      table.insert(tmp,v)
   end
   for k,v in pairs(x2) do
      tabe.insert(tmp,v)
   end
   return tmp
end

In = function(x,set)
   for k,v in pairs(set) do
      if equal(x,v) then
         return true
      end
   end
   return false
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

max = function(x)
   local tmp = x[1]
   for k,v in pairs(x) do
      if v>tmp then
         tmp = v
      end
   end
   return tmp
end

rest = function(x)
   table.remove(x,1)
   return x
end

append = function(x1,x2)
   table.insert(x1,x2)
   return x1
end

make_constant = function(symbol)
   return {0,symbol}
end

constant_q = function(thing)
   return thing[1] == 0
end

make_variable = function(symbol)
   return {1,symbol}
end

variable_q =function(thing)
   return thing[1] == 1
end

variable_symbol = function(thing)
   return thing[2]
end

make_compound_term = function(functor,terms)
   return {2,functor,terms}
end

compound_term_q = function(thing)
   return thing[1] == 2
end

compound_term_functor = function(compound_term)
   return compound_term[2]
end

compound_term_terms = function(compound_term)
   return compound_term[3]
end

make_clause = function(p,term,terms)
   return {4,p,term,terms}
end

clause_q = function(thing)
   return thing[1] == 4
end

clause_p = function(clause)
   return clause[2]
end

clause_term = function(clause)
   return clause[3]
end

clause_terms = function(clause)
   return clause[4]
end

make_p = 0

make_q = 1

make_x = 2

make_binding = function(variable,value)
   return {variable,value}
end

binding_variable = function(binding)
   return binding[1]
end

binding_value = function(binding)
   return binding[2]
end

make_double = function(p,substitution)
   return {p,substitution}
end

double_p = function(double)
   return double[1]
end

double_substitution = function(double)
   return double[2]
end


binds_q = function(substitution,variable)
	if length(substitution) ~= 0 then
		flag = false
		for k, item in pairs(substitution) do
			if equal(variable,item) then
				flag = true
			end
		end
		return flag

	
	else
		return false
	end
end

lookup_value = function(variable,substitution)
	for k,item in pairs({substitution}) do
	
		if equal(variable,binding_variable(item)) then
			return binding_value(item)
		end
	end
	return {}
end
		
apply_substitution = function(substitution, thing)

	inner_loop = function(thing_local)
		if clause_q(thing_local) then
			p = clause_p(thing_local)
			term = inner_loop(clause_term(thing_local))
			terms = inner_loop(clause_terms(thing_local))
			return make_clause(p,term,terms)
			
			
			
		elseif compound_term_q(thing_local) then
			functor = compound_term_functor(thing_local)
			terms = inner_loop(compound_term_terms(thing_local))
			return make_compound_term(functor,terms)
			
			
			
		elseif variable_q(thing_local) and binds_q(substitution,thing_local) then
			return lookup_value(thing_local,substitution)
		else
			return thing_local
		end
	end
			
	thing_local = thing
	return inner_loop(thing_local)
end
		
member_q = function(variable,set_)
	if In(variable,set_) then
		return true
	else
		return false
	end
end
		
union = function(set1,set2)
	
	set_union = {}
	for k,ele in pairs(set1) do
		if In(ele,set_union) then
			k=k
		else
			table.insert(set_union,ele)
		end
	end
			
	for k,ele in pairs(set2) do
		if In(ele,set_union) then
			k=k
		else
			table.insert(set_union,ele)
		end
	end
			
	return set_union
end


variables_in = function(thing)
	if clause_q(thing) then
		return union(variables_in(clause_term(thing)),variables_in(clause_terms(thing)))	
	elseif compound_term_q(thing) then
		return union({},variables_in(compound_term_terms(thing)))
	elseif variable_q(thing) then
		return {thing}
	else
		return {}
	end
end
		
occurs_q = function(variable,term)
   return member_q(variable,variables_in(term))
end
	
unify = function(term1,term2)

	inner_loop1 = function(terms1,terms2,substitution)
		if length(terms1[1])== 0 then
			if length(terms2[1]) == 0 then
				return substitution
			else
				return false
			end
		else
			if length(terms2[1]) == 0 then
				return false
			else
				substitution1 = unify(apply_substitution(substitution,terms1[1]),apply_substitution(substitution,terms2[1]))
			end
		end
               	if type(substitution1) == "boolean" then
					return false
               	else
                	terms1 = rest(terms1)
                	terms2 = rest(terms2)
                	if length(substitution) == 0 then
                		substitution = substitution1
                	elseif length(substitution[1]) == 0 then
                		substitution = substitution1
                	else
                		if length(substitution1) == 0 then
                			substitution = substitution
                		elseif length(substitution1[1]) == 0 then
                			substitution = substitution
                		else
                			for k, item in substitution1 do
                			 	substitution = append(substitution,item)
					end
				end
			end
                	return inner_loop1({terms1},{terms2},substitution)
		end
	end
                        	
	if compound_term_q(term1) then
		if compound_term_q(term2) then
			if equal(compound_term_functor(term1), compound_term_functor(term2)) then
				terms1 = compound_term_terms(term1)
				terms2 = compound_term_terms(term2)
				substitution = {}
				return inner_loop1({terms1},{terms2},substitution)
        
			else
				return false
			end
        
		elseif variable_q(term2) then
			if occurs_q(term2,term1) then
				return false
			else
				return make_binding(term2,term1)
			end
    
		else
			return false
		end

	elseif variable_q(term1) then

		if compound_term_q(term2) then
			if occurs_q(term1,term2) then
				return false
			else
				return make_binding(term1,term2)
			end
    
		elseif variable_q(term2) then
			return make_binding(term1,term2)
    
		else
			return make_binding(term1,term2)
		end

	else
		if compound_term_q(term2) then
			return false
                             
		elseif variable_q(term2) then
			return make_binding(term2,term1)
		else
			if equal(term1,term2) then
				return {}
			else
				return false	
			end
		end
	end
end
     
function1 = function(variable,i,offset)
	return make_binding(variable,make_variable(offset+i))   
end    
	 
map_indexed = function(f,l,offset)
	inner_loop2 = function(f,l,i)
		if length(l) == 0 then
			return {}
		else
			tmp1 = f(l[1],i,offset)
			tmp2 = inner_loop2(f,rest(l),i+1) 
			if length(tmp1) ~=0 then
				if length(tmp2) ~= 0 then
					for k,item in pairs(tmp2) do
						tmp1 = append(tmp1,item)
					end
					return tmp1
				else
					return tmp1
				end
			else
				if length(tmp2) ~= 0 then
					return tmp2
				else
					return {}
				end
			end
		end
	end				
     
	i = 0 
	return inner_loop2(f,l,i)
end

alpha_rename = function(clause, offset)
	var = variables_in(clause)
	map_result = map_indexed(function1,var,offset)
	return apply_substitution(map_result,clause)
end

proof_distribution = function(term,clauses)

	inner_loop3 = function(p,substitution,terms)
		if type(substitution) == "boolean"  then
			return {}
		else
			if length(terms[1]) == 0 then
				return make_double(p,substitution)
			else
				return_result= {}
				
				for k,double in pairs(proof_distribution(apply_substitution(substitution,terms[1]),clauses)) do
					p = p*double_p(double)
					if length(substitution) == 0 then
						substitution = {double_substitution(double)}
					else
						substitution = add(substitution,double_substitution(double))
					end
					terms = rest(terms)
					loop_result = inner_loop3(p,substitution,{terms})
					return_result = append(return_result,inner_loop3(p,substitution,{terms}))
				end
				return return_result
			end
		end
	end	
	
	variable_list = {}
	for i,clause in pairs(clauses) do
		tmp_v =variables_in(clause)
		if length(tmp_v) == 0 then
			i = i
		else
			for k,v in pairs(tmp_v) do
				variable_list = append(variable_list,v)
			end
		end
	end

	v_s = {}
	for k,item in pairs(union(variables_in(term),variable_list)) do
		table.insert(v_s, variable_symbol(item))
	end
	offset = 1+max({0,max(v_s)})
	
	local clause_result = {}
	
	for i,clause in pairs(clauses) do
		clause = alpha_rename(clause,offset)
		p = clause_p(clause)
		substitution = unify(term,clause_term(clause))
		terms = clause_terms(clause)
		loop_result = inner_loop3(p,substitution,{terms})
		if length(loop_result) == 0 then
			clause_result = clause_result
		else
			if length(clause_result) == 0 then
				clause_result = {loop_result}
			else
				clause_result  = add(clause_result,loop_result)
			end
		end
	end
	return clause_result
end
		
likelihood = function(substitution_distribution)
	p_list = {}
	prob = 0.0
	if type(substitution_distribution[1][1]) == "table" then
		substitution_distribution = substitution_distribution[1]
	end
	for k,item in pairs(substitution_distribution) do
		p_list = append(p_list,double_p(item))
	end
	for k,item in pairs(p_list) do
		prob = prob + item 
	end
	return prob
end
		
		
		 

example = function(p)
	clauses = {}
	clause = append(clauses,make_clause(p.p0,make_compound_term(make_p,make_constant(0)),{}))
	clause = append(clauses,make_clause(1-p.p0,make_compound_term(make_p,make_variable(make_x)),make_compound_term(make_q,make_variable(make_x))))
	clause = append(clauses,make_clause(p.p1,make_compound_term(make_q,make_constant(1)),{}))
	clause = append(clauses,make_clause(1-p.p1,make_compound_term(make_q,make_constant(2)),{}))	
	observation = {0,1,2,2}
	overall_prob = 1.0
	for k,obsv in pairs(observation) do
		ct = make_compound_term(make_p,make_constant(obsv))
		overall_prob = overall_prob*likelihood(proof_distribution(ct,clauses))
	end
 	return overall_prob
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
