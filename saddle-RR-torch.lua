t = require 'torch'
common = require 'common-torch'
grad = require 'autograd'
grad.optimize(true)

local INNER = 2
local OUTER = 2
local TOTAL = INNER+OUTER

local f = function(x1,x2)
   local tmp = 0
   for i=1,INNER do
      tmp = tmp+x1[i]*x1[i]-x2[i]*x2[i]
   end
   return tmp
end

local inner = function(x2,x1c)
   x1 = x1c
   return -f(x1,x2)
end

local outer = function(x1,x2)
   local x2_star = t.Tensor(INNER)
   local x1c = 1.0*x1
   local fx,_ = common.multivariate_argmin(inner, x2, x2_star, x1c)
   return -fx
end

x1_start = t.Tensor({1.0,1.0})
x2_start = t.Tensor({1.0,1.0})
x1_star = t.Tensor(OUTER)
x2_star = t.Tensor(INNER)
_, x1_star = common.multivariate_argmin(outer, x1_start, x1_star, x2_start)
_, x2_star = common.multivariate_argmin(inner, x2_start, x2_star, x1_star)
print(x1_star,x2_star)
