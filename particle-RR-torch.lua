t = require 'torch'
common = require 'common-torch'
grad = require 'autograd'
grad.optimize(true)

local p = function(X,W)
   local d1 = 1.0/t.sqrt((X[1]-10)^2+(X[2]-10+W[1])^2)
   local d2 = 1.0/t.sqrt((X[1]-10)^2+(X[2])^2)
   local d = d1+d2
   return d
end

local naive_euler = function(W,X)
   local xdot = t.Tensor({0.75,0.0})
   local xddot
   local delta_t = 1e-1
   local g
   local T
   local x_new
   local x_t_f
   local delta_t_f = 0
   local x = X*1.0
   local g_p
   local grads
   while(true)
   do
      ::continue::
      g_p = grad(p)
      grads = g_p(x,W)
      xddot = -1.0*grads
      T = xdot*delta_t
      x_new = x+T
      if(x_new[2].raw>0.0)
      then
         x = 1.0*x_new
         T = xddot*delta_t
         xdot = xdot+T
         goto continue
      end
      delta_t_f = -x[2]/xdot[2]
      T = xdot*delta_t_f
      x_t_f = x+T
      return x_t_f[1]*x_t_f[1]
   end
end

w0 = t.Tensor({0.0})
w_star = t.Tensor({0.0})
X = t.Tensor({0.0,8.0})
_, w_star = common.multivariate_argmin(naive_euler, w0, w_star, X)
print(w_star[1])
