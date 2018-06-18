t = require 'torch'
grad = require 'autograd'
grad.optimize(true)

common_Torch = {}

function common_Torch.magnitude_squared(x,size)
   if(type(x)==type(table))
   then
      local tmp=0
      for i=1,size do
         tmp = tmp+x[i].raw*x[i].raw
      end
      return tmp
   else
      local tmp=0
      for i=1,size do
         tmp = tmp+x[i]*x[i]
      end
      return tmp
   end
end

function common_Torch.magnitude(x,size)
   tmp = t.sqrt(common_Torch.magnitude_squared(x,size))
   if (type(tmp)==type(table)) then
      return tmp.raw
   else
      return tmp
   end
end

function common_Torch.distance(u,v,size)
   tmp = t.sqrt(common_Torch.magnitude_squared(u-v,size))
   if (type(tmp)==type(table)) then
      return tmp.raw
   else
      return tmp
   end
end

function common_Torch.compareSmall(x1,x2)
   if (type(x1)==type(table)) then
      return x1.raw<x2.raw
   else
      return x1<x2
   end
end

function common_Torch.multivariate_argmin(f, x, x_star, x2)
   local size = x_star:size()[1]
   local fx
   local gx
   local eta = 1e-5
   local T
   local x_prime
   local fx_prime = 0
   local i = 0
   local g_f = grad(f)
   gx, fx = g_f(x,x2)
   x_star = 1.0*x
   while(true)
   do
      ::continue::
      if (common_Torch.magnitude(gx,size) <= 1e-5)
      then
         return fx, x_star
      end
      if (i==10)
      then
         eta = eta*2.0
         i = 0
         goto continue
      end
      T = gx*eta
      x_prime = x_star-T
      if (common_Torch.distance(x_star,x_prime,size) <= 1e-5)
      then
         return fx, x_star
      end
      g_f = grad(f)
      gx, fx_prime = g_f(x_prime,x2)
      if (common_Torch.compareSmall(fx_prime,fx))
      then
         x_star = 1.0*x_prime
         fx = fx_prime
         i = i+1
         goto continue
      end
      eta = eta/2.0
      i = 0
   end
end

return common_Torch
