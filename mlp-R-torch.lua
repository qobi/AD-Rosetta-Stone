t = require 'torch'
grad = require 'autograd'
util = require 'autograd.util'

N_SAMPLES = 4
N_IN = 2
N_OUT = 1
LAYERS = 2
WEIGHTS = 3
ELEMENTS_LAYER = {
   ELEMENTS_LAYER1 = 2,
   ELEMENTS_LAYER2 = 1,
   ELEMENTS_LAYER_MAX = 2,
}

magnitude_squared = function(x)
   local tmp = 0
   if type(x)=="table" then
      for i=1,x["value"]:size()[1] do
         tmp = tmp+x[i]*x[i]
      end
   else
      for i=1,x:size()[1] do
         tmp = tmp+x[i]*x[i]
      end
   end
   return tmp
end

sum_layer_sigmoid = function(ws_layer, activities, n_out_layer)
   local out = ws_layer.w*activities
   out = out + ws_layer.b
   out = util.sigmoid(out)
   return out
end

forward_pass = function(ws_layers, In)
   local temp_out
   local temp_in
   temp_in = In
   for i = 1,LAYERS do
      temp_out = sum_layer_sigmoid(ws_layers["layer"..tostring(i)], temp_in, ELEMENTS_LAYER["ELEMENTS_LAYER"..tostring(i)])
      temp_in = temp_out
   end
   return temp_out
end

error_on_dataset = function(ws_layers, xor_data)
   local Error = 0.0
   local In
   local out
   local absolute_error
   for i = 1,N_SAMPLES do
      In = xor_data.xor_data[i]
      out = forward_pass(ws_layers, In)
      absolute_error = out-xor_data.xor_label[i]
      Error = Error + 0.5*magnitude_squared(absolute_error)
   end
   return Error
end

weight_gradient = function(f, ws_layers, xor_data)
   local g_f = grad(f)
   local grad_f,_ = g_f(ws_layers, xor_data)
   return grad_f
end

vanilla = function(f, w0, xor_data, n, eta)
   for i=1,n do
      local grad_f = weight_gradient(f, w0, xor_data)
      for k,_ in pairs(w0) do
         w0[k].w = w0[k].w-eta*grad_f[k].w
         w0[k].b = w0[k].b-eta*grad_f[k].b
      end
   end
   return w0
end

xor_ws0 = {
   layer1 = {
      w = t.Tensor(ELEMENTS_LAYER.ELEMENTS_LAYER1,WEIGHTS-1),
      b = t.Tensor(ELEMENTS_LAYER.ELEMENTS_LAYER1),
   },
   layer2 = {
      w = t.Tensor(ELEMENTS_LAYER.ELEMENTS_LAYER2,WEIGHTS-1),
      b = t.Tensor(ELEMENTS_LAYER.ELEMENTS_LAYER2),
   }
}
xor_ws0.layer1.b[1] = 0.0;
xor_ws0.layer1.w[1][1] = -0.284227;
xor_ws0.layer1.w[1][2] = 1.16054;
xor_ws0.layer1.b[2] = 0.0;
xor_ws0.layer1.w[2][1] = 0.617194;
xor_ws0.layer1.w[2][2] = 1.30467;
xor_ws0.layer2.b[1] = 0.0;
xor_ws0.layer2.w[1][1] = -0.084395;
xor_ws0.layer2.w[1][2] = 0.648461;
xor_data = {
   xor_data = t.Tensor(N_SAMPLES,N_IN),
   xor_label = t.Tensor(N_SAMPLES),
}
xor_data.xor_data[1][1] = 0.0
xor_data.xor_data[1][2] = 0.0
xor_data.xor_data[2][1] = 0.0
xor_data.xor_data[2][2] = 1.0
xor_data.xor_data[3][1] = 1.0
xor_data.xor_data[3][2] = 0.0
xor_data.xor_data[4][1] = 1.0
xor_data.xor_data[4][2] = 1.0
xor_data.xor_label[1] = 0.0
xor_data.xor_label[2] = 1.0
xor_data.xor_label[3] = 1.0
xor_data.xor_label[4] = 0.0
xor_ws0 = vanilla(error_on_dataset, xor_ws0, xor_data, 1000000, 0.3)
Error = error_on_dataset(xor_ws0, xor_data)
print(Error)
