from autograd.numpy import exp
from autograd import grad
from common_hips_autograd import vminus, magnitude_squared

def sum_activities(activities):
        def inner(bias_ws):
                result = bias_ws[0]
                for i in range(len(activities)):
                        result = result+bias_ws[i+1]*activities[i]
                return result
        return inner

def sum_layer(activities, ws_layer):
        return [sum_activities(activities)(bias_ws) for bias_ws in ws_layer]

def sigmoid(x):
	return 1.0/(1.0+exp(-x))

def forward_pass(ws_layers):
        def inner(in_):
                if len(ws_layers)==0:
                        return in_
                return forward_pass(ws_layers[1:])(
                        [sigmoid(out) for out in sum_layer(in_, ws_layers[0])])
        return inner

def error_on_dataset(dataset):
        def inner(ws_layers):
                result = 0.0
                for in_target in dataset:
                        in_ = in_target[0]
                        target = in_target[1]
                        result = result+0.5*magnitude_squared(vminus(forward_pass(ws_layers)(in_), target))
                return result
        return inner

def s_minus_k_times(x, k, y):
        if isinstance(x, list):
                return [s_minus_k_times(x[i],k,y[i]) for i in range(len(x))]
        return x-k*y

weight_gradient = grad

def vanilla(f, w0, n, eta):
	for i in range(n):
                w0 = s_minus_k_times(w0, eta, weight_gradient(f)(w0))
	return f(w0)

xor_ws0 = [[[0.0, -0.284227, 1.16054], [0.0, 0.617194, 1.30467]],
           [[0.0, -0.084395, 0.648461]]]

xor_data = [[[0.0, 0.0], [0.0]],
            [[0.0, 1.0], [1.0]],
            [[1.0, 0.0], [1.0]],
            [[1.0, 1.0], [0.0]]]

print vanilla(error_on_dataset(xor_data), xor_ws0, 10000, 0.3)
