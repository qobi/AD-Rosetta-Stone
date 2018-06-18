import common_hips_autograd as cha
import autograd.numpy as np
from autograd import grad

N_SAMPLES = 4
N_IN      = 2
N_OUT     = 1

xor_data = np.zeros([N_SAMPLES, N_IN+N_OUT])
xor_data[0][0] = 0.0
xor_data[0][1] = 0.0
xor_data[0][2] = 0.0
xor_data[1][0] = 0.0
xor_data[1][1] = 1.0
xor_data[1][2] = 1.0
xor_data[2][0] = 1.0
xor_data[2][1] = 0.0
xor_data[2][2] = 1.0
xor_data[3][0] = 1.0
xor_data[3][1] = 1.0
xor_data[3][2] = 0.0

def sigmoid(x):
	return 1.0/(1.0+np.exp(-x))

def sum_layer_sigmoid(activities, ws_layer):
	activities = np.concatenate((np.ones(1), activities), 0)
	activities = np.expand_dims(activities, 1)
	out = np.matmul(ws_layer, activities)
	out = out.reshape(-1)
	return sigmoid(out)

def forward_pass(ws_layers, in_):
	temp_in = in_
	for i in range(len(ws_layers)):
		temp_out = sum_layer_sigmoid(temp_in, ws_layers[i])
		temp_in = temp_out
	return temp_out

def error_on_dataset(ws_layers):
	error = np.zeros([1])
	for i in range(N_SAMPLES):
		in_ = xor_data[i][:N_IN]
		out_ = forward_pass(ws_layers, in_)
		absolute_error = out_-xor_data[i, N_IN:]
		error = error+0.5*cha.magnitude_squared(absolute_error)
	return error

def vanilla(f, w0, n, eta):
	g_f = grad(f)
	for i in range(n):
		grad_f = g_f(w0)
		w1 = np.subtract(w0[0], np.multiply(eta, grad_f[0]))
		w2 = np.subtract(w0[1], np.multiply(eta, grad_f[1]))
		w0 = (w1, w2)
	return w0

if __name__=="__main__":
	weights1 = np.array([[0.0, -0.284227, 1.16054],
                             [0.0, 0.617194, 1.30467]])
	weights2 = np.array([[0.0, -0.084395, 0.648461]])
	xor_ws0 = (weights1, weights2)
	xor_ws0 = vanilla(error_on_dataset, xor_ws0, 10000, 0.3)
	error = error_on_dataset(xor_ws0)
	print '{0:.18f}'.format(error[0])
