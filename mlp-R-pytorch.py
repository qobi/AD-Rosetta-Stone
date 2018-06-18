import torch

N_SAMPLES 		= 4
N_IN 			= 2
N_OUT 			= 1
LAYERS 			= 2
ELEMENTS_LAYER1 	= 2
ELEMENTS_LAYER2 	= 1
WEIGHTS 		= 3

xor_data = torch.zeros([N_SAMPLES, N_IN+N_OUT])
xor_data[0][0] 	= 0.0
xor_data[0][1] 	= 0.0
xor_data[0][2] 	= 0.0
xor_data[1][0] 	= 0.0
xor_data[1][1] 	= 1.0
xor_data[1][2] 	= 1.0
xor_data[2][0] 	= 1.0
xor_data[2][1] 	= 0.0
xor_data[2][2] 	= 1.0
xor_data[3][0] 	= 1.0
xor_data[3][1] 	= 1.0
xor_data[3][2] 	= 0.0

class w_layer(object):
	def __init__(self, n = 1, w = 1, weights = None):
		self.n = n
		self.w = w
		if weights is None:
			self.layer = torch.ones((n, w), requires_grad = True)
		else:
			self.layer = torch.tensor(weights, requires_grad = True)

def magnitude_squared(x):
	return torch.sum(torch.mul(x, x))

def sum_layer_sigmoid(activities, ws_layer):
	activities = torch.cat((torch.ones(1), activities), 0)
	activities = torch.unsqueeze(activities, 1)
	out = torch.mm(ws_layer.layer, activities)
	return torch.sigmoid(out.view(-1))

def forward_pass(n_ws_layers, ws_layers, in_):
	temp_in = in_
	for i in range(n_ws_layers):
		temp_out = sum_layer_sigmoid(temp_in, ws_layers[i])
		temp_in = temp_out
	return temp_out

def error_on_dataset(n_ws_layers, ws_layers):
	out_ = torch.empty(N_OUT)
	error = torch.zeros([1])
	for i in range(N_SAMPLES):
		in_ = xor_data[i][:N_IN]
		out_ = forward_pass(n_ws_layers, ws_layers, in_)
		absolute_error = out_-xor_data[i, N_IN:]
		error = error+0.5*magnitude_squared(absolute_error)
	return error

def weight_gradient(f, n_ws_layers, ws_layers, grad_f):
	result = f(n_ws_layers, ws_layers)
	result.backward()
	for i in range(n_ws_layers):
		grad_f[i].layer = ws_layers[i].layer.grad
	return grad_f

def vanilla(f, n_w0, w0, n, eta):
	grad_f = []
	for i in range(n_w0):
		grad_f.append(w_layer())
	for i in range(n_w0):
    		grad_f[i].n = w0[i].n
    		grad_f[i].w = w0[i].w
		grad_f[i].layer = torch.zeros([w0[i].n, w0[i].w])
	for i in range(n):
		grad_f = weight_gradient(f, n_w0, w0, grad_f)
		for j in range(n_w0):
			w0[j].layer = w0[j].layer-eta*grad_f[j].layer
			layer_n = w0[j].n
			layer_w = w0[j].w
			weights = w0[j].layer.data.numpy()
			w0[j] = []
			w0[j] = w_layer(layer_n, layer_w, weights)
	return w0

if __name__=="__main__":
	xor_ws0 = []
	weights1 = [[0.0, -0.284227, 1.16054], [0.0, 0.617194, 1.30467]]
	weights2 = [[0.0, -0.084395, 0.648461]]
	xor_ws0.append(w_layer(ELEMENTS_LAYER1, WEIGHTS, weights1))
	xor_ws0.append(w_layer(ELEMENTS_LAYER2, WEIGHTS, weights2))
	xor_ws0 = vanilla(error_on_dataset, LAYERS, xor_ws0, 100000, 0.3);
	error = error_on_dataset(LAYERS, xor_ws0)
	print '{0:.18f}'.format(error.data[0].numpy())
