import autograd.numpy as np
from autograd import grad

def vplus(u, v):
	return np.add(u, v)

def vminus(u, v):
	return np.subtract(u, v)

def ktimesv(k, v):
	return np.multiply(k, v)

def magnitude_squared(x):
	return np.dot(x, x)

def magnitude(x):
	return np.sqrt(magnitude_squared(x))

def distance_squared(u, v):
	return magnitude_squared(vminus(u, v))

def distance(u, v):
	return np.sqrt(distance_squared(u, v))

def multivariate_argmin(f, x):
	eta = 1e-5
	i = 0
	fx = f(x)
	grad_f = grad(f)
	gx = grad_f(x)
	x_star = np.zeros(len(x))
	for j in range(len(x)):
		x_star[j] = x[j]
	while True:
		if magnitude(gx)< 1e-5:
			return [fx, x_star]
		if i==10:
			eta = eta*2.0
			i = 0
			continue
		t = ktimesv(eta, gx)
		x_prime = vminus(x_star, t)
		if distance(x_star, x_prime)<=1e-5:
			return [fx, x_star]
		fx_prime = f(x_prime)
		gx       = grad_f(x_prime)
		if fx_prime<fx:
			x_star = x_prime
			fx = fx_prime
			i = i+1
			continue
		eta = eta/2.0
		i = 0
