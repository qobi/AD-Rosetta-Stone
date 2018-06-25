import torch
from torch import sqrt
import torch.autograd as grad

def vplus(u, v):
        return [u[i]+v[i] for i in range(len(u))]

def vminus(u, v):
        return [u[i]-v[i] for i in range(len(u))]

def ktimesv(k, v):
        return [k*v[i] for i in range(len(v))]

def magnitude_squared(x):
        sum = 0
        for i in range(len(x)):
                sum = sum+x[i]*x[i]
	return sum

def magnitude(x):
	return sqrt(magnitude_squared(x))

def distance_squared(u, v):
	return magnitude_squared(vminus(u, v))

def distance(u, v):
	return sqrt(distance_squared(u, v))

def replace_ith(x, i, xi):
        return x[0:i]+[xi]+x[i+1:len(x)]

def gradient_R(f):
        def inner(x):
                x = [torch.tensor([x[i]], requires_grad = True)
                     for i in range(len(x))]
		f(x).backward()
                return [x[i].grad.data.numpy()[0] for i in range(len(x))]
        return inner

def gradient_ascent_R(f, x0, n, eta):
        for i in range(n):
                x0 = vplus(x0, ktimesv(eta, gradient_R(f)(x0)))
        return x0

def multivariate_argmin_R(f, x):
	g = gradient_R(f)
	fx = f(x)
	gx = g(x)
	eta = 1e-5
	i = 0
	while True:
		if magnitude(gx)<1e-5:
			return x
		if i==10:
			eta = 2.0*eta
			i = 0
			continue
		x_prime = vminus(x, ktimesv(eta, gx))
		if distance(x, x_prime)<=1e-5:
			return x
		fx_prime = f(x_prime)
		if fx_prime<fx:
			x = x_prime
			fx = fx_prime
                        gx = g(x_prime)
			i = i+1
			continue
		eta = eta/2.0
		i = 0

def multivariate_argmax_R(f, x):
        return multivariate_argmin_R(lambda x: -f(x), x)

def multivariate_max_R(f, x):
        return f(multivariate_argmax_R(f, x))
