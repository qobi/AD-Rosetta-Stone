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

def gradient_R(f):
        return grad(f)

def gradient_ascent_F(f, x0, n, eta):
        for i in range(n):
                x0 = vplus(x0, ktimesv(eta, gradient_F(f)(x0)))
        return x0

def gradient_ascent_R(f, x0, n, eta):
        for i in range(n):
                x0 = vplus(x0, ktimesv(eta, gradient_R(f)(x0)))
        return x0

def multivariate_argmin_F(f, x):
	g = gradient_F(f)
	fx = f(x)
	gx = g(x)
	eta = 1e-5
	i = 0
	x_star = np.zeros(len(x))
	for j in range(len(x)):
		x_star[j] = x[j]
	while True:
		if magnitude(gx)<1e-5:
			return x_star
		if i==10:
			eta = 2.0*eta
			i = 0
			continue
		x_prime = vminus(x_star, ktimesv(eta, gx))
		if distance(x_star, x_prime)<=1e-5:
			return x_star
		fx_prime = f(x_prime)
		if fx_prime<fx:
			x_star = x_prime
			fx = fx_prime
                        gx = g(x_prime)
			i = i+1
			continue
		eta = eta/2.0
		i = 0

def multivariate_argmax_F(f, x):
        return multivariate_argmin_F(lambda x: -f(x), x)

def multivariate_max_F(f, x):
        return f(multivariate_argmax_F(f, x))

def multivariate_argmin_R(f, x):
	g = gradient_R(f)
	fx = f(x)
	gx = g(x)
	eta = 1e-5
	i = 0
	x_star = np.zeros(len(x))
	for j in range(len(x)):
		x_star[j] = x[j]
	while True:
		if magnitude(gx)<1e-5:
			return x_star
		if i==10:
			eta = 2.0*eta
			i = 0
			continue
		x_prime = vminus(x_star, ktimesv(eta, gx))
		if distance(x_star, x_prime)<=1e-5:
			return x_star
		fx_prime = f(x_prime)
		if fx_prime<fx:
			x_star = x_prime
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
