import common_hips_autograd as cha
import autograd.numpy as np

INNER = 2
OUTER = 2
TOTAL = INNER+OUTER

x1c = np.empty(OUTER)

def f(x):
	return np.subtract(np.add(np.multiply(x[0], x[0]),
                                  np.multiply(x[1], x[1])),
                           np.add(np.multiply(x[2], x[2]),
                                  np.multiply(x[3], x[3])))

def inner(x2):
	global x1c
	return -f(np.concatenate((x1c, x2), axis = 0))

def outer(x1):
	global x1c
	x1c = x1
	x2 = np.array([1.0, 1.0])
	fx, x2_star = cha.multivariate_argmin(inner, x2)
	return -fx

if __name__=="__main__":
	x1_start = np.array([1.0, 1.0], dtype = float)
	x2_start = np.array([1.0, 1.0], dtype = float)
	fx, x1_star = cha.multivariate_argmin(outer, x1_start)
	x1c = x1_star
	fx, x2_star = cha.multivariate_argmin(inner, x2_start)
	print '{0:.18f} {1:.18f} {2:.18f} {3:.18f}'.format(x1_star[0],
                                                           x1_star[1],
                                                           x2_star[0],
                                                           x2_star[1])
