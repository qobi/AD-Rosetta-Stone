import common_hips_autograd as cha
import autograd.numpy as np

x1c = np.empty(0)

def f(x):
        return (x[0]*x[0]+x[1]*x[1])-(x[2]*x[2]+x[3]*x[3])

def inner(x2):
	global x1c
	return f(np.concatenate((x1c, x2), axis = 0))

def outer(x1):
	global x1c
	x1c = x1
	return cha.multivariate_max_R(inner, np.array([1.0, 1.0]))

if __name__=="__main__":
	x1_star = cha.multivariate_argmin_R(outer, np.array([1.0, 1.0]))
	x1c = x1_star
	x2_star = cha.multivariate_argmax_R(inner, np.array([1.0, 1.0]))
	print '{0:.18f} {1:.18f} {2:.18f} {3:.18f}'.format(x1_star[0],
                                                           x1_star[1],
                                                           x2_star[0],
                                                           x2_star[1])
