import common_hips_autograd as cha
import autograd.numpy as np

CONTROLS = 1
DIMS     = 2
CHARGES  = 2

def p(x, w):
        result = 0.0
        for c in [[10.0, 10.0-w[0]], [10.0, 0.0]]:
                result = result+1.0/cha.distance(x, np.array(c))
	return result

def naive_euler(w):
	x = np.array([0.0, 8.0])
	xdot = np.array([0.75, 0.0])
	delta_t = 1e-1
	while True:
		xddot = cha.ktimesv(-1.0, cha.gradient_R(p)(x, w))
		x_new = cha.vplus(x, cha.ktimesv(delta_t, xdot))
		if (x_new[1]>0.0):
			x = x_new
			xdot = cha.vplus(xdot, cha.ktimesv(delta_t, xddot))
			continue
		delta_t_f = -x[1]/xdot[1]
		x_t_f = cha.vplus(x, cha.ktimesv(delta_t_f, xdot))
		return x_t_f[0]*x_t_f[0]

if __name__=="__main__":
	w_star = cha.multivariate_argmin_R(naive_euler, np.array([0.0]))
	print '{0:.18f}'.format(w_star[0])
