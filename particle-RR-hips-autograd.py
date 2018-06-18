import common_hips_autograd as cha
import autograd.numpy as np
from autograd import grad

CONTROLS = 1
DIMS     = 2
CHARGES  = 2

def p(x, w):
	r1 = 1.0/cha.distance(x, np.array([10.0, 10.0-w[0]]))
	r2 = 1.0/cha.distance(x, np.array([10.0, 0.0]))
	return r1+r2

def naive_euler(w):
	x       = np.array([0.0, 8.0])
	xdot    = np.array([0.75, 0.0])
	delta_t = 1e-1
	grad_p = grad(p)
	while True:
		g = grad_p(x, w)
		xddot = cha.ktimesv(-1.0, g)
		t = cha.ktimesv(delta_t, xdot)
		x_new = cha.vplus(x, t)
		if (x_new[1]>0.0):
			x = x_new
			t = cha.ktimesv(delta_t, xddot)
			xdot = cha.vplus(xdot, t)
			continue
		delta_t_f = -x[1]/xdot[1]
		t = cha.ktimesv(delta_t_f, xdot)
		x_t_f = cha.vplus(x, t)
		return x_t_f[0]*x_t_f[0]

if __name__=="__main__":
	w0 = np.array([0.0])
	fx, w_star = cha.multivariate_argmin(naive_euler, w0)
	print '{0:.18f}'.format(w_star[0])
