import common_hips_autograd as cha

def naive_euler(w):
        charges = [[10.0, 10.0-w], [10.0, 0.0]]
	x = [0.0, 8.0]
	xdot = [0.75, 0.0]
	delta_t = 1e-1
        def p(x):
                result = 0.0
                for c in charges:
                        result = result+1.0/cha.distance(x, c)
	        return result
	while True:
		xddot = cha.ktimesv(-1.0, cha.gradient_F(p)(x))
		x_new = cha.vplus(x, cha.ktimesv(delta_t, xdot))
		if (x_new[1]<=0.0):
		        delta_t_f = -x[1]/xdot[1]
		        x_t_f = cha.vplus(x, cha.ktimesv(delta_t_f, xdot))
	                return x_t_f[0]*x_t_f[0]
		x = x_new
		xdot = cha.vplus(xdot, cha.ktimesv(delta_t, xddot))

w0 = 0.0
print cha.multivariate_argmin_R(lambda w_list: naive_euler(w_list[0]), [w0])[0]
