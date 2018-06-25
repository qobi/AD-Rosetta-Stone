import common_hips_autograd as cha

start = [1.0, 1.0]

def f(x1, y1, x2, y2):
        return (x1*x1+y1*y1)-(x2*x2+y2*y2)

x1_star_y1_star = cha.multivariate_argmin_R(
        lambda x1_y1: cha.multivariate_max_F(
                lambda x2_y2: f(x1_y1[0], x1_y1[1], x2_y2[0], x2_y2[1]), start),
        start)

x1_star = x1_star_y1_star[0]

y1_star = x1_star_y1_star[1]

x2_star_y2_star = cha.multivariate_argmax_F(
        lambda x2_y2: f(x1_star, y1_star, x2_y2[0], x2_y2[1]), start)

x2_star = x2_star_y2_star[0]

y2_star = x2_star_y2_star[1]

print x1_star, y1_star, x2_star, y2_star
