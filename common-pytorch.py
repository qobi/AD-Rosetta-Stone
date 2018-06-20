import torch
import torch.autograd as grad

def gradient_ascent_R(f, x0, n, eta):
        x = [torch.tensor([x0[i]], requires_grad = True) for i in range(len(x0))]
	for i in range(n):
		f(x).backward()
                x = [torch.tensor((x[i]+eta*x[i].grad).data.numpy(), requires_grad = True) for i in range(len(x))]
	return [x[i].data.numpy()[0] for i in range(len(x))]
