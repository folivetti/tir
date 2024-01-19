import numpy as np

np.random.seed(42)

sin_domain_train = (-np.pi/2, np.pi/2)
pagie_domain_train = (-4, 4)
kotanchek_domain_train = (-0.1, 3)

sin_domain = (-np.pi, np.pi)
pagie_domain = (-5, 5)
kotanchek_domain = (-0.2, 4)

n_samples = 300
tau = 0.2

def pagie(x,y):
    return 1/(1 + x**(-4)) + 1/(1 + y**(-4))

def kotanchek(x, y):
    return np.exp(-(x-1)**2)/(1.2 + (y - 2.5)**2)

def create_sin(domain, n_samples):
    x = np.random.uniform(*domain, n_samples)
    return (x, np.sin(x))

def create_pagie(domain, n_samples):
    x = np.random.uniform(*domain, n_samples)
    y = np.random.uniform(*domain, n_samples)
    return (x, y, pagie(x,y))

def create_kotanchek(domain, n_samples):
    x = np.random.uniform(*domain, n_samples)
    y = np.random.uniform(*domain, n_samples)
    return (x, y, kotanchek(x,y))

def add_noise(y, tau):
    std = np.std(y) * np.sqrt(tau/(1-tau))
    return y + np.random.normal(0, std, y.shape)

x = np.zeros((n_samples, 2))
x[:,0], x[:,1] = create_sin(sin_domain_train, n_samples)
np.savetxt("sin.csv", x, delimiter=",")
x[:,-1] = add_noise(x[:,-1], tau)
np.savetxt("sin_noise.csv", x, delimiter=",")
x[:,0], x[:,1] = create_sin(sin_domain, n_samples)
np.savetxt("sin_test.csv", x, delimiter=",")

x = np.zeros((n_samples, 3))
x[:,0], x[:,1], x[:,2] = create_pagie(pagie_domain_train, n_samples)
np.savetxt("pagie.csv", x, delimiter=",")
x[:,-1] = add_noise(x[:,-1], tau)
np.savetxt("pagie_noise.csv", x, delimiter=",")
x[:,0], x[:,1], x[:,2] = create_pagie(pagie_domain, n_samples)
np.savetxt("pagie_test.csv", x, delimiter=",")

x = np.zeros((n_samples, 3))
x[:,0], x[:,1], x[:,2] = create_kotanchek(kotanchek_domain_train, n_samples)
np.savetxt("kotanchek.csv", x, delimiter=",")
x[:,-1] = add_noise(x[:,-1], tau)
np.savetxt("kotanchek_noise.csv", x, delimiter=",")
x[:,0], x[:,1], x[:,2] = create_kotanchek(kotanchek_domain, n_samples)
np.savetxt("kotanchek_test.csv", x, delimiter=",")
