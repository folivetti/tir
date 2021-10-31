import numpy as np
import tir_wrapper as tir

Z = np.loadtxt("datasets/airfoil/airfoil-train-0.dat", delimiter=",")
clr = tir.TiredRegressor(500,1000,1.0, 0.25, (-2,2))
clr.fit(Z[:,:-1], Z[:,-1])
yhat = clr.predict(Z[:,:-1])

mse = np.square(yhat - Z[:,-1]).mean()
print("Fitness should be approx.: ", np.sqrt(mse))
print(clr.expr)
print(clr.len)
