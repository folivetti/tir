import numpy as np
import pyTIR as tir

Z = np.loadtxt("datasets/airfoil/airfoil-train-0.dat", delimiter=",")
clr = tir.TIRRegressor(100,100,1.0, 0.25, (-2,2), penalty=0.01)
clr.fit(Z[:,:-1], Z[:,-1])
yhat = clr.predict(Z[:,:-1])

mse = np.square(yhat - Z[:,-1]).mean()
print("Fitness should be approx.: ", np.sqrt(mse))
print(clr.expr)
print(clr.len)
