import numpy as np
from pyTIR import TIRRegressor, TIRClassifier
from sklearn.datasets import load_iris
import sympy

Z = np.loadtxt("datasets/airfoil-train-0.dat", delimiter=",")
#Z = np.loadtxt("datasets/bazie.csv", delimiter=",")

clr = TIRRegressor(100,100,1.0, 0.25, (-2,2), max_time=5, penalty=0.01, alg='MOO', error='RMSE')
clr.fit(Z[:,:-1], Z[:,-1])
yhat = clr.predict(Z[:,:-1])

mse = np.square(yhat - Z[:,-1]).mean()
print("Fitness should be approx.: ", np.sqrt(mse))
print(clr.expr)
print(clr.len)
for e in clr.front:
    print(e)
print(sympy.sympify(clr.sympy))

clr2 = clr.create_model_from(3)
print("Tst:")
print(clr2.expr)
print(clr2.sympy)
print(clr2.len)
print(np.sqrt(np.square(clr2.predict(Z[:,:-1], Z[:,-1])).mean()))
"""
Z = np.loadtxt("datasets/breast-train.dat", delimiter=",")
clr = TIRClassifier(100,100,1.0, 0.25, (-2,2), penalty=0.01, niter=10)
clr.fit(Z[:,:-1], Z[:,-1])
yhat = clr.predict_proba(Z[:,:-1])

print("Fitness should be approx.: ", clr.score(Z[:,:-1], Z[:,-1]))
print(clr.expr)
print(clr.len)

X, y = load_iris(return_X_y=True)
clr = TIRClassifier(200,200, 0.7, 0.4, (0,3), penalty=0.0, niter=20)
clr.fit(X, y)
print("Iris score should be: ", clr.score(X, y))
for e in clr.expr.split("#"):
    print(e)
"""
