import numpy as np
import matplotlib.pyplot as plt 

lines = open("log/airfoil/front.csv").readlines()

def extract_objs(x):
    objs = x.split()[0].split(',')
    return float(objs[0][1:]), float(objs[1][:-1])

zs = [extract_objs(z) for z in lines]
x = [z[0] for z in zs]
y = [z[1] for z in zs]

plt.plot(x, y, '.')
plt.show()
