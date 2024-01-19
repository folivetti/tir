import numpy as np
import matplotlib.pyplot as plt 

def check_dominance(p1,p2):

    flag1 = 0
    flag2 = 0

    for o1,o2 in zip(p1,p2):
        if o1 < o2:
            flag1 = 1
        elif o1 > o2:
            flag2 = 1

    if flag1==1 and flag2 == 0:
        return 1
    elif flag1==0 and flag2 == 1:
        return -1
    else:
        return 0

def front(obj1,obj2):
    """return indices from x and y that are on the Pareto front."""
    rank = []
    assert(len(obj1)==len(obj2))
    n_inds = len(obj1)
    front = []

    for i in np.arange(n_inds):
        p = (obj1[i],obj2[i])
        dcount = 0
        dom = []
        for j in np.arange(n_inds):
            q = (obj1[j],obj2[j])
            compare = check_dominance(p,q)
            if compare == 1:
                dom.append(j)
#                 print(p,'dominates',q)
            elif compare == -1:
                dcount = dcount +1
#                 print(p,'dominated by',q)

        if dcount == 0:
#             print(p,'is on the front')
            front.append(i)

#     f_obj1 = [obj1[f] for f in front]
    f_obj2 = [obj2[f] for f in front]
#     s1 = np.argsort(np.array(f_obj1))
    s2 = np.argsort(np.array(f_obj2))
#     front = [front[s] for s in s1]
    front = [front[s] for s in s2]

    return front

lines = open("log/airfoil/front.csv").readlines()

def extract_objs(x):
    objs = x.split()[0].split(',')
    return float(objs[0][1:]), float(objs[1][:-1])

zs = [extract_objs(z) for z in lines]
x = np.array([z[0] for z in zs])
y = np.array([z[1] for z in zs])

ix = front(x,y)

plt.plot(x[ix], y[ix], '.', markersize=20)
plt.xlabel("NMSE", fontsize=24)
plt.ylabel("Size", fontsize=24)
plt.xticks(fontsize=20)
plt.yticks(fontsize=20)
plt.tight_layout()
plt.show()
#plt.savefig("pareto.pdf")
