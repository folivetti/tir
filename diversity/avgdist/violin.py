#!/usr/bin/python3.10

import sys
import pandas as pd
from seaborn import violinplot
import matplotlib.pyplot as plt

def getdata(fname):
    f = open(fname)
    xs = list(map(float, f.readlines()))
    f.close()
    return xs

dataset = sys.argv[1]

algname = {'FS' : 'FS-TIR', 'GPTIR' : 'TIR', 'FS_noise' : 'FS (noise)', 'GPTIR_noise' : 'TIR (noise)' }
algorithm = []
avgdist = []
for alg in ['FS', 'GPTIR']:
    for typ in ['', '_noise']:
        xs = getdata(f'{dataset}_{alg}{typ}_avgdist.csv')
        algorithm += [algname[alg+typ]]*len(xs)
        avgdist += xs

df = pd.DataFrame( { 'algorithm' : algorithm, 'avgdist' : avgdist } )
plt.rcParams.update({'font.size': 14, 'pdf.fonttype' : 42, 'ps.fonttype' : 42})
violinplot(data=df, x = 'avgdist', y = 'algorithm')
plt.savefig(f'{dataset}_violin.pdf', bbox_inches='tight')
