import glob
import os
from yaml import load, Loader

count = 0
for fullname in glob.glob("pmlb/datasets/*"):
    dname = fullname.split("/")[-1]
    if count>10:
        break
    if 'strogatz' in dname or 'feynman' in dname or 'lupus' in dname or 'dataset_sizes' in dname:
        continue
    metadata = load(
                open(f'pmlb/datasets/{dname}/metadata.yaml','r'),
                Loader=Loader
        )
    print(dname)
    print (metadata['task'])
    if metadata['task'] != 'regression':
        continue
    for seed in [23654, 15795 ,860, 5390, 29802, 21575, 11964, 11284, 22118, 6265]:
        if os.path.exists(f'srbench/experiment/results_test/{dname}_TIRRegressor_{seed}.json'):
            continue

        print(f'{dname}_{seed}.job')
        count=count+1

        fw = open(f'{dname}_{seed}.job', 'w')
        print(f'''#!/bin/bash
source activate srbench
cd srbench/experiment
python evaluate_model.py ~/pmlb/datasets/{dname}/{dname}.tsv.gz -ml TIRRegressor -seed {seed}
''', file=fw)
        fw.close()

