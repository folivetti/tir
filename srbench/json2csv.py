import glob
import json

for fname in glob.glob('results_test/*TIR*.json'):
    if '_cv_' in fname:
        continue
    with open(fname, "r") as read_file:
        dat = json.load(read_file)
        print(fname.split('/')[-1].split('_')[0], dat['r2_test'])
