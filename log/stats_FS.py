import json
import scipy.stats as st

def get_data(fname, field):
    with open(fname) as f:
        data = [json.loads(l)[field] for l in f.readlines()]
    return data
    

dirs = [["sin_GPTIR", "sin_FS_outer"], ["pagie_GPTIR", "pagie_FS_outer"], ["kotanchek_GPTIR", "kotanchek_FS_outer"]]
criteria = ["NMSE_train", "NMSE_test"]

for group in dirs:
    print(group, ": ")
    for c in criteria:
        print(c, ":")
        tir = get_data(f"{group[0]}/stats.json", c)
        fs  = get_data(f"{group[1]}/stats.json", c) 
        print("Normality: ", st.shapiro(tir), st.shapiro(fs))
        print("t-test: ", st.mstats.ttest_ind(tir, fs))
