import json
import glob
import pandas as pd
import numpy as np

algorithms = []
seeds = []
datanames = []
r2_tests = []
r2_trains = []
mse_tests = []
mse_trains = []
lens = []
times = []

files = glob.glob("results_test/*MOO4*.json")

for f in files:
    fields = f.split("/")[1].split("_")
    algorithms.append(fields[-2])
    seeds.append(fields[-1].split(".")[0])
    datanames.append("_".join(fields[0:-2]))
    results = json.load(open(f))
    r2_trains.append(results["r2_train"])
    r2_tests.append(results["r2_test"])
    mse_trains.append(results["mse_train"])
    mse_tests.append(results["mse_test"])
    lens.append(results["model_size"])
    times.append(results["time_time"])

#df = pd.DataFrame({"algorithm":algorithms, "dataset": datanames,  "r2_train":r2_trains, "r2_test":r2_tests, "mse_train":mse_trains, "mse_test":mse_tests, "model_size":lens})
df = pd.DataFrame({"algorithm":algorithms, "dataset": datanames,  "rmse_test":np.sqrt(mse_tests), "log_mse_test" : np.log(mse_tests), "r2_test":r2_tests, "model_size":lens, "training time (s)" : times} )
df.to_csv("resultsMOO4.csv", index=False)
