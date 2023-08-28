import json
import pandas as pd
import glob
import numpy as np

points = ["192_vineyard", "228_elusage", "485_analcatdata_vehicle", "1096_FacultySalaries", 
            "523_analcatdata_neavote", "663_rabe_266", "687_sleuth_ex1605", "659_sleuth_ex1714", 
            "678_visualizing_environmental", "611_fri_c3_100_5", "594_fri_c2_100_5", "624_fri_c0_100_5", 
            "656_fri_c1_100_5", "210_cloud", "706_sleuth_case1202", "1089_USCrime", 
            "712_chscase_geyser1", "542_pollution"]

for dataset in points:
    mse_test = []
    r2_test = []
    model_size = []
    time_time = []
    for fname in glob.glob(f"results_test/{dataset}*Penalty*.json"):
        with open(fname) as f:
            datum = json.load(f)
            mse_test.append(datum['mse_test'])
            r2_test.append(datum['r2_test'])
            model_size.append(datum['model_size'])
            time_time.append(datum['time_time'])
            print(datum['mse_test'])
    rmse = np.median(np.sqrt(mse_test))
    mse = np.median(mse_test)
    logmse = np.median(np.log(mse_test))
    r2 = np.median(r2_test)
    size = np.median(model_size)
    t = np.median(time_time)
    print(f"*TIR_points,{dataset},{rmse},{mse},{logmse},{r2},{size},{t}")
