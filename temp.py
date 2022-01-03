import pandas as pd 

df = pd.read_csv("blackboxPenalty.dat")
for data in df.values:
    print(f"*TIR-samples, {data[0]}, {data[-3]}, {data[-3]}, {data[-3]}, {data[-3]}, {data[-3]}")
    print(f"*TIR-dim, {data[0]}, {data[-2]}, {data[-2]}, {data[-2]}, {data[-2]}, {data[-2]}")
    print(f"*TIR-points, {data[0]}, {data[-1]}, {data[-1]}, {data[-1]}, {data[-1]}, {data[-1]}")
