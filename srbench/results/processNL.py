import pandas as pd

df = pd.read_csv("resultsTIRNL.csv")
df[df.algorithm == "TIRNL"].groupby('dataset').median().to_csv("TIRNL.csv")
