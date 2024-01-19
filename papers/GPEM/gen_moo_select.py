import pandas as pd

f = open("points_dataset")
datasets = [l.strip() for l in f.readlines()]
f.close()

df = pd.read_csv("blackboxTIRMOO.csv")

dfTIR = df[(df.algorithm=="*TIRMOO") & ~(df.dataset.isin(datasets))]
dfTIRSel = df[(df.algorithm=="*TIRMOO-Select") & (df.dataset.isin(datasets))]

dfTIRSelPoints = pd.concat([dfTIR, dfTIRSel])
dfTIRSelPoints.algorithm = "*TIRMOO-Sel-points"

dfFinal = pd.concat([df, dfTIRSelPoints])
dfFinal.to_csv("gpem_results.csv", index=False)
