import pandas as pd

df = pd.read_csv("resultsMoo2.csv")
df[df.algorithm == "TIRMOO2"].groupby('dataset').median().to_csv("TIRMOO.csv")

df = pd.read_csv("resultsMoo3.csv")
df[df.algorithm == "TIRMOO3"].groupby('dataset').median().to_csv("TIRMOO_Points.csv")

df = pd.read_csv("resultsMOO4.csv")
df[df.algorithm == "TIRMOO4"].groupby('dataset').median().to_csv("TIRMOO_Select.csv")
