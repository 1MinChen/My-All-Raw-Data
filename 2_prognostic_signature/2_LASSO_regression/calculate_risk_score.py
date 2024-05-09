import numpy as np
import pandas as pd
df = pd.read_csv("./GEO.expTime.txt", header=0, index_col=0, sep="\t")
genes = ["CAPN3", "HAVCR1", "GRIN2D", "BMI1", "SLC30A3", "GLP1R", "CSAG3", "NR4A3", "INPP4B", "ETV4", "MYH4", "ICA1", "ST6GALNAC4"]
coef = [-0.5974, 0.3376, 0.4161, 0.6536, 0.4733, 0.2293, 0.2211, -0.5401, -0.8607, 0.1339, -0.2406, -0.4394, 0.4485]
my_df = df.loc[:, genes]
risk_score = []
for row in range(my_df.shape[0]):
	score = 0
	for col in range(my_df.shape[1]):
		score += my_df.iloc[row, col]*coef[col]
	risk_score.append(round(score, 6))

df_time = df.loc[:, ["time", "status"]]
df_time["risk_score"] = risk_score

df_time.to_csv("./GEO_riskscore.txt", header=True, index=True, sep="\t")


