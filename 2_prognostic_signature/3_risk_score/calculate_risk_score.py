import numpy as np
import pandas as pd
df = pd.read_csv("./ESCA_PCD_LASSO_data.txt", header=0, index_col=0, sep="\t")
df_coef = pd.read_csv("./LASSO_result_coefficient.txt", header=0, sep="\t")
genes = df_coef.Gene
coef = df_coef.coef
my_df = df.loc[:, genes]
risk_score = []
for row in range(my_df.shape[0]):
	score = 0
	for col in range(my_df.shape[1]):
		score += my_df.iloc[row, col]*coef[col]
	risk_score.append(round(score, 6))

df_time = df.loc[:, ["time", "status"]]
df_time["risk_score"] = risk_score
df_time.to_csv("./ESCA_riskscore.txt", header=True, index=True, sep="\t")

