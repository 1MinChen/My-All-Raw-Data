library(survival)
library(survminer)

df = read.table("F:/single_cox_data.txt", row.names=1, header=T, sep="\t")

P_arr = c()
P_arr[1] = 0
P_arr[2] = 0

for (i in 3:ncol(df)){
	sub_df = df[, c(1, 2, i)]
	sub_df = na.omit(sub_df)
	names(sub_df)<-c("time", "status", "Site1")
	result = coxph(Surv(time, status)~Site1 , data=sub_df)
	p_value = summary(result)$coef[1,5]
	P_arr[i] = p_value
}
P_df = data.frame(P=P_arr)
rownames(P_df) = colnames(df)
write.table(P_df, "F:/single_cox_p_value.txt", sep="\t", quote=F)



data = read.table("F:/risk_score.txt", header=T, row.names=1, sep="\t")
result = coxph(Surv(time, status)~risk_score, data=data)

