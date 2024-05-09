library(survival)
library(survminer)

df = read.table("./IMvigor210_KM_data.txt", row.names=1, header=T, sep="\t")
fit = survfit(Surv(time, status)~RiskType, data=df)
ggsurvplot(fit,
	data = df,
	pval = TRUE,
	conf.int = TRUE,
	surv.median.line = "hv",
	risk.table = TRUE,
	xlab = "Time(days)",
	ylab = "Survival rate",
	palette = c("deepskyblue", "orangered"),
	legend.title = "Risk",
	legend.labs = c("Low risk", "High risk"),
	break.x.by = 500,
	pval.size = 7,
	font.legend = 20
)
