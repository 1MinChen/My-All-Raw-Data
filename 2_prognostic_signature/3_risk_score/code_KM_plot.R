library(survival)
library(survminer)

df = read.table("./ESCA_riskscore_KM_total.txt", row.names=1, header=T, sep="\t")

set.seed(1234)
sample = sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.6, 0.4))  #70%作为训练集，30%作为测试集
train = df[sample, ]
test = df[!sample, ]

fit = survfit(Surv(time, status)~Type, data=train)
ggsurvplot(fit,
	data = train,
	conf.int = TRUE,
	pval = TRUE,
	surv.median.line = "hv",
	risk.table = TRUE,
	xlab = "Time(days)",
	palette = c("tomato", "deepskyblue"),
	legend.labs = c("High risk", "Low risk"),
	legend.title = "Risk",
	break.x.by = 500,
	pval.size = 7,
	font.legend = 12
)

result = coxph(Surv(time, status)~risk_score , data=df)


df = read.table("F:/risk_score.txt", row.names=1, header=T, sep="\t")
fit = survfit(Surv(time, status)~type, data=df)
ggsurvplot(fit,
	data = df,
	pval = TRUE,
	conf.int = TRUE,
	surv.median.line = "hv",
	risk.table = TRUE,
	xlab = "Time(days)",
	ylab = "Survival rate",
	palette = c("skyblue", "tomato"),
	legend.title = "Risk",
	legend.labs = c("Low risk", "High risk"),
	break.x.by = 500,
	pval.size = 7,
	font.legend = 20
)
