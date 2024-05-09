###################################################### 计算出单变量cox回归显著的基因
library(survival)
library(survminer)
df = read.table("TCGA.expTime.txt", row.names=1, header=T, sep="\t")
P_arr = c()
P_arr[1] = 0
P_arr[2] = 0
for (i in 3:ncol(df)){
	sub_df = df[, c(1, 2, i)]
	sub_df = na.omit(sub_df)
	names(sub_df)<-c("futime", "fustat", "Site1")
	result = coxph(Surv(futime, fustat)~Site1 , data=sub_df)
	p_value = summary(result)$coef[1,5]
	P_arr[i] = p_value
}
P_df = data.frame(P=P_arr)
rownames(P_df) = colnames(df)
write.table(P_df, "uni_cox_sig_genes.txt", sep="\t", quote=F)

###################################################### 划分训练集和测试集
set.seed(1234)
df = read.table("TCGA_riskscore_all.txt", header=T, row.names=1, sep="\t")
sample = sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7, 0.3))  #70%作为训练集，30%作为测试集
train = df[sample, ]
test = df[!sample, ]

###################################################### 用训练集做LASSO cox 回归
library(glmnet)
library(survival)
data = data %>% filter(time>0)
x = as.matrix(data[, 3:ncol(data)])
y = as.matrix(data[, 1:2])
fit = glmnet(x, y, family="cox", alpha=1)                      #拟合模型，指定family="cox"
plot(fit, xvar="lambda", label=TRUE)                           #画log lambda 图
cv_fit = cv.glmnet(x, y, family="cox", alpha=1)                #交叉验证构建模型
plot(cv_fit)
result = coef(cv_fit, s="lambda.min")                          #挑选出CV最小时的lambda值

###################################################### 画风险得分的ROC曲线
library(survival)
library(survivalROC)

df = read.table("TCGA_riskscore_all.txt", header=T, row.names=1, sep="\t")
result1 = survivalROC(Stime=df$time, status=df$status, marker=df$risk_score, predict.time=1, method="KM")
result2 = survivalROC(Stime=df$time, status=df$status, marker=df$risk_score, predict.time=3, method="KM")
result3 = survivalROC(Stime=df$time, status=df$status, marker=df$risk_score, predict.time=5, method="KM")

plot(result1$FP, result1$TP, type="l", col="green", xlim=c(0,1), ylim=c(0,1), lwd=2, xlab="1-Specificity", ylab="Sensitivity")
lines(result2$FP, result2$TP, type="l", col="blue", xlim=c(0,1), ylim=c(0,1), lwd=2)
lines(result3$FP, result3$TP, type="l", col="red", xlim=c(0,1), ylim=c(0,1), lwd=2)
abline(0,1, col="black", lty=2, lwd=2)
legend(0.6, 0.3, c(paste("AUC at 1 years: ", round(result1$AUC, 3)),
	paste("AUC at 3 years: ", round(result2$AUC, 3)),
	paste("AUC at 5 years: ", round(result3$AUC, 3))),
	x.intersp=0.5, y.intersp=1,
	lty=1, lwd=3, col=c("green", "blue", "red"),
	seg.len=1.5, cex=1)








