#为什么需要用 Lasso + Cox 生存分析模式一般我们在筛选影响患者预后的变量时，通常先进行单因素Cox分析筛选出关联的变量，然后构建多因素模型进一步确认变量与生存的关联是否独立。
#但这种做法没有考虑到变量之间多重共线性的影响，有时候我们甚至会发现单因素和多因素Cox回归得到的风险比是矛盾的，这是变量之间多重共线性导致模型失真的结果。
#并且，当变量个数大于样本量时，此时传统的Cox回归的逐步回归、前进法、后退法等变量筛选方法都不再适用。

library("survival")
library("glmnet")

data = read.table("./ESCA_PCD_LASSO_data.txt", header=T, row.names=1, sep="\t")
x = as.matrix(data[, 3:ncol(data)])
y = data.matrix(Surv(data$time,data$status))
fit = glmnet(x, y, family="cox", alpha=1)
plot(fit,xvar="lambda",label=T)
cv_fit = cv.glmnet(x, y, family="cox", alpha=1,nfolds=10)
plot(cv_fit)
result = coef(cv_fit, s="lambda.min")
my_result = as.data.frame(as.matrix(result))
colnames(my_result) = c("coef")
final_result = subset(my_result, my_result$coef>0)
write.table(final_result, "./LASSO_result_coefficient.txt", row.names=T, col.names=T, sep="\t", quote=F)

