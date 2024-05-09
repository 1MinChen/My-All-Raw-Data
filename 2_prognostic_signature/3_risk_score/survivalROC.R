library(survival)
library(survivalROC)

df = read.table("./ESCA_riskscore_KM_train.txt", header=T, row.names=1, sep="\t")
result1 = survivalROC(Stime=df$time, status=df$status, marker=df$risk_score, predict.time=365*1, method="KM")
result2 = survivalROC(Stime=df$time, status=df$status, marker=df$risk_score, predict.time=365*3, method="KM")
result3 = survivalROC(Stime=df$time, status=df$status, marker=df$risk_score, predict.time=365*5, method="KM")

plot(result1$FP, result1$TP, type="l",col="blue",xlim=c(0,1),ylim=c(0,1), xlab="1-Specificity", ylab="Sensitivity", main="Total dataset", lwd=2)
lines(result2$FP, result2$TP, type="l",col="green",xlim=c(0,1), ylim=c(0,1), lwd=2)
lines(result3$FP, result3$TP, type="l",col="red",xlim=c(0,1), ylim=c(0,1), lwd=2)
abline(0, 1, col="black", lty=2, lwd=2)
legend(0.5, 0.3, c(
	paste("AUC at 1 year=", round(result1$AUC, 3)),
	paste("AUC at 3 year=", round(result2$AUC, 3)),
	paste("AUC at 5 year=", round(result3$AUC, 3))
	),
	lty=1, lwd=3, col=c("blue", "green", "red"), cex=1.2)


