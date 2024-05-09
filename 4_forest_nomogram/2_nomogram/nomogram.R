library(rms)
library(survival)


data = read.table("F:/risk_score_multiple.txt", header=T, row.names=1, sep="\t")
dd = datadist(data)
options(datadist="dd")

f1 = lrm(status~age+gender+grade+stage+riskScore , data=data)
nom = nomogram(f1, fun=plogis, lp=F, funlabel="Risk")
plot(nom)

f1
Logistic Regression Model
 
 lrm(formula = status ~ age + gender + grade + stage + riskScore, 
     data = data)
 
                         Model Likelihood    Discrimination    Rank Discrim.    
                               Ratio Test           Indexes          Indexes    
 Obs            310    LR chi2      53.90    R2       0.215    C       0.728    
  0             182    d.f.             8    g        1.087    Dxy     0.456    
  1             128    Pr(> chi2) <0.0001    gr       2.965    gamma   0.456    
 max |deriv| 0.0001                          gp       0.227    tau-a   0.222    
                                             Brier    0.204                     
 
                 Coef    S.E.   Wald Z Pr(>|Z|)
 Intercept       -6.5880 1.5319 -4.30  <0.0001 
 age              0.0433 0.0132  3.29  0.0010  
 gender=male      0.4675 0.2672  1.75  0.0802  
 grade=G2         1.4590 1.1136  1.31  0.1901  
 grade=G3         1.5417 1.1071  1.39  0.1638  
 stage=Stage II   0.3250 0.4478  0.73  0.4680  
 stage=Stage III  1.0373 0.4338  2.39  0.0168  
 stage=Stage IV   1.8330 0.5645  3.25  0.0012  
 riskScore        0.6405 0.1440  4.45  <0.0001
####################################################################################C-index为0.728


#################################################################
## 构建COX比例风险模型
f2 = psm(Surv(time,status)~age+gender+stage+grade+riskScore, data=data, dist='lognormal')
med <- Quantile(f2)                               # 计算中位生存时间
surv <- Survival(f2)                              # 构建生存概率函数

## 绘制COX回归中位生存时间的Nomogram图
nom <- nomogram(f2, fun=function(x) med(lp=x),funlabel="Median Survival Time")
plot(nom)
############################################################################

nom <- nomogram(f2, fun=list(function(x) surv(365*1, x),
                            function(x) surv(365*3, x),
                            function(x) surv(365*5, x)),
                            funlabel=c("1-year Survival Probability", "3-years Survival Probability", "5-years Survival Probability"))
plot(nom, xfrac=.2)





##################################################################################
data = read.table("F:/risk_score_multiple.txt", header=T, row.names=1, sep="\t")
dd = datadist(data)
options(datadist="dd")
f2 = psm(Surv(time,status)~age+gender+stage+grade+riskScore, data=data, dist='lognormal')
surv <- Survival(f2)
nom <- nomogram(f2, fun=list(function(x) surv(365*1, x),
                            function(x) surv(365*3, x),
                            function(x) surv(365*5, x)),
                            funlabel=c("1-year Survival Probability", "3-years Survival Probability", "5-years Survival Probability"))
plot(nom, xfrac=.2)
################################################################################################################


##构建校准曲线
##time.in 和 u 要是一样的，都是要评价的时间节点
coxm_1 = cph(Surv(time,status)~risk_score+pathologic_T+pathologic_N+pathologic_M+tumor_stage+alcohol_history+cigarettes_per_day+Age+barretts_esophagus, data=data, surv=T, x=T, y=T, time.inc=365*1)
cal_1 = calibrate(coxm_1, u=365*1, cmethod='KM', m=54, B=1000)
##绘制1年生存期校准曲线
par(mar=c(7,4,4,3),cex=1.0)
plot(cal_1,lwd=2,lty=1,                                          #设置线条形状和尺寸
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),           #设置一个颜色
     xlab='Nomogram-Predicted Probability of 1-Year OS',         #便签
     ylab='Actual 1-Year OS(Proportion)',                        #标签
     col=c(rgb(192,98,83,maxColorValue = 255)),                  #设置一个颜色
     xlim = c(0,1),ylim = c(0,1))                                #x轴和y轴范围











