library(DGEobj.utils)
library(IMvigor210CoreBiologies)
data(cds)
data = as.data.frame(counts(cds))    # 表达矩阵，值是count
annoData = fData(cds)                # 基因的注释信息
phenoData = pData(cds)               # 样本信息
data_TPM = convertCounts(data, "TPM", annoData$length, log=FALSE, normalize="none", prior.count=NULL)
data_TPM = log(data_TPM+1, base=2)
write.table(data_TPM, "data_TPM.txt", col.names=T, row.names=T, quote=F, sep="\t")
write.table(phenoData, "phenoData.txt", col.names=T, row.names=T, quote=F, sep="\t")
############################################# 






