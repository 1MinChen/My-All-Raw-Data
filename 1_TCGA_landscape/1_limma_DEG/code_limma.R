
library(limma)

exp = read.table("./TCGA_ESCA_coding_FPKM.txt", header=T, row.names=1, sep="\t")
design = read.table("./label.txt", header=T, row.names=1, sep="\t")

contrast.matrix = makeContrasts("Tumor-Normal", levels=design)
fit = lmFit(exp, design)
fit2 = contrasts.fit(fit, contrast.matrix)
fit3 = eBayes(fit2)
DEG = topTable(fit3, coef=1, n=Inf)
write.table(DEG, "./DEG_limma.txt", col.names=T, row.names=T, sep="\t", quote=F)

