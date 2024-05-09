# 在R语言中运行Cibersort共需要三个文件，分别是（1）官方提供的22种细胞基因集“LM22.txt”；（2）自己的表达矩阵；（3）Cibersort代码。
# 放在 E:/CIBERSORT

setwd("C:/my_file")
source("CIBERSORT.R")
TME_results = CIBERSORT("LM22.txt", "TCGA_ESCA_exp.txt", perm=1000, QN=T)
write.table(TME_results, "./CIBERSORT_result.txt", col.names=T, row.names=T, quote=F, sep="\t")

                 B cells naive B cells memory Plasma cells T cells CD8 T cells CD4 naive T cells CD4 memory resting
TCGA-3M-AB46-01A   0.046177856    0.000000000  0.008028555  0.04284880                 0                  0.2366685
TCGA-3M-AB47-01A   0.056924342    0.008184919  0.147497667  0.06825701                 0                  0.4068777
TCGA-B7-5818-01A   0.010262428    0.000000000  0.000000000  0.12778022                 0                  0.1030044
TCGA-B7-A5TI-01A   0.106546830    0.000000000  0.086348444  0.10305670                 0                  0.1864248
TCGA-B7-A5TJ-01A   0.050182934    0.000000000  0.019872516  0.03115561                 0                  0.1969234
TCGA-B7-A5TK-01A   0.002458429    0.000000000  0.038932255  0.35600831                 0                  0.0000000



