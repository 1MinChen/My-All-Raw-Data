library(genefilter)
library(GSVA)
library(Biobase)
library(stringr)


data = read.table("./TCGA_ESCA_exp.txt", header = T, row.names=1, sep="\t")
gene_set = read.table("./ssGSEA_gene_set.txt", header=T, sep="\t")
gene_set = gene_set[, 1:2]
list = split(as.matrix(gene_set)[,1], gene_set[,2])
gsva_matrix = gsva(as.matrix(data), list, method='ssgsea', kcdf='Gaussian', abs.ranking=TRUE)
write.table(gsva_matrix, "./ssGSEA_score.txt", row.names=T, col.names=T, sep="\t", quote=F)



