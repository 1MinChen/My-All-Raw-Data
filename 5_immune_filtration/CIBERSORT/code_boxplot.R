library(ggplot2)
library(ggpubr)

df = read.table("./CIBERSORT_immune_boxplot.txt", header=T, sep="\t")
ggplot(df, aes(x=reorder(Cell, -Score, sum), y=Score, fill=Type))+
scale_fill_manual(values=c("#63B8FF", "#FF6347"))+
geom_boxplot(outlier.size=0.1, width=0.3)+
theme_bw()+
stat_compare_means(aes(group=Type), label="p.signif", method="wilcox")+
theme(axis.text.x=element_text(angle=15, hjust=1, face="bold", size=10), axis.text.y=element_text(face="bold", size=10))



