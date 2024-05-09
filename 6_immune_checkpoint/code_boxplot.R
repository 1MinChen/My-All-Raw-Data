library(ggplot2)
library(ggpubr)
##########################################################################################
# 画两组TIDE得分的箱线图
TIDE_df = read.table("./ESCA_TIDE_result.txt", header=T, sep="\t")
TIDE_df = subset(TIDE_df, select=c("ID", "TIDE"))
risk_df = read.table("./riskscore_phenotype.txt", header=T, sep="\t")
risk_df = subset(risk_df, select=c("ID", "RiskType"))
df = merge(x=TIDE_df, y=risk_df, by="ID")
write.table(df, "./TIDE_boxplot.txt", col.names=T, row.names=T, quote=F, sep="\t")
p = ggplot(df, aes(x=RiskType, y=TIDE, fill=RiskType, color=RiskType))+
scale_color_manual(values=c("orangered", "deepskyblue"))+
scale_fill_manual(values=c("orangered", "deepskyblue"))+
geom_violin(width=0.3, alpha=0.6)+
geom_boxplot(width=0.2, fill="white")+
geom_jitter(width=0.15, shape=21, size=1, aes(fill=RiskType))+
geom_signif(comparisons=list(c("Low", "High")), textsize=5, test=wilcox.test, step_increase=0.2, map_signif_level=F)+
theme_bw()+
theme(axis.text=element_text(size=15, face="bold"), axis.title.y=element_text(size=15, face="bold"), axis.title.x=element_blank(), legend.position="none")
ggsave("TIDE_boxplot.pdf", p, width=6, height=6)
###########################################################################################
# 画两组免疫检查点基因的箱线图
df = read.table("./ESCA_checkpoint_boxplot.txt", header=T, sep="\t")
p = ggplot(df, aes(x=reorder(Gene, -Exp, sum), y=Exp, fill=RiskType))+
scale_fill_manual(values=c("orangered", "deepskyblue"))+
geom_boxplot(outlier.size=0.1, width=0.3)+
theme_bw()+
stat_compare_means(aes(group=RiskType), label="p.signif", method="t.test")+
theme(axis.text.x=element_text(angle=15, hjust=1, face="bold", size=10), axis.text.y=element_text(face="bold", size=10))
ggsave("ESCA_checkpoint_boxplot.pdf", p, width=6, height=6)






