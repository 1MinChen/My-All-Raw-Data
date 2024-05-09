library(ggplot2)

data = read.table("./DEG_limma.txt", header=T, row.names=1, sep="\t")
data$change <- as.factor(ifelse(data$adj.P.Val<0.05 & abs(data$logFC)>0.5,ifelse(data$logFC>0.5,'Up','Down'),'None'))

ggplot(data=data, aes(x=logFC, y=-log10(adj.P.Val), color=change)) +
geom_point(alpha=1, size=1.5) +
theme_bw() +
geom_hline(yintercept=-log10(0.05), linetype="dashed")+
geom_vline(xintercept=c(-0.5, 0.5), linetype="dashed")+
scale_color_manual(name="", values=c("chartreuse1", "lightgray", "chocolate1"), limits=c("Down", "None", "Up"))+
theme(axis.text.x=element_text(face="bold", size=15), axis.text.y=element_text(face="bold", size=15), legend.position="none", axis.title=element_text(face="bold", size=15))

########################################################################################
# 标示显著差异基因
data$sign <- ifelse(data$logFC>3 | data$logFC< -5, rownames(data), NA)

library(ggrepel)
ggplot(data=data, aes(x=logFC, y=-log10(adj.P.Val), color=change))+
geom_point(alpha=0.8, size=1.3)+
theme_bw()+
geom_hline(yintercept=-log10(0.05), linetype="dashed")+
geom_vline(xintercept=c(-0.5, 0.5), linetype="dashed")+
scale_color_manual(name="", values=c("chartreuse1", "lightgray", "chocolate1"), limits=c("Down", "None", "Up"))+
geom_label_repel(aes(label=sign), fontface="bold", color="grey50", box.padding=unit(0.35, "lines"), point.padding=unit(0.5, "lines"), segment.colour="grey50")






