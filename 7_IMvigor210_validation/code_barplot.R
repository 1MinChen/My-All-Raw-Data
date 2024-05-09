library(ggplot2)
library(dplyr)

df = read.table("./IMvigor210_risktype.txt", header=T, row.names=1, sep="\t")
bar = df %>% group_by(binaryResponse, RiskType) %>% count()
write.table(bar, "./IMvigor210_barplot.txt", col.names=T, row.names=T, quote=F, sep="\t")

p = ggplot(data=bar, aes(x=binaryResponse, y=n, fill=RiskType))+ 
geom_bar(stat="identity", position=position_fill())+
scale_fill_manual(values=c("orangered", "deepskyblue"))+theme_classic()+
theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(face="bold", size=12), axis.text.y=element_text(face="bold", size=12), legend.text=element_text(face="bold", size=12), legend.title=element_blank())
ggsave("IMvigor210_barplot.pdf", p, width=6, height=6)


