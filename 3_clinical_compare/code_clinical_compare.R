
# Age
df = read.table("./riskscore_phenotype.txt", row.names=1, header=T, sep="\t")
ggplot(df, aes(x=RiskType, y=cigarettes_per_day, color=RiskType, fill=RiskType))+
scale_color_manual(values=c("tomato", "deepskyblue"))+
scale_fill_manual(values=c("tomato", "deepskyblue"))+
geom_violin(alpha=0.7)+
geom_boxplot(fill="white", width=0.4)+
geom_jitter(width=0.15, shape=21, size=1, aes(fill=Type))+
geom_signif(comparisons=list(c("High", "Low")), textsize=5, test=wilcox.test, step_increase=0.2, map_signif_level=F)+
ylab("cigarettes_per_day")+
theme_bw()+
theme(text=element_text(family="Times"), axis.text=element_text(size=15, face="bold"), axis.title.y=element_text(size=15, face="bold"), axis.title.x=element_blank(), legend.position="none")

# tumor_stage
df = read.table("./riskscore_phenotype.txt", row.names=1, header=T, sep="\t")
sub_df = subset(df, select=c("RiskType", "tumor_stage"))
sub_df = na.omit(sub_df)
bar = sub_df %>% group_by(RiskType, tumor_stage) %>% count()
colors = c4a("carto.pastel", 7)
ggplot(data=bar, aes(x=RiskType, y=n, fill=tumor_stage))+ 
geom_bar(stat="identity", position=position_fill())+
scale_fill_manual(values=colors)+theme_bw()+
theme(text=element_text(family="Times"), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(face="bold", size=15), axis.text.y=element_text(face="bold", size=15), legend.text=element_text(face="bold", size=15), legend.title=element_blank(), legend.position="right")







