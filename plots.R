library(RColorBrewer)

colors<-c("#5d9cd6","#6ca844")

bp<-ggplot(data=df, aes(x=Sex, y=Length, fill=Sex))

bp+geom_boxplot()+
  geom_jitter(width=0.2)+
  scale_fill_manual(values=colors)+
  theme_classic()+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  coord_flip()+
  labs(y="length \\cm", x="", title="Boxplot of finger length by sex")
    
  

scat<-ggplot(data=df,aes(y=Length, x=Age))  

scat+geom_point(aes(color=Sex),size=4, shape=18) + theme_classic()+
  #stat_smooth(method = "lm", se=FALSE)+
  #facet_grid(Sex ~ .)
  geom_smooth(method="lm", se=FALSE, color="black", linetype="dashed")+
  scale_color_manual(values=colors)+
  theme_classic()+
  theme(axis.title.y = element_text(),   legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  labs(x="Age \\Years", y="Finger length \\cm", title = "Finger length against age") 

hist<-ggplot(data=df,aes(x=Length, fill=Sex))

hist + geom_histogram(bins =10, color="black")+
  labs(x="Finger length \\cm", y="", title = "Finger length frequency by sex")+
  scale_fill_manual(values=colors)+
  theme_classic()+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=20))


correlation <- cor.test(df$Age,df$Length)

ttest<- t.test(Length~Sex, data = df, mu=0)

ftest<-var.test(Length~Sex,df, alternative="two.sided")


                