library(ggplot2)
library(dplyr)
library(readr)

df <- read_csv("ringData.csv")

colors<-c("#5d9cd6","#6ca844")


bp<-ggplot(data=df,aes(x=Sex,y=Length, fill=Sex, color=Sex))  

bp+geom_boxplot()+
  geom_jitter(width=0.2)+
  scale_fill_manual(values=colors)+
  scale_color_manual(values=c("#003399", "#006600"))+
  theme_classic()+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  coord_flip()+
  labs(y="length \\cm", x=" ", title="Boxplot of finger length by sex")
    

scat<-ggplot(data=df,aes(y=Length, x=Age))  

scat+geom_point(aes(color=Sex),size=4, shape=18) + theme_classic()+
  #stat_smooth(method = "lm", se=FALSE)+
  #facet_grid(Sex ~ .)
  geom_smooth(method="lm", se=FALSE, color="black", linetype="dashed")+
  scale_color_manual(values=colors)+
  theme_classic()+
  theme(axis.title.y = element_text(),   legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  labs(x="Age \\Years", y="Finger length \\cm", title = "Finger length against age") 


hist<-ggplot(data=df,aes(x=Length, fill=Sex, color=Sex))

##hist with facets#
hist + geom_histogram(bins =12, size = 1)+
  labs(x="Finger length \\cm", y="", title = "Finger length frequency by sex")+
  scale_fill_manual(values=colors)+
  scale_color_manual(values=c("blue", "dark green"))+
  theme_classic()+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  facet_grid(Sex ~.)

## interleaved hist ##

hist +
  labs(x="Finger length \\cm", y="", title = "Finger length frequency by sex")+
  scale_fill_manual(values=colors)+
  scale_color_manual(values=c("blue", "dark green"))+
  theme_classic()+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  geom_histogram(bins =12, position = "dodge", size = 1)


correlation <- cor.test(df$Age,df$Length)

ttest<- t.test(Length~Sex, data = df, var.equal=TRUE, mu=0)

ftest<-var.test(Length~Sex,df, alternative="two.sided")

cor(df$Age, df$Length)

?t.test

