library(ggplot2)
library(dplyr)
library(readr)

df <- read_csv("ringData.csv")
df1<-filter(df, df$Length>60)
#df<-df1

colors<-c("#5d9cd6","#6ca844")
line_colors<-c("#003399", "#006600")


bp<-ggplot(data=df1,aes(x=Sex,y=Length, fill=Sex, color=Sex))  

bp+geom_boxplot()+
  geom_jitter(width=0.1)+
  scale_fill_manual(values=colors)+
  scale_color_manual(values=line_colors)+
  theme_classic()+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  coord_flip()+
  labs(y="length \\cm", x=" ", title="Boxplot of finger length by sex")

    

scat<-ggplot(data=df1,aes(y=Length, x=Age))  

scat+geom_point(aes(color=Sex),size=4, shape=18) + theme_classic()+
  #facet_grid(Sex ~ .)+
  geom_smooth(method="lm", se=FALSE, color="black", linetype="dashed")+
  scale_color_manual(values=colors)+
  theme_classic()+
  theme(axis.title.y = element_text(),   legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  labs(x="Age \\Years", y="Finger length \\cm", title = "Finger length against age") 


hist<-ggplot(data=df1,aes(x=Length, fill=Sex, color=Sex, y=..density..))

##hist with facets#
hist + 
  geom_histogram(binwidth=4, size = 1)+
  labs(x="Finger length \\mm", y="frequency", title = "Finger length frequency by sex")+
  scale_fill_manual(values=colors)+
  scale_color_manual(values=line_colors)+
  theme_classic()+
  geom_density(alpha=0.2, size=1)+
  xlim(55,90)+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=20))+
  facet_grid(Sex ~.)

## interleaved hist ##

hist +
  geom_histogram(binwidth =4, position = "dodge", size = 1)+
  labs(x="Finger length \\mm", y="", title = "Finger length frequency by sex")+
  scale_fill_manual(values=colors)+
  scale_color_manual(values=line_colors)+
  theme_classic()+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=20))
  


correlation <- cor.test(df$Age,df$Length)

ttest<- t.test(Length~Sex, data = df, var.equal=TRUE, mu=0)

ftest<-var.test(Length~Sex,df, alternative="two.sided")

cor(df$Age, df$Length)

?t.test

x <- seq(50, 100, length=1000)
y <- dnorm(x, mean=mean(df$Length), sd=sd(df$Length))
plot(x, y, type="l", lwd=1)
