library(tidyr)
library(ggplot2)
library(ggpubr)
df<-read.csv("agepara.csv")
head(df)

FacetPlot1 = ggplot(df, aes(x=Age, y=Parasitemia, color= Age, group=Age)) + 
  geom_boxplot() + facet_grid(~Day) +
  ggtitle("Age categoties")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust =0.5, face = "bold", size = 10))
FacetPlot1

library(scales)
df<-read.csv("hospara.csv")
head(df)
df$Day=as.factor(df$Day)
df$location=as.factor(df$location)
df$no=as.numeric(df$no)

str(df)
FacetPlot1 = ggplot(df, aes(x=location, y=no, color= Day, group=location)) + 
  geom_boxplot() + facet_grid(~Day) +
  ggtitle("Regional parasitemia")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust =0.5, face = "bold", size = 10))+
  #scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                #labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic()
FacetPlot1


p2<-read.csv("parasitemia.csv")
head(p2)

p2 <- ggplot(p2, aes(x = Day1, y = Day0)) + geom_point() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic()
# log-log plot without log tick marks
p2

p3<-read.csv("gametocytes.csv")
p3$gametocytes=as.factor(p3$gametocytes)

head(p3)
plot <- ggplot(p3, aes(x=gametocytes, y=Day0, group=gametocytes, color=gametocytes))+
  geom_boxplot()+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x))) +
  ylab("Parasitemia")+
  theme_classic()+
  theme(legend.position = "none")
plot1 <- ggplot(p3, aes(x=gametocytes, y=Day1, group=gametocytes, color=gametocytes))+
  geom_boxplot()+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ylab("Parasitemia")+
  theme_classic()+
  theme(legend.position = "center")


plot1
plot

ggarrange(plot, plot1, ncol = 2, nrow = 1, labels = c("Day0", "Day1", common.legend = "true", legend = "left"))

