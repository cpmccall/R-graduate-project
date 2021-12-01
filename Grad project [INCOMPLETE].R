library(readxl)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(colorspace)
library(ggpubr)
setwd("C:/Users/Friend Computer/Desktop/e6_intro2R/R graduate project")
list.files()
A=read_xlsx("Values for R project.xlsx")
#experiment conducted only used wells in rows A through E
Asub=A[1:5,]
head(Asub)
colnames(Asub)=c("placeholder","Urine.only","0mg/dl","40mg/dl","80mg/dl","120mg/dl",
                                   "160mg/dl","200mg/dl","240mg/dl","280mg/dl","320mg/dl","360mg/dl",
                                   "Empty.well")
Asub2=Asub[,-1]
Asub2
Asub3=Asub2
Asub2
K.pnu=as.data.frame(Asub2)
rownames(K.pnu)=c("A","B","C","D","E")
summary(K.pnu)
#I'd like to reorder each of these columns from greatest values to least values, I'm going to do one column manually,
##write a custom function for the next, and use a for loop to run that function across the rest of the columns
Urine.only=Asub2[,1]
Urine.only
Urine.only2=arrange(.data=Urine.only, desc(Urine.only))
Asub3$Urine.only=Urine.only2$Urine.only
Asub3

sort.data <- function(x){sorted <- apply(x, 2, sort, decreasing=T);return(sorted)}
sorted.df <- data.frame(matrix(nrow = nrow(Asub3), ncol = length(Asub3)))
names(sorted.df) <- names(Asub3)
Asub3 <- as.data.frame(Asub3)
for(i in 1:length(Asub3)){values <- as.vector(Asub3[,i]);sorted.df[,i] <- sort(values, decreasing=T)}
K.pnu=sorted.df
#What is the average absorbance value for each column?
K.averages=apply(X=K.pnu,MARGIN=2,FUN=mean)
#what is the standard deviation?
K.stdev=apply(X=K.pnu,MARGIN=2,FUN=sd)
K.stdev2=as.data.frame(x=K.stdev)
colnames(K.stdev2)=c("stdev")
K.stdev2
K.averages2=as.data.frame(x=K.averages)
colnames(K.averages2)=c("average")
K.averages2
#I'd like to combine these two data frames for ease of usage
K.stats=cbind(K.stdev2,K.averages2)
#these two data frames give me a decent overview of the data
K.stats
K.pnu
#What was the mean absorbance amongst all of the wells? Let's leave out Empty.well and Urine.only, which are controls
K.pnu2=K.pnu[,-c(1,12)]
K.pnu2
K.pnu3=gather(data=K.pnu2,key=group,value=absorbance)
K.pnu3
K.pnu4=K.pnu3%>%group_by(group)%>%summarise(mean.absorbance=mean(absorbance))
K.pnu4
#considering the average absorbance of all the experimental groups was less than the average absorbance of
##Urine.only wells, which is a control without bacteria. It seems reasonable to conclude there was no biofilm growth
if_else(condition=K.pnu2>0.1168,true="yes",false="no")
#in fact, the vast majority of absorbance values were under the average of the negative control
###this was an unsuccessful experiment! We can still plot it and do data analysis on it though
K.plot=K.pnu3
K.plot2=K.stats
K.plot2=K.plot2[-c(1,12),]
K.plot2$glu.mg.dl=c(0,40,80,120,160,200,240,280,320,360)
K.plot2
K.plot
ggplot(data=K.plot2,aes(x=glu.mg.dl,y=average))+
  geom_point()+
  geom_line()+
  geom_smooth(method="lm")
ggplot(data=K.plot,aes(x=absorbance,))+
  geom_histogram()+
  facet_grid(.~group)
hist(x=K.plot2$average)
ggqqplot(K.plot2$average)
model.avg=aov(formula=average~glu.mg.dl,data=K.plot2)
model.avg
summary(model.avg)
model.stdev=aov(formula=stdev~glu.mg.dl,data=K.plot2)
model.stdev
summary(model.stdev)
model.absorb=aov(formula=absorbance~group,data=K.plot)
model.absorb
summary(model.absorb)
  

  
