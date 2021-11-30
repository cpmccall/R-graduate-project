library(readxl)
library(dplyr)
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
#I'd like to reorder each of these columns from greatest values to least values, I'm going to do one column manually,
##write a custom function for the next, and use a for loop to run that function across the rest of the columns
Urine.only=Asub2[,1]
Urine.only
Urine.only2=arrange(.data=Urine.only, desc(Urine.only))
Asub3$Urine.only=Urine.only2$Urine.only
Asub3
high2low=function(x,y){z=data.frame(y);a=arrange(.data=z,desc(z[,1]));x=cbind(x,new_col=a);return(x)}
Asub4=high2low(Asub3,Asub3[2])
Asub4
for(i in 3:12){b=high2low(Asub3,Asub3[i]);return(b)}
b



