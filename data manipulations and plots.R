#####loading packages needed#####

library(tidyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(car)
library(Hmisc)

#####preparing rut data#####

rut <- read.delim("~/RUM data/Rut_observations.txt")

unique1 <- unique(rut[c("StagCode")])
datapoints1 <- aggregate(data.frame(count=rut$StagCode), list(value = rut$StagCode), length)
sum(is.na(rut$BirthYear))
rut.1<-rut %>% drop_na(BirthYear)
sum(is.na(rut$StagCode))
sum(is.na(rut$Date))
sum(is.na(rut$Day))
sum(is.na(rut$Month))
sum(is.na(rut$Year))
view(rut.1)

#removing males with poor data
rut.1=filter(rut.1, !(StagCode %in% c("M0776", "M0790","M1176","M1183","M1390","M1490","M1791","M2191","M2385","M2690","M3190","M3291","M3293","M3491","M3493","M3495","M3590","M3695","M3894","M4094","M4385","M4887","M0686","M238S","M438S")))
rut.1=filter(rut.1, !(StagCode %in% c("Y0381","Y1284","Y1295","Y1386","Y1488","Y1493","Y1499","Y1585","Y1590","Y1688","Y1690","Y1794","Y1890","Y1894","Y1994","Y2184","Y2294","Y2295","Y2392","Y2493","Y2592","Y2793","Y2888","Y2892","Y2984","Y3192","Y3291","Y3292","Y3592","Y3690","Y3691","Y4090","Y4390","Y4690","Y4990")))

rut.1$Age <- (rut.1$Year-rut.1$BirthYear)
rut.1$ordinal<-yday(dmy(rut.1$Date))
view(rut.1)
rut.1$yearcode<-paste(rut.1$Year,rut.1$StagCode)
rutmin<-tapply(rut.1$ordinal, rut.1$yearcode, min) 
rutmax<-tapply(rut.1$ordinal, rut.1$yearcode, max)
rutyear<-substring(names(rutmax),1,4) 
rutcode<-substring(names(rutmax),6,20)
rutdata<-as.data.frame(cbind(rutmin,rutmax,rutyear,rutcode,names(rutmin)))
view(rutdata) 
colnames(rutdata)[colnames(rutdata)=="V5"]<-"yearcode" 
str(rutdata)
head(rutdata)
rutdata$Age<-rut.2$Age[match(rutdata$yearcode, rut.1$yearcode)]
view(rutdata)
write.table(rutdata,"edited rut data.txt", sep="\t", row.names = FALSE)

#####preparing antler mass data#####

sum(is.na(museum$Weight))
museum<-museum %>% drop_na(Weight)

sum(is.na(rum$RumWeight))
rum<-rum %>% drop_na(RumWeight)

museum$yearcode<-paste(museum$CastYear,museum$Code)
rum$yearcode<-paste(rum$CastYear,rum$Code)

AvgWeightM<-tapply(museum$Weight, museum$yearcode, mean) 
AvgWeightR<-tapply(rum$RumWeight, rum$yearcode, mean)
yearM<-substring(names(AvgWeightM),1,4)
codeM<-substring(names(AvgWeightM),6,20)
yearR<-substring(names(AvgWeightR),1,4)
codeR<-substring(names(AvgWeightR),6,20)

museumavg<-as.data.frame(cbind(AvgWeightM, yearM, codeM, names(AvgWeightM)))
view(museumavg)
colnames(museumavg)[colnames(museumavg)=="V4"]<-"yearcode" 
rumavg<-as.data.frame(cbind(AvgWeightR, yearR, codeR, names(AvgWeightR)))
view(rumavg)
colnames(museumavg)[colnames(museumavg)=="yearcodeM"]<-"yearcode"

#merging dataframes

total<-merge(museumavg, rumavg, by="yearcode")
view(total)

#correlation between rum and museum weights

sum(is.na(total$AvgWeightR))
total$AvgWeightR<-as.numeric(total$AvgWeightR)
total[!is.na(total$AvgWeightR), ]

cor.test(total$AvgWeightR, total$AvgWeightM)
install.packages("ggpubr")
library(ggpubr)
ggscatter(total, x="AvgWeightR", y="AvgWeightM", add="reg.line", conf.int=TRUE, cor.coef=TRUE, cor.methods="pearson", xlab="antler masses on rum (g)", ylab="antler masses in the museum (g)")

#####plotting means per year, and calculating regressions, for antler mass and rut start#####

avgmass<-ggplot(setDT(rutdata)[,.(AverageMass = mean(antler.mass)), by=CastYear], aes(x=CastYear, y=AverageMass))+geom_point()+ggtitle("Temporal trends in antler mass from 2012 to 2019") +xlab("year") + ylab("average antler mass (g)") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot(avgmass)

massreg<-lm(antler.mass~CastYear, data=rutdata) #antler mass regression
summary(massreg)
confint(massreg)

avgrutmin<-ggplot(setDT(rutdata)[,.(AverageRutmin = mean(Rutmin)), by=Year], aes(x=Year, y=AverageRutmin))+geom_point()+ggtitle("Temporal trends in rut start from 2012 to 2019") +xlab("year") + ylab("average rut start date (ordinal days)") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot(avgrutmin)

rutreg<-lm(antler.mass~CastYear, data=rutdata) #rut start regression
summary(rutreg)
confint(rutreg)

#glm rutmin/max and year -- checking residuals

rutminres<-lm(rutmin~year, data=rutdata)
par(mfrow=c(2,2))
plot(rutminres)
par(mfrow=c(1,1))

antlerres<-lm(antler.mass~year, data=rut)
par(mfrow=c(2,2))
plot(antlerres)
par(mfrow=c(1,1))
