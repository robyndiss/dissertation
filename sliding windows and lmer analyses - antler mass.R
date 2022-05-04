#####loading packages needed#####

library(tidyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lme4)
library(lmerTest)

#####preparing gridded data#####

trait.data<-read.table(file.choose(), header=TRUE)
View(trait.data)
trait.data$year.fac<-as.factor(trait.data$growth.year)
trait.data$cohort<-as.factor(trait.data$BirthYear)

weatherdata<-read.csv(file.choose(),header=TRUE)
View(weatherdata)

#####gridded rain window#####

yearbyrain<-matrix(nrow=(2019-2012)+1,ncol=175+1)

for (yearval in 2012:2019){ 
  yearbyrain[(yearval-2011),2:176]<-weatherdata [which(weatherdata$Year==yearval),"PrecipGrid"][97:271]
  
  yearbyrain[(yearval-2011),1]<-yearval}

colnames(yearbyrain)<-as.character(c("Year",seq(97,271,1)))
View(yearbyrain)
sum(is.na(yearbyrain))
write.table(yearbyrain,"ANTLERRAINGRID.csv",sep=",",col.names=TRUE,row.names=TRUE)
trait.data2<-cbind(trait.data2,yearbyrain[pmatch(trait.data$growth.year,yearbyrain[,"Year"],duplicates.ok=TRUE),2:176])
View(trait.data2)

##starting the sliding window##

str(trait.data2)
trait.data2$Yearf<-as.factor(trait.data2$growth.year)
trait.data2$yearcent<-trait.data2$growth.year-mean(trait.data2$growth.year)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+(1|Code),data=trait.data2,REML=FALSE)
summary(nullmodel)

store.opt<-c() 
store.slope<-c()
store.AIC<-c()
store.start<-c()
store.end<-c()
store.intercept<-c()
store.loglik<-c()
store.significance<-c()
store.SE<-c()
store.t<-c()
x<-0

for(startval in 97:271){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=271){
      x<-x+1
      
      trait.data2$rainred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) 
      rainmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+rainred+(1|Code),data=trait.data2,REML=FALSE)
      
      store.opt[x]<-tempmodel@optinfo$conv$opt
      store.AIC[x]<-AIC(rainmodel)
      store.slope[x]<-summary(rainmodel)$coefficients[6,1]
      store.start[x]<-startval
      store.end[x]<-endval
      store.intercept[x]<-summary(rainmodel)$coefficients[1,1]
      store.significance[x]<-summary(rainmodel)$coefficients[6,5]
      store.SE[x]<-summary(rainmodel)$coefficients[6,2]
      store.t[x]<-summary(rainmodel)$coefficients[6,4]
      store.loglik[x]<-logLik(rainmodel)
    }
  }
}

summary(rainmodel)

which(store.AIC==min(store.AIC))

store.AIC[7658] 
store.slope[7658]
store.start[7658]
store.end[7658]
store.intercept[7658]
store.significance[7658]
store.SE[7658]
store.t[7658]
AIC(nullmodel)

store.opt[7658]

alt.antler.mass.rain.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(alt.antler.mass.rain.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))
write.table(alt.antler.mass.rain.windows, "alt.antler.mass.rain.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

#####gridded temp window#####

yearbytemp<-matrix(nrow=(2019-2012)+1,ncol=175+1)

for (yearval in 2012:2019){ #could try running just for one year and see the output
  #select weather data for yearval 
  yearbytemp[(yearval-2011),2:176]<-weatherdata [which(weatherdata$Year==yearval),"TempGrid"][97:271]
  
  yearbytemp[(yearval-2011),1]<-yearval}

colnames(yearbytemp)<-as.character(c("Year",seq(97,271,1)))
View(yearbytemp)
sum(is.na(yearbytemp))
write.table(yearbytemp,"ANTLERTEMPGRID.csv",sep=",",col.names=TRUE,row.names=TRUE)
trait.data2<-cbind(trait.data,yearbytemp[pmatch(trait.data$growth.year,yearbytemp[,"Year"],duplicates.ok=TRUE),2:176])
View(trait.data2)

##starting the sliding window##

str(trait.data2)
trait.data2$Yearf<-as.factor(trait.data2$growth.year)
trait.data2$yearcent<-trait.data2$growth.year-mean(trait.data2$growth.year)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+(1|Code),data=trait.data2,REML=FALSE)
summary(nullmodel)

store.opt<-c() 
store.slope<-c()
store.AIC<-c()
store.start<-c()
store.end<-c()
store.intercept<-c()
store.loglik<-c()
store.significance<-c()
store.SE<-c()
store.t<-c()
x<-0

for(startval in 97:271){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=271){
      x<-x+1
      
      trait.data2$tempred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) 
      tempmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+tempred+(1|Code),data=trait.data2,REML=FALSE)
      
      store.opt[x]<-tempmodel@optinfo$conv$opt
      store.AIC[x]<-AIC(tempmodel)
      store.slope[x]<-summary(tempmodel)$coefficients[6,1]
      store.start[x]<-startval
      store.end[x]<-endval
      store.intercept[x]<-summary(tempmodel)$coefficients[1,1]
      store.significance[x]<-summary(tempmodel)$coefficients[6,5]
      store.SE[x]<-summary(tempmodel)$coefficients[6,2]
      store.t[x]<-summary(tempmodel)$coefficients[6,4]
      store.loglik[x]<-logLik(tempmodel)
    }
  }
}

summary(tempmodel)

which(store.AIC==min(store.AIC))

store.AIC[11099] 
store.slope[11099]
store.start[11099]
store.end[11099]
store.intercept[11099]
store.significance[11099]
store.SE[11099]
store.t[11099]
AIC(nullmodel)

store.opt[11099]

alt.antler.mass.temp.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(alt.antler.mass.temp.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))
write.table(alt.antler.mass.temp.windows, "alt.antler.mass.temp.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

#####gridded wind window#####

yearbywind<-matrix(nrow=(2019-2012)+1,ncol=175+1)

for (yearval in 2012:2019){ 
  yearbywind[(yearval-2011),2:176]<-weatherdata [which(weatherdata$Year==yearval),"WindGrid"][97:271]
  
  yearbywind[(yearval-2011),1]<-yearval}

colnames(yearbywind)<-as.character(c("Year",seq(97,271,1)))
View(yearbywind)
sum(is.na(yearbywind))
write.table(yearbywind,"ANTLERWINDGRID.csv",sep=",",col.names=TRUE,row.names=TRUE)
trait.data2<-cbind(trait.data,yearbywind[pmatch(trait.data$growth.year,yearbywind[,"Year"],duplicates.ok=TRUE),2:176])
View(trait.data2)

##starting the sliding window##

str(trait.data2)
trait.data2$Yearf<-as.factor(trait.data2$growth.year)
trait.data2$yearcent<-trait.data2$growth.year-mean(trait.data2$growth.year)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+(1|Code),data=trait.data2,REML=FALSE)
summary(nullmodel)

store.opt<-c() 
store.slope<-c()
store.AIC<-c()
store.start<-c()
store.end<-c()
store.intercept<-c()
store.loglik<-c()
store.significance<-c()
store.SE<-c()
store.t<-c()
x<-0

for(startval in 97:271){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=271){
      x<-x+1
      
      trait.data2$windred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) 
      windmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+windred+(1|Code),data=trait.data2,REML=FALSE)
      
      store.opt[x]<-windmodel@optinfo$conv$opt
      store.AIC[x]<-AIC(windmodel)
      store.slope[x]<-summary(windmodel)$coefficients[6,1]
      store.start[x]<-startval
      store.end[x]<-endval
      store.intercept[x]<-summary(windmodel)$coefficients[1,1]
      store.significance[x]<-summary(windmodel)$coefficients[6,5]
      store.SE[x]<-summary(windmodel)$coefficients[6,2]
      store.t[x]<-summary(windmodel)$coefficients[6,4]
      store.loglik[x]<-logLik(windmodel)
    }
  }
}

summary(windmodel)

which(store.AIC==min(store.AIC))

store.AIC[10039] 
store.slope[10039]
store.start[10039]
store.end[10039]
store.intercept[10039]
store.significance[10039]
store.SE[10039]
store.t[10039]
AIC(nullmodel)

store.opt[10039]

alt.antler.mass.wind.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(alt.antler.mass.wind.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))
write.table(alt.antler.mass.wind.windows, "alt.antler.mass.wind.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

#####preparing local data#####

library(readxl)
weatherdata <- read_excel("RUM data/weather/variables/tblWeatherDaily.xlsx")
View(weatherdata)

#getting temp variable

temp<-aggregate(AirTempAvg24~Timestamp, weather, mean)
View(temp)
temp<-separate(temp, "Timestamp", c("Year", "Month", "Day"), sep="-")
view(temp)
write.csv(temp, "Temp Local Weather.csv", row.names=FALSE)
temp<-read.csv(file.choose(), header=TRUE)

#getting wind variable

wind<- aggregate(WindSpeedAvg24 ~ Timestamp, weather, mean)
View(wind)
wind<-separate(wind, "Timestamp", c("Year", "Month", "Day"), sep = "-")
View(wind)
write.csv(wind, "Wind Local Weather.csv", row.names=FALSE)
wind<-read.csv(file.choose(), header=TRUE)

#getting rain variable

rain<- aggregate(Rainfall24 ~ Timestamp, weather, mean)
View(rain)
rain<-separate(rain, "Timestamp", c("Year", "Month", "Day"), sep = "-")
View(rain)
write.csv(rain, "Rainfall Local Weather.csv", row.names=FALSE)
rain<-read.csv(file.choose(), header=TRUE)

#####local rain window#####

yearbyrain<-matrix(nrow=(2019-2012)+1,ncol=175+1)

for (yearval in 2012:2019){ 
  yearbyrain[(yearval-2011),2:176]<-rain [which(rain$Year==yearval),"Rainfall24"][97:271]
  
  yearbyrain[(yearval-2011),1]<-yearval}

colnames(yearbyrain)<-as.character(c("Year",seq(97,271,1)))
View(yearbyrain)
sum(is.na(yearbyrain))
write.table(yearbyrain,"ANTLERRAINLOCAL.csv",sep=",",col.names=TRUE,row.names=TRUE)
trait.data2<-cbind(trait.data,yearbyrain[pmatch(trait.data$growth.year,yearbyrain[,"Year"],duplicates.ok=TRUE),2:176])
View(trait.data2)

##starting the sliding window##

str(trait.data2)
trait.data2$Yearf<-as.factor(trait.data2$growth.year)
trait.data2$yearcent<-trait.data2$growth.year-mean(trait.data2$growth.year)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+(1|Code),data=trait.data2,REML=FALSE)
summary(nullmodel)

store.opt<-c() 
store.slope<-c()
store.AIC<-c()
store.start<-c()
store.end<-c()
store.intercept<-c()
store.loglik<-c()
store.significance<-c()
store.SE<-c()
store.t<-c()
x<-0

for(startval in 97:271){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=271){
      x<-x+1
      
      trait.data2$rainred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) 
      rainmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+rainred+(1|Code),data=trait.data2,REML=FALSE)
      
      store.opt[x]<-tempmodel@optinfo$conv$opt
      store.AIC[x]<-AIC(rainmodel)
      store.slope[x]<-summary(rainmodel)$coefficients[6,1]
      store.start[x]<-startval
      store.end[x]<-endval
      store.intercept[x]<-summary(rainmodel)$coefficients[1,1]
      store.significance[x]<-summary(rainmodel)$coefficients[6,5]
      store.SE[x]<-summary(rainmodel)$coefficients[6,2]
      store.t[x]<-summary(rainmodel)$coefficients[6,4]
      store.loglik[x]<-logLik(rainmodel)
    }
  }
}

summary(rainmodel)

which(store.AIC==min(store.AIC))

store.AIC[8656] 
store.slope[8656]
store.start[8656]
store.end[8656]
store.intercept[8656]
store.significance[8656]
store.SE[8656]
store.t[8656]
AIC(nullmodel)

store.opt[8656]

local.antler.mass.rain.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(local.antler.mass.rain.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))
write.table(local.antler.mass.rain.windows, "local.antler.mass.rain.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

#####local temp window#####

yearbytemp<-matrix(nrow=(2019-2012)+1,ncol=175+1)

for (yearval in 2012:2019){ 
  
  yearbytemp[(yearval-2011),2:176]<-temp [which(temp$Year==yearval),"AirTempAvg24"][97:271]
  
  yearbytemp[(yearval-2011),1]<-yearval}

colnames(yearbytemp)<-as.character(c("Year",seq(97,271,1)))
View(yearbytemp)
sum(is.na(yearbytemp))
write.table(yearbytemp,"ANTLERTEMPLOCAL.csv",sep=",",col.names=TRUE,row.names=TRUE)
trait.data2<-cbind(trait.data,yearbytemp[pmatch(trait.data$growth.year,yearbytemp[,"Year"],duplicates.ok=TRUE),2:176])
View(trait.data2)

##starting the sliding window##

str(trait.data2)
trait.data2$Yearf<-as.factor(trait.data2$growth.year)
trait.data2$yearcent<-trait.data2$growth.year-mean(trait.data2$growth.year)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+(1|Code),data=trait.data2,REML=FALSE)
summary(nullmodel)

store.opt<-c() 
store.slope<-c()
store.AIC<-c()
store.start<-c()
store.end<-c()
store.intercept<-c()
store.loglik<-c()
store.significance<-c()
store.SE<-c()
store.t<-c()
x<-0

for(startval in 97:271){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=271){
      x<-x+1
      
      trait.data2$tempred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) 
      tempmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+tempred+(1|Code),data=trait.data2,REML=FALSE)
      
      store.opt[x]<-tempmodel@optinfo$conv$opt
      store.AIC[x]<-AIC(tempmodel)
      store.slope[x]<-summary(tempmodel)$coefficients[6,1]
      store.start[x]<-startval
      store.end[x]<-endval
      store.intercept[x]<-summary(tempmodel)$coefficients[1,1]
      store.significance[x]<-summary(tempmodel)$coefficients[6,5]
      store.SE[x]<-summary(tempmodel)$coefficients[6,2]
      store.t[x]<-summary(tempmodel)$coefficients[6,4]
      store.loglik[x]<-logLik(tempmodel)
    }
  }
}

summary(tempmodel)

which(store.AIC==min(store.AIC))

store.AIC[10471] 
store.slope[10471]
store.start[10471]
store.end[10471]
store.intercept[10471]
store.significance[10471]
store.SE[10471]
store.t[10471]
AIC(nullmodel)

store.opt[10471]

local.antler.mass.temp.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(local.antler.mass.temp.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))
write.table(local.antler.mass.temp.windows, "local.antler.mass.temp.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

#####local wind window#####

yearbywind<-matrix(nrow=(2019-2012)+1,ncol=175+1)

for (yearval in 2012:2019){ 
  yearbywind[(yearval-2011),2:176]<-wind [which(wind$Year==yearval),"WindSpeedAvg24"][97:271]
  
  yearbywind[(yearval-2011),1]<-yearval}

colnames(yearbywind)<-as.character(c("Year",seq(97,271,1)))
View(yearbywind)
sum(is.na(yearbywind))
write.table(yearbywind,"ANTLERWINDLOCAL.csv",sep=",",col.names=TRUE,row.names=TRUE)
trait.data2<-cbind(trait.data,yearbywind[pmatch(trait.data$growth.year,yearbywind[,"Year"],duplicates.ok=TRUE),2:176])
View(trait.data2)

##starting the sliding window##

str(trait.data2)
trait.data2$Yearf<-as.factor(trait.data2$growth.year)
trait.data2$yearcent<-trait.data2$growth.year-mean(trait.data2$growth.year)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+(1|Code),data=trait.data2,REML=FALSE)
summary(nullmodel)

store.opt<-c() 
store.slope<-c()
store.AIC<-c()
store.start<-c()
store.end<-c()
store.intercept<-c()
store.loglik<-c()
store.significance<-c()
store.SE<-c()
store.t<-c()
x<-0

for(startval in 97:271){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=271){
      x<-x+1
      
      trait.data2$windred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) 
      windmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+dencent+windred+(1|Code),data=trait.data2,REML=FALSE)
      
      store.opt[x]<-windmodel@optinfo$conv$opt
      store.AIC[x]<-AIC(windmodel)
      store.slope[x]<-summary(windmodel)$coefficients[6,1]
      store.start[x]<-startval
      store.end[x]<-endval
      store.intercept[x]<-summary(windmodel)$coefficients[1,1]
      store.significance[x]<-summary(windmodel)$coefficients[6,5]
      store.SE[x]<-summary(windmodel)$coefficients[6,2]
      store.t[x]<-summary(windmodel)$coefficients[6,4]
      store.loglik[x]<-logLik(windmodel)
    }
  }
}

summary(windmodel)

which(store.AIC==min(store.AIC))

store.AIC[11864] 
store.slope[11864]
store.start[11864]
store.end[11864]
store.intercept[11864]
store.significance[11864]
store.SE[11864]
store.t[11864]
AIC(nullmodel)

store.opt[11864]

local.antler.mass.wind.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(local.antler.mass.wind.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))
write.table(local.antler.mass.wind.windows, "local.antler.mass.wind.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

#####plotting windows within 2 AICs of top model#####

##gridded temp - top AIC = 1417.485##
data<-alt.antler.mass.temp.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

temp.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
temp.graph
temp.graph + ggtitle("Critical windows within 2 AIC's of the best model for temperature as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

temp.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=1418.485, linetype="solid", color = "red", size=1)
temp.graph2
temp.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for temperature as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##gridded rain - top AIC = 1418.485##
data<-alt.antler.mass.rain.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

rain.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
rain.graph
rain.graph + ggtitle("Critical windows within 2 AIC's of the best model for rain as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

rain.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=1418.064, linetype="solid", color = "red", size=1)
rain.graph2
rain.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for rain as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##gridded wind - top AIC =##
data<-alt.antler.mass.wind.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

wind.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
wind.graph
wind.graph + ggtitle("Critical windows within 2 AIC's of the best model for wind as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

wind.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=1418.074, linetype="solid", color = "red", size=1)
wind.graph2
wind.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for wind as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##local temp - top AIC = 1417.166##
data<-local.antler.mass.temp.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

temp.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
temp.graph
temp.graph + ggtitle("Critical windows within 2 AIC's of the best model for temperature as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

temp.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=1418.165, linetype="solid", color = "red", size=1)
temp.graph2
temp.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for temperature as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##local rain - top AIC = 1416.852##
data<-local.antler.mass.rain.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

rain.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
rain.graph
rain.graph + ggtitle("Critical windows within 2 AIC's of the best model for rain as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

rain.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=1417.844, linetype="solid", color = "red", size=1)
rain.graph2
rain.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for rain as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##local wind - top AIC = 1416.864##
data<-local.antler.mass.wind.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

wind.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
wind.graph
wind.graph + ggtitle("Critical windows within 2 AIC's of the best model for wind as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

wind.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=1417.864, linetype="solid", color = "red", size=1)
wind.graph2
wind.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for wind as predictor of \nantler mass (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

#####adding windows to trait data frame#####

raingrid<-read.csv(file.choose())
tempgrid<-read.csv(file.choose())
windgrid<-read.csv(file.choose())
rainlocal<-read.csv(file.choose())
templocal<-read.csv(file.choose())
windlocal<-read.csv(file.choose())
View(raingrid)
View(tempgrid)
View(windgrid)
View(rainlocal)
View(templocal)
View(windlocal)

traits<-read.table(file.choose(),header=TRUE)
View(traits)
traits<-traits%>%filter(between(CastYear, 2012,2019))

weather.by.year<-data.frame(CastYear=seq(2012,2019,1))
weather.by.year$temp<-apply(tempgrid[1:8,94:161],1,mean)
weather.by.year$wind<-apply(windgrid[1:8,82:91],1,mean)
weather.by.year$rain<-apply(raingrid[1:8,58:122],1,mean)
traits.weather<-merge(traits,weather.by.year,by="CastYear",duplicates.ok=TRUE)
View(traits.weather)

colnames(traits.weather)[colnames(traits.weather)=='temp']<-'tempgrid'
colnames(traits.weather)[colnames(traits.weather)=='rain']<-'raingrid'
colnames(traits.weather)[colnames(traits.weather)=='wind']<-'windgrid'

weather.by.year<-data.frame(CastYear=seq(2012,2019,1))
weather.by.year$templocal<-apply(templocal[1:8,87:93],1,mean)
weather.by.year$windlocal<-apply(windlocal[1:8,67:148],1,mean)
weather.by.year$rainlocal<-apply(rainlocal[1:8,105:145],1,mean)
traits.weather<-merge(traits.weather,weather.by.year,by="CastYear",duplicates.ok=TRUE)
View(traits.weather)

write.table(traits.weather,"antlermasstraits.csv",sep=",", col.names=TRUE, row.names=TRUE)

#####combining antler mass and NAO#####

#NAO data set contains DJF,DJFM,MAM,JJA for current year and SON for previous year
NAO<- NAO %>% filter(between(year, 2012, 2019))
view(NAO)
mass<-read.csv(file.choose(), header=TRUE) #antlermasstraits.csv
view(mass)
mass<-mass%>%filter(between(CastYear,2012,2019))
colnames(mass)[colnames(traits.weather)=='CastYear']<-'year'
mass<-merge(mass,NAO, by="year")
view(mass)
write.table(mass, "antler.mass.NAO.csv", sep=",", col.names=TRUE, row.names=FALSE)

#####gridded and local window data correlation#####

data<- read.csv(file.choose())
View(data)
data[data==0.000]<-NA
data<-data[complete.cases(data),]

#temp

cor.test(data$templocal, data$tempgrid)
ggscatter(data, x="templocal", y="tempgrid", add="reg.line", conf.int=TRUE, cor.coef=TRUE, cor.methods="pearson", xlab="local average daily temperature (C)", ylab="gridded average daily temperature (C)")

#rain

cor.test(data$rainlocal, data$raingrid)
ggscatter(data, x="rainlocal", y="raingrid", add="reg.line", conf.int=TRUE, cor.coef=TRUE, cor.methods="pearson", xlab="local average daily rainfall (mm)", ylab="gridded average daily rainfall (mm)")

#wind

cor.test(data$windlocal, data$windgrid)
ggscatter(data, x="windlocal", y="windgrid", add="reg.line", conf.int=TRUE, cor.coef=TRUE, cor.methods="pearson", xlab="local average daily windspeed (mph)", ylab="gridded average daily windspeed (mph)")

#####competing measures#####

traits<-read.csv(file.choose(),header=TRUE)
View(traits)

traits$yearcent<-traits$year-mean(traits$year) 
traits$dencent<-traits$pop.density-mean(traits$pop.density)
View(traits)

#comparing seasonal NAO indices

NAOmax<-lmer(antler.mass~yearcent+dencent+Age+I(Age^2)+(1|Code)+DJF+DJFM+MAM+JJA+SON+annual, data=traits, REML=FALSE)

anova(NAOmax)

#after competing significant values in maximal model, DJFM,DJFM and SON were significant

#####competing measures - temp#####

tempmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+(1|Code)+DJF+DJFM+SON+tempgrid+templocal, data=traits, REML=FALSE)
anova(tempmodel)

#removing values from maximal model

tempmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+(1|Code)+DJF+DJFM+SON+templocal, data=traits, REML=FALSE)
anova(tempmodel)

tempmodel<-lmer(antler.mass~Age+I(Age^2)+(1|Code)+DJF+DJFM+SON, data=traits, REML=FALSE)
anova(tempmodel)

#DJF,DJFM, and SON significant together but not separately

#####competing measures - rain#####

rainmodel<-lmer(antler.mass~yearcent+Age+I(Age^2)+(1|Code)+DJF+DJFM+SON+raingrid+rainlocal, data=traits, REML=FALSE)
anova(rainmodel)

#removing values from maximal model

rainmodel<-lmer(antler.mass~Age+I(Age^2)+(1|Code)+DJFM+SON+raingrid, data=traits, REML=FALSE)
anova(rainmodel)

#DJFM,SON, and grid significant predictors of antler mass

#####competing measures - wind#####

windmodel<-lmer(antler.mass~Age+I(Age^2)+(1|Code)+DJF+DJFM+SON+windlocal+windgrid, data=traits, REML=FALSE)
anova(windmodel)

#removing from maximal model

windmodel<-lmer(antler.mass~Age+I(Age^2)+(1|Code)+DJF+DJFM+SON, data=traits, REML=FALSE)
anova(windmodel)

#DJF,DJFM,SON all significant