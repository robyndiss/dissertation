#####loading packages needed#####

library(tidyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lme4)
library(lmerTest)
library(MuMIn)

#####gridded temp window#####

GRID<-read.csv(file.choose(), header=TRUE)
View(GRID)
weatherdata<-GRID

yearbytemp<-matrix(nrow=(2019-2012)+1,ncol=365+365+1)

for (yearval in 2012:2019){  
  yearbytemp[(yearval-2011),367:731]<-weatherdata [which(weatherdata$Year==yearval),"TempGrid"][1:365]
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"TempGrid"])==365){
    yearbytemp[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"TempGrid"]}
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"TempGrid"])==366){
    yearbytemp[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"TempGrid"][2:366]}
  
  yearbytemp[(yearval-2011),1]<-yearval
  
}

colnames(yearbytemp)<-as.character(c("Year",seq(-364,365,1)))

View(yearbytemp)

sum(is.na(yearbytemp))

write.table(yearbytemp,"TEMPGRID.csv",sep=",",col.names=TRUE,row.names=TRUE)

trait.data2<-cbind(trait.data,yearbytemp[pmatch(trait.data$Rutyear,yearbytemp[,"Year"],duplicates.ok=TRUE),2:731])
View(trait.data2)

trait.data2<- trait.data2 %>% filter(between(Rutyear,2012,2019))
trait.data2$yearcent<-trait.data2$Rutyear-mean(trait.data2$Rutyear) # this is mean-centring the years to enable a sensible intercept
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+(1|Rutcode),data=trait.data2,REML=FALSE)
summary(nullmodel)

mean(trait.data2$Rutmin) 

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
store.rsquared<-c()
x<-0

for(startval in 1:150){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=267){
      x<-x+1
      
      trait.data2$tempred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) #here I can decide how the weather varibale is is considered eg mean temp, mean windspeed. which is what it is now
      tempmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+tempred+(1|Rutcode),data=trait.data2,REML=FALSE)
      
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
      store.rsquared[x]<-r.squaredGLMM(tempmodel)[2]
      
      
      
    }
  }
}

summary(tempmodel)

which(store.AIC==min(store.AIC))

store.AIC[20109]
store.slope[20109]
store.start[20109]
store.end[20109]
store.intercept[20109]
store.significance[20109]
store.SE[20109]
store.t[20109]
AIC(nullmodel)

store.opt[20109]

rut.start.temp.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(rut.start.temp.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))

write.table(rut.start.temp.windows, "rut.start.gridded.temp.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

sliding<-rut.start.temp.windows

min(sliding[,1])
best.AIC<-min(sliding[,1])

best.AIC

sliding<-as.data.frame(sliding)
sliding$possible<-(sliding$top.AIC-best.AIC)
View(sliding)

select.sliding<- subset(sliding, possible <=2, select=top.AIC:possible)

View(select.sliding)

head(select.sliding)
select.sliding[which(select.sliding$top.AIC == min(select.sliding$top.AIC)), ]

test<-select.sliding[order(select.sliding$top.AIC),]
View(test)

store.opt[20109]

#####gridded rain window#####

yearbyrain<-matrix(nrow=(2019-2012)+1,ncol=365+365+1)

for (yearval in 2012:2019){ 
  yearbyrain[(yearval-2011),367:731]<-weatherdata [which(weatherdata$Year==yearval),"PrecipGrid"][1:365]
  
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"PrecipGrid"])==365){
    yearbyrain[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"PrecipGrid"]}
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"PrecipGrid"])==366){
    yearbyrain[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"PrecipGrid"][2:366]}
  
  yearbyrain[(yearval-2011),1]<-yearval
  
}

colnames(yearbyrain)<-as.character(c("Year",seq(-364,365,1)))

View(yearbyrain)

sum(is.na(yearbyrain))

write.table(yearbyrain,"PRECIPGRID.csv",sep=",",col.names=TRUE,row.names=TRUE)

trait.data2<-cbind(trait.data,yearbyrain[pmatch(trait.data$Rutyear,yearbyrain[,"Year"],duplicates.ok=TRUE),2:731])

View(trait.data2)

trait.data2$yearcent<-trait.data2$Rutyear-mean(trait.data2$Rutyear)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

trait.data2<- trait.data2 %>%filter(between(Rutyear,2012,2019))
nullmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+(1|Rutcode),data=trait.data2,REML=FALSE)
summary(nullmodel)

mean(trait.data2$Rutmin)

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
store.rsquared<-c()
x<-0


for(startval in 1:150){
  
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=267){
      
      x<-x+1
      
      trait.data2$rainpred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) #here I can decide how the weather varibale is is considered eg mean temp, mean windspeed. which is what it is now
      rainmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+rainpred+(1|Rutcode),data=trait.data2,REML=FALSE)
      
      store.opt[x]<-rainmodel@optinfo$conv$opt
      store.AIC[x]<-AIC(rainmodel)
      store.slope[x]<-summary(rainmodel)$coefficients[6,1]
      store.start[x]<-startval
      store.end[x]<-endval
      store.intercept[x]<-summary(rainmodel)$coefficients[1,1]
      store.significance[x]<-summary(rainmodel)$coefficients[6,5]
      store.SE[x]<-summary(rainmodel)$coefficients[6,2]
      store.t[x]<-summary(rainmodel)$coefficients[6,4]
      store.loglik[x]<-logLik(rainmodel)
      store.rsquared[x]<-r.squaredGLMM(rainmodel)[2]
      
    }
  }
}

summary(rainmodel)

which(store.AIC==min(store.AIC))

store.AIC[5042]
store.slope[5042]
store.start[5042]
store.end[5042]
store.intercept[5042]
store.significance[5042]
store.SE[5042]
store.t[5042]
AIC(nullmodel)

store.opt[5042]

rut.start.rain.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(rut.start.rain.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))
write.table(rut.start.rain.windows, "rut.start.gridded.rain.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

sliding<-rut.start.rain.windows
min(sliding[,1])
best.AIC<-min(sliding[,1])
sliding<-as.data.frame(sliding)
sliding$possible<-(sliding$top.AIC-best.AIC)
View(sliding)

select.sliding<- subset(sliding, possible <=2,select=top.AIC:possible)
View(select.sliding)

head(select.sliding)
select.sliding[which(select.sliding$top.AIC == min(select.sliding$top.AIC)), ]

test<-select.sliding[order(select.sliding$top.AIC),]
View(test)

store.opt[5042]

#####gridded wind window#####

yearbywind<-matrix(nrow=(2019-2012)+1,ncol=365+365+1)

for (yearval in 2012:2019){ 
  yearbywind[(yearval-2011),367:731]<-weatherdata [which(weatherdata$Year==yearval),"WindGrid"][1:365]
  
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"WindGrid"])==365){
    yearbywind[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"WindGrid"]}
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"WindGrid"])==366){
    yearbywind[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"WindGrid"][2:366]}
  
  yearbywind[(yearval-2011),1]<-yearval
  
}

colnames(yearbywind)<-as.character(c("Year",seq(-364,365,1)))

View(yearbywind)

sum(is.na(yearbywind))

write.table(yearbywind,"WINDGRID.csv",sep=",",col.names=TRUE,row.names=TRUE)

trait.data2<-cbind(trait.data,yearbywind[pmatch(trait.data$Rutyear,yearbywind[,"Year"],duplicates.ok=TRUE),2:731])
View(trait.data2)

trait.data2<-trait.data2%>%filter(between(Rutyear, 2012,2019))

trait.data2$yearcent<-trait.data2$Rutyear-mean(trait.data2$Rutyear)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+(1|Rutcode),data=trait.data2,REML=FALSE)
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
store.rsquared<-c()
x<-0


for(startval in 1:150){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=267){
      
      x<-x+1
      
      trait.data2$windpred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) #here I can decide how the weather varibale is is considered eg mean temp, mean windspeed. which is what it is now
      windmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+windpred+(1|Rutcode),data=trait.data2,REML=FALSE)
      
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
      store.rsquared[x]<-r.squaredGLMM(windmodel)[2]
      
      
      
    }
  }
}

summary(windmodel)

which(store.AIC==min(store.AIC))

store.AIC[8600]
store.slope[8600]
store.start[8600]
store.end[8600]
store.intercept[8600]
store.significance[8600]
store.SE[8600]
store.t[8600]
AIC(nullmodel)

store.opt[8600]

rut.start.wind.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel))
colnames(rut.start.wind.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model"))
write.table(rut.start.wind.windows, "rut.start.gridded.wind.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

sliding<-rut.start.wind.windows

min(sliding[,1])

best.AIC<-min(sliding[,1])
best.AIC
sliding$possible<-(sliding$top.AIC-best.AIC)

View(sliding)

select.sliding<- subset(sliding, possible <=2,
                        select=top.AIC:possible)

View(select.sliding)

head(select.sliding)
select.sliding[which(select.sliding$top.AIC == min(select.sliding$top.AIC)), ]

test<-select.sliding[order(select.sliding$top.AIC),]
View(test)

#####preparing local data#####

weather<-read.csv(file.choose())
View(weather)
str(weather)

#getting temp variable

temp<-aggregate(AirTempAvg24~Timestamp, weather, mean)
View(temp)
temp<-separate(temp, "Timestamp", c("Day", "Month", "Year"), sep="-")
view(temp)
write.csv(temp, "Temp Local Weather.csv", row.names=FALSE)
temp<-read.csv(file.choose(), header=TRUE)

#getting wind variable

wind<- aggregate(WindSpeedAvg24 ~ Timestamp, weather, mean)
View(wind)
wind<-separate(wind, "Timestamp", c("Day", "Month", "Year"), sep = "-")
View(wind)
write.csv(wind, "Wind Local Weather.csv", row.names=FALSE)
wind<-read.csv(file.choose(), header=TRUE)

#getting rain variable

rain<- aggregate(Rainfall24 ~ Timestamp, weather, mean)
View(rain)
rain<-separate(rain, "Timestamp", c("Day", "Month", "Year"), sep = "-")
View(rain)
write.csv(rain, "Rainfall Local Weather.csv", row.names=FALSE)
rain<-read.csv(file.choose(), header=TRUE)

trait.data<-read.table(file.choose(), header=TRUE)
weatherdata<-temp
view(weatherdata)

yearbytemp<-matrix(nrow=(2019-2012)+1,ncol=365+365+1)

for (yearval in 2012:2019){ 
  
  yearbytemp[(yearval-2011),367:731]<-weatherdata [which(weatherdata$Year==yearval),"AirTempAvg24"][1:365]
  
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"AirTempAvg24"])==365){
    yearbytemp[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"AirTempAvg24"]}
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"AirTempAvg24"])==366){
    yearbytemp[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"AirTempAvg24"][2:366]}
  
  yearbytemp[(yearval-2011),1]<-yearval
  
}

colnames(yearbytemp)<-as.character(c("Year",seq(-364,365,1)))

write.table(yearbytemp,"AirTempAvg24.csv",sep=",",col.names=TRUE,row.names=TRUE)

View(yearbytemp)
sum(is.na(yearbytemp)) 

view(trait.data)
trait.data<- trait.data %>% filter(between(Rutyear,2011, 2019))

trait.data2<-cbind(trait.data,yearbytemp[pmatch(trait.data$Rutyear,yearbytemp[,"Year"],duplicates.ok=TRUE),2:731])
View(trait.data2)

trait.data2<- trait.data2 %>% filter(between(Rutyear,2012,2019))

trait.data2$yearcent<-trait.data2$Rutyear-mean(trait.data2$Rutyear) 
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

#####local temp window#####

nullmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+(1|Rutcode),data=trait.data2,REML=FALSE)

summary(nullmodel)

install.packages("lmerTest")
install.packages("MuMIn")
library(MuMIn)
library(lmerTest)

mean(trait.data2$Rutmin) #267

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
store.rsquared<-c()
x<-0


for(startval in 1:150){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=267){
      
      x<-x+1
      
      trait.data2$tempred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) #here I can decide how the weather varibale is is considered eg mean temp, mean windspeed. which is what it is now
      tempmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+tempred+(1|Rutcode),data=trait.data2,REML=FALSE)
      
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
      store.rsquared[x]<-r.squaredGLMM(tempmodel)[2]
      
    }
  }
}

summary(tempmodel)

which(store.AIC==min(store.AIC))

store.AIC[4611]
store.slope[4611]
store.start[4611]
store.end[4611]
store.intercept[4611]
store.significance[4611]
store.SE[4611]
store.t[4611]
AIC(nullmodel)
store.years[4611]
store.N[4611]
store.rsquared[4611]
store.opt[4611]

rut.start.temp.local.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel),store.rsquared)
colnames(rut.start.temp.local.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model","R squared"))
write.table(rut.start.temp.local.windows, "rut.start.temp.local.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

sliding<-read.csv("rut.start.temp.local.windows.csv")

min(sliding[,1])
best.AIC<-min(sliding[,1]) 
best.AIC

sliding$possible<-(sliding$top.AIC-best.AIC)

View(sliding)

select.sliding<- subset(sliding, possible <=2,select=top.AIC:possible)
View(select.sliding)
head(select.sliding)
select.sliding[which(select.sliding$top.AIC == min(select.sliding$top.AIC)), ]
test<-select.sliding[order(select.sliding$top.AIC),]
View(test)

#####local wind window#####

weatherdata<-wind
View(weatherdata)

yearbywind<-matrix(nrow=(2019-2012)+1,ncol=365+365+1)

for (yearval in 2012:2019){ 
  yearbywind[(yearval-2011),367:731]<-weatherdata [which(weatherdata$Year==yearval),"WindSpeedAvg24"][1:365]
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"WindSpeedAvg24"])==365){
    yearbywind[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"WindSpeedAvg24"]}
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"WindSpeedAvg24"])==366){
    yearbywind[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"WindSpeedAvg24"][2:366]}
  
  yearbywind[(yearval-2011),1]<-yearval
  
}

colnames(yearbywind)<-as.character(c("Year",seq(-364,365,1)))

View(yearbywind)

sum(is.na(yearbywind))

write.table(yearbywind,"WindSpeedAvg24.csv",sep=",",col.names=TRUE,row.names=TRUE)

trait.data2<-cbind(trait.data,yearbywind[pmatch(trait.data$Rutyear,yearbywind[,"Year"],duplicates.ok=TRUE),2:731])

View(trait.data2)

trait.data2<- trait.data2 %>% filter(between(Rutyear,2012,2019))

trait.data2$yearcent<-trait.data2$Rutyear-mean(trait.data2$Rutyear)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+(1|Rutcode),data=trait.data2,REML=FALSE)

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
store.rsquared<-c()
x<-0

for(startval in 1:150){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=267){
      
      x<-x+1
      
      trait.data2$windpred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) #here I can decide how the weather varibale is is considered eg mean temp, mean windspeed. which is what it is now
      windmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+windpred+(1|Rutcode),data=trait.data2,REML=FALSE)
      
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
      store.rsquared[x]<-r.squaredGLMM(windmodel)[2]
      
    }
  }
}

summary(windmodel)

which(store.AIC==min(store.AIC))

store.AIC[21031] 
store.slope[21031]
store.start[21031]
store.end[21031]
store.intercept[21031]
store.significance[21031]
store.SE[21031]
store.t[21031]
AIC(nullmodel)
store.years[21031] #error message; because year wasnt included as random factor
store.N[21031]
store.rsquared[21031]

store.opt[21031]

rut.start.wind.local.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel),store.rsquared)
colnames(rut.start.wind.local.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model","R squared"))
write.table(rut.start.wind.local.windows, "rut.start.wind.local.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

sliding<-read.csv("rut.start.wind.local.windows.csv")

min(sliding[,1])

best.AIC<-min(sliding[,1]) 
best.AIC

sliding$possible<-(sliding$top.AIC-best.AIC)

View(sliding)

select.sliding<- subset(sliding, possible <=2,
                        select=top.AIC:possible)

View(select.sliding)

head(select.sliding)
select.sliding[which(select.sliding$top.AIC == min(select.sliding$top.AIC)), ]

test<-select.sliding[order(select.sliding$top.AIC),]
View(test)

#####local rain window#####

weatherdata<-rain
View(weatherdata)

yearbyrain<-matrix(nrow=(2019-2012)+1,ncol=365+365+1)

for (yearval in 2012:2019){ 
  yearbyrain[(yearval-2011),367:731]<-weatherdata [which(weatherdata$Year==yearval),"Rainfall24"][1:365]
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"Rainfall24"])==365){
    yearbyrain[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"Rainfall24"]}
  
  if(length(weatherdata [which(weatherdata$Year==yearval-1),"Rainfall24"])==366){
    yearbyrain[(yearval-2011),2:366]<-weatherdata [which(weatherdata$Year==yearval-1),"Rainfall24"][2:366]}
  
  yearbyrain[(yearval-2011),1]<-yearval
  
}

colnames(yearbyrain)<-as.character(c("Year",seq(-364,365,1)))

View(yearbyrain)

sum(is.na(yearbyrain))

write.table(yearbyrain,"Rainfall24.csv",sep=",",col.names=TRUE,row.names=TRUE)

trait.data2<-cbind(trait.data,yearbyrain[pmatch(trait.data$Rutyear,yearbyrain[,"Year"],duplicates.ok=TRUE),2:731])

View(trait.data2)

trait.data2<- trait.data2 %>% filter(between(Rutyear,2012,2019))

trait.data2$yearcent<-trait.data2$Rutyear-mean(trait.data2$Rutyear)
trait.data2$dencent<-trait.data2$pop.density-mean(trait.data2$pop.density)

nullmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+(1|Rutcode),data=trait.data2,REML=FALSE)

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
store.rsquared<-c()
x<-0

for(startval in 1:150){
  
  for(duration in 7:150){
    
    endval<-(startval+duration)-1
    
    if(endval<=267){
      
      x<-x+1
      
      trait.data2$rainpred<-rowMeans(trait.data2[,which(names(trait.data2)==paste(startval)):which(names(trait.data2)==paste(endval))]) #here I can decide how the weather varibale is is considered eg mean temp, mean windspeed. which is what it is now
      rainmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+dencent+rainpred+(1|Rutcode),data=trait.data2,REML=FALSE)
      
      store.opt[x]<-rainmodel@optinfo$conv$opt
      store.AIC[x]<-AIC(rainmodel)
      store.slope[x]<-summary(rainmodel)$coefficients[6,1]
      store.start[x]<-startval
      store.end[x]<-endval
      store.intercept[x]<-summary(rainmodel)$coefficients[1,1]
      store.significance[x]<-summary(rainmodel)$coefficients[6,5]
      store.SE[x]<-summary(rainmodel)$coefficients[6,2]
      store.t[x]<-summary(rainmodel)$coefficients[6,4]
      store.loglik[x]<-logLik(rainmodel)
      store.rsquared[x]<-r.squaredGLMM(rainmodel)[2]
      
    }
  }
}

summary(rainmodel)

which(store.AIC==min(store.AIC))

store.AIC[9666]
store.slope[9666]
store.start[9666]
store.end[9666]
store.intercept[9666]
store.significance[9666]
store.SE[9666]
store.t[9666]
AIC(nullmodel)
store.years[9666]
store.N[9666]
store.rsquared[9666]

store.opt[9666]

rut.start.rain.local.windows<-cbind(store.AIC,store.slope,store.start,store.end,store.intercept,store.significance,store.SE,store.t,AIC(nullmodel),store.rsquared)
colnames(rut.start.rain.local.windows)<-as.character(c("top.AIC","slope","start","end","intercept","significance","SE","t value","null model","R squared"))
write.table(rut.start.rain.local.windows, "rut.start.rain.local.windows.csv",sep=",",col.names=TRUE,row.names=FALSE)

sliding<-read.csv("rut.start.rain.local.windows.csv")

min(sliding[,1])

best.AIC<-min(sliding[,1]) 
best.AIC

sliding$possible<-(sliding$top.AIC-best.AIC)

View(sliding)

select.sliding<- subset(sliding, possible <=2,select=top.AIC:possible)

View(select.sliding)

head(select.sliding)
select.sliding[which(select.sliding$top.AIC == min(select.sliding$top.AIC)), ]

test<-select.sliding[order(select.sliding$top.AIC),]
View(test)

#####plotting windows within 2 AICs of top model#####

##local temp - top AIC = 3459.372##
data<-rut.start.temp.local.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

temp.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
temp.graph
temp.graph + ggtitle("Critical windows within 2 AIC's of the best model for temperature as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

temp.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=3460.371, linetype="solid", color = "red", size=1)
temp.graph2
temp.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for temperature as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##local rain - top AIC = 3457.901##
data<-rut.start.rain.local.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

rain.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
rain.graph
rain.graph + ggtitle("Critical windows within 2 AIC's of the best model for rain as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

rain.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=3458.901, linetype="solid", color = "red", size=1)
rain.graph2
rain.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for rain as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##local wind - top AIC = 3458.194##
data<-rut.start.wind.local.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

wind.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
wind.graph
wind.graph + ggtitle("Critical windows within 2 AIC's of the best model for wind as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

wind.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=3459.194, linetype="solid", color = "red", size=1)
wind.graph2
wind.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for wind as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##gridded temp - top AIC = 3459.249##
data<-rut.start.temp.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

temp.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
temp.graph
temp.graph + ggtitle("Critical windows within 2 AIC's of the best model for temperature as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

temp.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=3460.248, linetype="solid", color = "red", size=1)
temp.graph2
temp.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for temperature as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##gridded rain - top AIC = 3457.950##
data<-rut.start.rain.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

rain.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
rain.graph
rain.graph + ggtitle("Critical windows within 2 AIC's of the best model for rain as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

rain.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=3458.949, linetype="solid", color = "red", size=1)
rain.graph2
rain.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for rain as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##gridded wind - top AIC = 3457.832##
data<-rut.start.wind.windows
data<-as.data.frame(data)
best.AIC<-min(data[,1])
data$possible<-(data$top.AIC-best.AIC)
data1<- subset(data, possible <=2, select=top.AIC:possible)
View(data1)

wind.graph<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))
wind.graph
wind.graph + ggtitle("Critical windows within 2 AIC's of the best model for wind as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

data2<- subset(data, possible <=1, select=top.AIC:possible)
max(data2$top.AIC)

wind.graph2<- ggplot(data = data1, aes(x = start, xend = end, y = top.AIC, yend= top.AIC, col = top.AIC))+geom_segment(size=0.1)+scale_y_reverse()+guides(color = guide_colorbar(reverse = TRUE))+geom_hline(yintercept=3458.83, linetype="solid", color = "red", size=1)
wind.graph2
wind.graph2 + ggtitle("Critical windows within 2 AIC's of the best model for wind as predictor of Rut \nstart date (years 2012-2019)") +xlab("\nOrdinal days") + ylab("AIC score\n") + labs(fill = "AIC") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

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
traits<-traits%>%filter(between(Rutyear, 2012,2019))

weather.by.year<-data.frame(Rutyear=seq(2012,2019,1))
weather.by.year$tempgrid<-apply(tempgrid[1:8,508:594],1,mean)
weather.by.year$windgrid<-apply(windgrid[1:8,426:535],1,mean)
weather.by.year$raingrid<-apply(raingrid[1:8,402:409],1,mean)
traits.weather<-merge(traits,weather.by.year,by="Rutyear",duplicates.ok=TRUE)
View(traits.weather)

weather.by.year<-data.frame(Rutyear=seq(2012,2019,1))
weather.by.year$templocal<-apply(templocal[1:8,399:407],1,mean)
weather.by.year$templocal2<-apply(templocal[1:8,512:593],1,mean)
weather.by.year$windlocal<-apply(windlocal[1:8,516:592],1,mean)
weather.by.year$rainlocal<-apply(rainlocal[1:8,434:457],1,mean)
traits.weather<-merge(traits.weather,weather.by.year,by="Rutyear",duplicates.ok=TRUE)
View(traits.weather)

write.table(traits.weather,"rutstarttraits.csv",sep=",", col.names=TRUE, row.names=TRUE)

#####combining rut start and NAO#####

#NAO data set contains DJF,DJFM,MAM,JJA for current year and SON for previous year
NAO<- NAO %>% filter(between(year, 2012, 2019))
view(NAO)
rut<-read.csv(file.choose(), header=TRUE) #rutstarttraits.csv
colnames(rut)[colnames(rut)=="Rutyear"]<-"year"
view(rut)
rut<-rut%>%filter(between(year,2012,2019))
rut<-merge(rut,NAO, by="year")
view(rut)
write.table(rut, "rut.start.short.NAO.csv", sep=",", col.names=TRUE, row.names=FALSE)

#####preparing measures#####

traits<-read.csv(file.choose(),header=TRUE)
View(traits)

traits$yearcent<-traits$year-mean(traits$year) 
traits$dencent<-traits$pop.density-mean(traits$pop.density)
View(traits)

#comparing seasonal NAO indices 2012-2019

NAOmax<-lmer(Rutmin~yearcent+dencent+Age+I(Age^2)+(1|Rutcode)+DJF+DJFM+MAM+JJA+SON+annual, data=traits, REML=FALSE)

anova(NAOmax)


#####competing measures - temp#####

tempmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+annual+tempgrid+templocal+templocal2, data=traits, REML=FALSE)
anova(tempmodel)

#removing values from maximal model

tempmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+tempgrid+templocal+templocal2, data=traits, REML=FALSE)
anova(tempmodel)

tempmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+tempgrid+templocal2, data=traits, REML=FALSE)
anova(tempmodel)

tempmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+tempgrid, data=traits, REML=FALSE)
anova(tempmodel)

#tempgrid was the best

#####competing measures - rain#####

rainmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+annual+raingrid+rainlocal, data=traits, REML=FALSE)
anova(rainmodel)

#removing values from maximal model

rainmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+raingrid+rainlocal, data=traits, REML=FALSE)
anova(rainmodel)

rainmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+rainlocal, data=traits, REML=FALSE)
anova(rainmodel)

#rainlocal was best

#####competing measures - wind#####

windmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+windlocal+windlocal2+windgrid+annual, data=traits, REML=FALSE)
anova(windmodel)

#removing from maximal model

windmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+windlocal+windlocal2+windgrid, data=traits, REML=FALSE)
anova(windmodel)

windmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+windlocal2+windgrid, data=traits, REML=FALSE)
anova(windmodel)

windmodel<-lmer(Rutmin~yearcent+Age+I(Age^2)+(1|Rutcode)+windgrid, data=traits, REML=FALSE)
anova(windmodel)
