#### This is the analysis code for analyzing the second Vanad call center data set.
input_dir=c("/Users/mamadouthiongane/Desktop/work")
##input_dir = c("C:/Users/lli680/Downloads/sihan")
output_dir = c("/Users/mamadouthiongane/Desktop/work")
setwd(input_dir)

source("Functions Data analysis.R")
source("Functions for input and output.R")

Windows = TRUE
act_arrival = TRUE ## If TRUE, then the actuals are uniformly distributed in each interval
Empirical_arrival = FALSE ### If TRUE, then the Empirical arrival times are used in simulation
AHT_per_day = TRUE  ## If TRUE, then it means that I use the AHT per day instead of the AHT of the whole year
AHT_fit = FALSE ## If TRUE, then fitted AHT is used, instead of the empirical AHT.
Empirical_HT = TRUE ## If TRUE, then the percentile of the empirical HT is used, instead of the AHT
wrap_up_time_consideration = TRUE ## if TRUE, then wrap up time is added in the service duration
Empirical_patience = TRUE  ## If TRUE, then it means that I use the Empirical patience time
#AHT_per_interval = FALSE
#Patience_per_day = FALSE
##test from here!!



## test end!!
working_activity = c(3, 16)
# c(3, 7, 16) or working_activity = c(3, 16, 39, 40, 41, 42, 61)

#### explanation of the activity number
act_explanation = load_activity_explanation()

#### activity data
act.data = load_activity_data()

#### load call records/log data
call.data = load_call_data()

#### calculate the queue name distribution
x = call.data
x$count = 1
y = tapply(x$count, x$queue_name, sum)
call.distribution = data.frame(queue_name = row.names(y), num = as.numeric(y), per = round(as.numeric(y) / sum(y), 3))
call.distribution = call.distribution[order(-call.distribution$per), ]
queue_level = as.character(call.distribution$queue_name)
call.distribution$queue_name = factor(call.distribution$queue_name, levels = queue_level)
#### More than 99% of the calls come from the biggest 8 routers
# which are 30175, 30560, 30172, 30181, 30179, 30066, 30518, 30241


#### calculate the arrival rate of each 15 minutes interval for a selected queue of (a) selected day(s)
interval_length = 1800 ## this should be either 1800 sec (30min) or 900 sec(15min)
selected_queue_name = c(30175, 30560, 30172, 30181, 30179, 30066, 30518, 30241)
considered_days = seq(as.Date("2014-1-1"), as.Date("2014-12-30"), by = "days")
# remove holidays
holidays = as.Date(c("2014-1-1", "2014-4-21", "2014-5-29", "2014-6-9", "2014-12-25", "2014-12-26"))
holidays.row = which(considered_days %in% holidays)
if (length(holidays.row) > 0) {
  considered_days = considered_days[-holidays.row]
}

# remove Weekends since it is closed
##kk=which(weekdays(considered_days)%in% c("Saturday","Sunday"))
##kk=which(weekdays(considered_days)%in% c("zaterdag" , "zondag" ))
kk=weekdays(considered_days)
considered_days = considered_days[-which(weekdays(considered_days) %in% c("Saturday","Sunday"))]
# remove AHT fluctuating days
bad.days = as.Date(c("2014-01-17", "2014-01-28", "2014-02-07", "2014-02-18", "2014-02-19",
             "2014-02-20", "2014-03-04", "2014-04-18", "2014-05-01", "2014-05-05",
             "2014-05-19", "2014-05-27", "2014-06-05", "2014-08-25", "2014-10-09",
             "2014-11-19", "2014-11-24"))
considered_days = considered_days[-which(considered_days %in% bad.days)]


## calculate the new AHT fitting with calls per day (APRIL WORK)!!!
## obviously this aht.fit.data is for using in the model that each day has a prediction rather than aggregated month
## For giving the fit for 8 skills
aht.fit.data = data.frame(date = considered_days)
## so aht.fit.data has a name with "date",it is a [237,1] framework, to give the fit one by one
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ab.row = which(is.na(data$answered))
if (length(ab.row) > 0) {
  data = data[-ab.row,]
}
## per agent using agent_id
agent_id = unique(data$agent_number)
temp = data.frame(answered = data$answered, transfer = data$transfer, hangup = data$hangup)
##temp is the only matrix we needed with relevant attributes
y = apply(temp, 1, function(x) min(x[2], x[3], na.rm = T))
## using y because y here is the fitting plot's y value,here is the minimal time between transfer or hangup since either means an end
data$service_dur = as.numeric(difftime(as.POSIXct(y), data$answered, units = "secs"))
day_level=as.character(considered_days)
data$date = format(data$date_received, "%Y-%m-%d")
data$date=factor(data$date,levels=day_level)
data$month = as.numeric(format(data$date_received, "%m"))
##dd = tapply(data$service_dur, list(data$agent_number, data$month), mean)

## here the dd is mean the service_duration as an initial matrix used in the fit

dd=tapply(data$service_dur,list(data$agent_number,data$date),mean)
##NA represent this agent on this day have no calls!!

##for(index in 1:dim(data)[1])
##{
##data$date_index[index]=which(as.character(considered_days)%in%format(data$date_received[index], "%Y-%m-%d")  )
##}

## step 2 data with 12 attributes including data_index for fitting x!! (method 1: using the day as x value)

aht.result = data.frame(agent = rownames(dd), a = 0, b = 0)
aht.result = cbind(aht.result, dd)
## aht.result restore the value with dim[agents, a,b,dd]
## this is used to become the initial value(mean value): vector

data$counting=1
##Max_Day=tapply(data$date_index,data$agent_number,max)


for(agent_index in 1:dim(dd)[1])
{
  agent_index=1
  data.agent=data[which(data$agent_number== rownames(dd)[agent_index]),]
  ## this data.agent is the data can be managed based on each agent_index
  Num_of_Call=tapply(data.agent$counting,data.agent$date,sum)
  Num_of_Day=length(unique(data.agent$date))
  x=as.numeric(data.agent$date)
  y=data.agent$service_dur
  x_index_day=seq(1:length(considered_days))
  ##xname=dd[agent_index,] 
  xx=seq(1:length(x))
  mod <- nls(as.numeric(y) ~ a * exp(b * as.numeric(xx)), start = list(a=mean(y,na.rm=T),b=0), control = list(maxiter = 500),trace=TRUE)
  aht.result$a[agent_index] = coef(mod)[1]
  aht.result$b[agent_index] = coef(mod)[2]
  fitted=coef(mod)[1] * exp(coef(mod)[2] *xx)
  plot(fitted)
  pdf(file = paste("ttt", ".pdf", sep = ""), width = 11.69, height = 8.27)
  plot(y~xx)
  dev.off()
  aht.result[agent_index, 4:dim(aht.result)[2]] =fitted
  if (Num_of_Day==0) 
    ##No calls at all
  {
    aht.result$a[agent_index] = aht.result$b[agent_index] = NA
    aht.result[agent_index,4:dim(aht.result)[2]] = NA
  }else if(Num_of_Day<=10)
    ## At least work for 30 days and above considering that they have a pattern of learning!! decreasing time
  {
    aht.result$a[agent_index] = aht.result$b[agent_index] = NA
    fitted = mean(y,na.rm=T)
    aht.result[agent_index,4:dim(aht.result)[2]] = as.numeric(fitted)
    aht.result[agent_index,(3+as.numeric(which(is.na(Num_of_Call))))]=NA
    
  }else{
    mod <- nls(as.numeric(y) ~ a * exp(b * as.numeric(x)), start = list(a=mean(y,na.rm=T),b=0), control = list(maxiter = 500),trace=TRUE)
    aht.result$a[agent_index] = coef(mod)[1]
    aht.result$b[agent_index] = coef(mod)[2]
    fitted=coef(mod)[1] * exp(coef(mod)[2] *x_index_day)
    aht.result[agent_index, 4:dim(aht.result)[2]] =fitted
  }
  
  
}
##pdf(file=paste("current fit","pdf",sep=""))

##dev.off()
##aht.result is for each agent, each day fit value
##aht.fit is for everyday fit for all agent
### Calculate number of answered calls
## this time we consider per day
aht.fit = NULL
for (day_index2 in 1:length(considered_days)) {
  selected_days2 = considered_days[day_index2]
  temp = data[which(format(data$date_received, "%Y-%m-%d") == selected_days2),]
  if (length(temp$date_received) > 0) 
  {
    temp$count = 1
    num = tapply(temp$counting, temp$agent_number, sum)
    na.row = which(is.na(num))
    if (length(na.row) > 0) {
      num = num[-na.row]
    }
    agent = names(num)
    ## today, which agent works restored in agent!!
    ## for now, only work on these agents,anyway, each day we only give a fit result in the end
    agent.aht.data = data.frame(agent = agent, num = as.numeric(num), fit.aht = 0)
    col=3+day_index2
    ##col = which(colnames(aht.result)==considered_days[day_index2])
    xx = unlist(lapply(agent.aht.data$agent, function(x) which(as.character(aht.result$agent) == as.character(x))))
    agent.aht.data$fit.aht = aht.result[xx,col]
    agent.aht.data$num[is.na(agent.aht.data$fit.aht)]=0
    agent.aht.data$fit.aht[is.na(agent.aht.data$fit.aht)]=0
    
    aht.fit = c(aht.fit, sum(agent.aht.data$num * agent.aht.data$fit.aht) / sum(agent.aht.data$num))
  } else {
    aht.fit = c(aht.fit, NA)
  }
  
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (queue_index in 1:length(selected_queue_name)) {
   temp.x = call.data[which(call.data$queue_name == selected_queue_name[queue_index]),]
  temp.x = temp.x[which(format(temp.x$date_received, "%Y-%m-%d") %in% as.character(considered_days)),]
    aht.fit = fitting_AHT_NEW2(temp.x, considered_days) 
    aht.fit.data = cbind(aht.fit.data, aht.fit)
}
write.table(aht.fit.data, file = "10fitresults.csv",sep = ",", col.names = NA)
##Calculate the R_Square!!!~~
R_SQUARE=NULL
RealAHT_overall=NULL
RealAHT_ALLSKILL=NULL
for(queue_index in 1:length(selected_queue_name))
{
  RealAHT_per_day=NULL
  temp.r = call.data[which(call.data$queue_name == selected_queue_name[queue_index]),]
  temp.r = temp.r[which(format(temp.r$date_received, "%Y-%m-%d") %in% as.character(considered_days)),]
  RealAHT_per_day=calculate_aht_per_day(temp.r,considered_days)
  RealAHT_overall=c(RealAHT_overall,mean(RealAHT_per_day,na.rm=TRUE))
  RealAHT_ALLSKILL=cbind(RealAHT_ALLSKILL, RealAHT_per_day)
  R_SQUARE=c(R_SQUARE,1-sum((RealAHT_per_day-aht.fit.data[,queue_index+1])^2,na.rm=TRUE)/sum((RealAHT_per_day-mean(RealAHT_per_day,na.rm=TRUE))^2,na.rm=TRUE))
}
 pdf(file=paste("R_square using new fitting method",".pdf",sep=""))
 plot(R_SQUARE~c(1:8),ylim=c(0,1),cex.lab=1.5,type="p",lwd = 3, cex.axis = 1.5, xlab = "Skill",ylab="R_SQUARE")
 dev.off()

## Here we also need to plot some figures!
for(selected in 1:length(selected_queue_name))
{pdf(file = paste("aht_fit_skill",selected, ".pdf", sep = ""), width = 11.69, height = 8.27)
plot(aht.fit.data[,1+selected], ylab = "fit AHT per day", cex.lab = 1.5, type = "p",pch=2 ,lwd = 3, cex.axis = 1.5, xaxt="n", xlab = "Day ",ylim=c(0,350))
points(RealAHT_ALLSKILL[,selected],col="red",pch=5)
dev.off()
}
write.table(RealAHT_ALLSKILL, file = "Real_aht_data.csv",sep = ",", col.names = NA)
write.table(RealAHT_overall,file="Real_aht_alloveryear.csv",sep=",",col.names=NA)
write.table(R_SQUARE, file = "r_square.csv",sep = ",", col.names = NA)


## The arrival process test
## Arrival process analysis for intra-year, intra-month,intra-day,intra-hour
## Intra_year~~~~~~~~~
##selected_months is a vector contains the "january..."
selected_months=unique(months(considered_days))
Intra_year_ALLSKILL=NULL
for(queue_index in 1:length(selected_queue_name))
  Intra_year_ALLSKILL=cbind(Intra_year_ALLSKILL,arrival_analysis_intra_year(selected_queue_name[queue_index]))

## which skill you want to plot? change the skill
skill=1
time_month_x=c(1:length(selected_months))
plot(Intra_year_ALLSKILL[,skill]~time_month_x,col=1,main="intra-year variation of arrival process",xlab="months",ylab="number of arrivals",xlim=c(1,12),ylim=c(0,max(Intra_year_ALLSKILL[,skill])*5/4))
lines(Intra_year_ALLSKILL[,skill]~time_month_x)

## intra_week~~~~~~~~ if longer is wanted, we can use 41~90, i.e.
Arrivaltest_week=NULL
## choose 41~60 in considered_days as four weeks as 1 month
for(index in 41:60)
{
  interval_values=calculate_info_15_minutes_interval(considered_days[index],selected_queue_name[1],interval_length )
  Arrivaltest_week=cbind(Arrivaltest_week,interval_values[,2])
}
##interval_values[,2] because the second is arrivals
Arrivaltest_week=cbind(Arrivaltest_week,time1)
##intra_week here is the number of arrivals each day as a sum
intra_week=NULL;
for(k in 1:20)
{
  intra_week=c(intra_week,sum(Arrivaltest_week[,k],na.rm=T))
}
timeweek=c(1:20)
plot(intra_week~timeweek,col=1,main="intra-week variation of arrival process",xlab="weekdays",ylab="number of arrivals",xlim=c(1,20),ylim=c(0,3500))
lines(intra_week~timeweek)

##intra-days and intra-hours are displayed following with analysis

##Show the heterogeneity between different mondays/ tuesday/ wednesday/...
interval_length = 900   ## the interval is 15 minutes
acceptable_waiting_time=60
## Dutch monday-friday
selected_Monday=which(weekdays(considered_days)%in% "maandag")
selected_Tuesday=which(weekdays(considered_days)%in% "dinsdag")
selected_Wendsday=which(weekdays(considered_days)%in% "woensdag")
selected_Thursday=which(weekdays(considered_days)%in% "donderdag")
selected_Friday=which(weekdays(considered_days)%in% "vrijdag")

selected_Monday=which(weekdays(considered_days)%in% "Monday")
selected_Tuesday=which(weekdays(considered_days)%in% "Tuesday")
selected_Wendsday=which(weekdays(considered_days)%in% "Wednesday")
selected_Thursday=which(weekdays(considered_days)%in% "Thursday")
selected_Friday=which(weekdays(considered_days)%in% "Friday")

starttime1 = strptime("08:00:00", "%H:%M:%S")
if (interval_length == 900) {
  endtime1 = strptime("19:45:00", "%H:%M:%S")
} else if (interval_length == 1800) {
  endtime1 = strptime("19:30:00", "%H:%M:%S")
}
##time1 = format(seq(starttime1, endtime1, by = interval_length), "%H:%M:%S")
time1=seq(8,19.75,by=0.25)
## time1 is within one day with 15 minutes, time2 is with 1 hour
time2=seq(8,19,by=1)
Arrivaltest=NULL
## change the weekday you want to check?
for(index in 1:length(selected_Monday))
{
  interval_values=calculate_info_15_minutes_interval(considered_days[selected_Monday[index]], selected_queue_name[1], interval_length)
  Arrivaltest=cbind(Arrivaltest,interval_values[,2])
}
Arrivaltest=cbind(Arrivaltest,time1)
Arrivaltest[,length(selected_Monday)+1]=floor(Arrivaltest[,length(selected_Monday)+1])

## output the arrival analysis for arrival test!!!
Arrivaltest_hist=NULL
Arrivaltest_allyear=NULL
for(index in 1:length(considered_days))
{
  interval_values=calculate_info_15_minutes_interval(considered_days[index], selected_queue_name[1], interval_length)
  Arrivaltest_allyear=cbind(Arrivaltest_allyear,interval_values[,2])
}
## 1:48 is 15 minutes consideration
for(index in 1:48)
{
pdf(file = paste("arrival analysis",index,".pdf", sep = ""), width = 11.69, height = 8.27)
hist(Arrivaltest_allyear[index,],100,main="histogram of arrival numbers within 15 minutes",
     lwd =3, ylab = "Frequency of arrival numbers", xlab="Number of arrivals",cex.lab = 1.5, cex.axis = 1.5)
dev.off()
}
## Arrivaltest is all mondays' arrival within 15 minutes
arrivaltransfer=NULL
for(k in 1:length(selected_Monday))
{arrivaltransfer=cbind(arrivaltransfer,tapply(Arrivaltest[,k],Arrivaltest[,length(selected_Monday)+1],sum))}


plot(arrivaltransfer[,1]~time2,main="heterogeneity between different mondays",xlab="hours",ylab="number of arrivals",ylim=c(0,max(arrivaltransfer[,1]*5/4)))
lines(arrivaltransfer[,1]~time2)
for(k in 1:4)
  {
  points(arrivaltransfer[,k]~time2,col=k)
  lines(arrivaltransfer[,k]~time2,col=k)
}

##1st month in Monday in 15 minutes and mean
mondayAvg_transfer=apply(arrivaltransfer[,1:4],1,mean)
monday_average=apply(Arrivaltest[,1:4],1,mean)
plot(Arrivaltest[,1]~time1,col=1,main="intra-day variation of arrival process",xlab="hour",ylab="number of arrivals",xlim=c(8,17),ylim=c(0,max(Arrivaltest[,1])*5/4))
lines(Arrivaltest[,1]~time1)
for(index2 in 2:4)
{
  points(Arrivaltest[,index2]~time1,lty=index2)
  lines(Arrivaltest[,index2]~time1,lty=index2)
}
points(monday_average~time1,col=3)
lines(monday_average~time1,col=3)
## the 1st month!! for the subinterval=1 hour and mean
plot(arrivaltransfer[,1]~time2,col=1,pch=0,main="intra-day variation of arrival process",xlab="hour",ylab="number of arrivals",xlim=c(8,17),ylim=c(0,max(arrivaltransfer[,1])*5/4))
lines(arrivaltransfer[,1]~time2)
for(index2 in 2:4)
{
  points(arrivaltransfer[,index2]~time2,pch=index2)
  lines(arrivaltransfer[,index2]~time2)
}
points(mondayAvg_transfer~time2,col=3,pch="*")
lines(mondayAvg_transfer~time2,col=3)

## the 2nd month!!! for the subinterval=15 minutes
mondayAvg_transfer=apply(arrivaltransfer[,5:8],1,mean)
monday_average=apply(Arrivaltest[,5:8],1,mean)
plot(Arrivaltest[,5]~time1,col=1,main="Skill 30175",xlab="t",ylab="average hourly arrival rate",xlim=c(8,17),ylim=c(0,max(Arrivaltest[,5]*5/4)))
lines(Arrivaltest[,5]~time1)
for(index2 in 6:8)
{
  points(Arrivaltest[,index2]~time1,lty=index2)
  lines(Arrivaltest[,index2]~time1,lty=index2)
}
points(monday_average~time1,col=3)
lines(monday_average~time1,col=3)
## the 2nd month!! for the subinterval=1 hour
plot(arrivaltransfer[,5]~time2,col=1,pch=0,main="Skill 30175",xlab="t",ylab="average hourly arrival rate",xlim=c(8,17),ylim=c(0,max(arrivaltransfer[,5])*5/4))
lines(arrivaltransfer[,5]~time2)
for(index2 in 6:8)
{
  points(arrivaltransfer[,index2]~time2,pch=index2)
  lines(arrivaltransfer[,index2]~time2)
}
points(mondayAvg_transfer~time2,col=4)
lines(mondayAvg_transfer~time2,col=4)

## compare different months of monday, the value of y is the average arrival number of 4 mondays in one month!!!
mondayAvg_transfer=apply(arrivaltransfer[,1:4],1,mean)
monday_average=apply(Arrivaltest[,1:4],1,mean)
plot(mondayAvg_transfer~time2,col=3,pch="*",main="Heterogeneity between different months",xlab="hours",ylab="average hourly arrival rate",xlim=c(8,17),ylim=c(0,500))
lines(mondayAvg_transfer~time2,col=3)
mondayAvg_transfer=apply(arrivaltransfer[,5:8],1,mean)
monday_average=apply(Arrivaltest[,5:8],1,mean)
points(mondayAvg_transfer~time2,col=4)
lines(mondayAvg_transfer~time2,col=4)
mondayAvg_transfer=apply(arrivaltransfer[,9:13],1,mean)
monday_average=apply(Arrivaltest[,9:13],1,mean)
points(mondayAvg_transfer~time2,col=5)
lines(mondayAvg_transfer~time2,col=5)
mondayAvg_transfer=apply(arrivaltransfer[,14:16],1,mean)
monday_average=apply(Arrivaltest[,14:16],1,mean)
points(mondayAvg_transfer~time2,col=6)
lines(mondayAvg_transfer~time2,col=6)
mondayAvg_transfer=apply(arrivaltransfer[,17:18],1,mean)
monday_average=apply(Arrivaltest[,17:18],1,mean)
points(mondayAvg_transfer~time2,col=1)
lines(mondayAvg_transfer~time2,col=1)
mondayAvg_transfer=apply(arrivaltransfer[,19:22],1,mean)
monday_average=apply(Arrivaltest[,19:22],1,mean)
points(mondayAvg_transfer~time2,col=2)
lines(mondayAvg_transfer~time2,col=2)
mondayAvg_transfer=apply(arrivaltransfer[,23:26],1,mean)
monday_average=apply(Arrivaltest[,23:26],1,mean)
points(mondayAvg_transfer~time2,col=7)
lines(mondayAvg_transfer~time2,col=7)
mondayAvg_transfer=apply(arrivaltransfer[,27:29],1,mean)
monday_average=apply(Arrivaltest[,27:29],1,mean)
points(mondayAvg_transfer~time2,col=2,pch=5)
lines(mondayAvg_transfer~time2,col=2,pch=5)
mondayAvg_transfer=apply(arrivaltransfer[,30:34],1,mean)
monday_average=apply(Arrivaltest[,30:34],1,mean)
points(mondayAvg_transfer~time2,col=3,pch=4)
lines(mondayAvg_transfer~time2,col=3,pch=4)
mondayAvg_transfer=apply(arrivaltransfer[,35:38],1,mean)
monday_average=apply(Arrivaltest[,35:38],1,mean)
points(mondayAvg_transfer~time2,col=4,pch=3)
lines(mondayAvg_transfer~time2,col=4,pch=3)
mondayAvg_transfer=apply(arrivaltransfer[,39:41],1,mean)
monday_average=apply(Arrivaltest[,39:41],1,mean)
points(mondayAvg_transfer~time2,col=5,pch=2)
lines(mondayAvg_transfer~time2,col=5,pch=2)
mondayAvg_transfer=apply(arrivaltransfer[,42:46],1,mean)
monday_average=apply(Arrivaltest[,42:46],1,mean)
points(mondayAvg_transfer~time2,col=6,pch=6)
lines(mondayAvg_transfer~time2,col=6,pch=6)

##different workdays in the firs week
Arrivaltest=NULL
ski=1
mon=calculate_info_15_minutes_interval(considered_days[selected_Monday[1]], selected_queue_name[ski], interval_length)
tue=calculate_info_15_minutes_interval(considered_days[selected_Tuesday[1]], selected_queue_name[ski], interval_length)
wed=calculate_info_15_minutes_interval(considered_days[selected_Wendsday[1]], selected_queue_name[ski], interval_length)
thr=calculate_info_15_minutes_interval(considered_days[selected_Thursday[1]], selected_queue_name[ski], interval_length)
fri=calculate_info_15_minutes_interval(considered_days[selected_Friday[1]], selected_queue_name[ski], interval_length)
Arrivaltest=cbind(mon[,2],tue[,2],wed[,2],thr[,2],fri[,2])
Arrivaltest=cbind(Arrivaltest,time1)
Arrivaltest[,6]=floor(Arrivaltest[,6])
arrivaltransfer=NULL
for(k in 1:5)
{arrivaltransfer=cbind(arrivaltransfer,tapply(Arrivaltest[,k],Arrivaltest[,6],sum))}
overallAvg_transfer=apply(arrivaltransfer[,1:5],1,mean)
overall_average=apply(Arrivaltest[,1:5],1,mean)

plot(Arrivaltest[,1]~time1,col=1,main="Skill 30241",xlab="t",ylab="average hourly arrival rate",bty="c",type="s")

for(index2 in 2:5)

{
  points(Arrivaltest[,index2]~time1,lty=index2,bty="c",type="s")
}
points(overall_average~time1,bty="c",type="s",col=2)

plot(arrivaltransfer[,1]~time2,col=1,main="Skill 30241",xlab="t",ylab="average hourly arrival rate",bty="c",type="s")

for(index2 in 2:5)
{
  points(arrivaltransfer[,index2]~time2,lty=index2,bty="c",type="s")
  
}
points(overallAvg_transfer~time2,col=3,bty="c",type="s")


## for NHPP test for arrival test########################

## give the duration between two arrival calls
Qname=selected_queue_name[1]
selected_days=considered_days[selected_Monday[1]]
temp = call.data[which(call.data$queue_name %in% Qname & format(call.data$date_received,
                                                                              "%Y-%m-%d") %in% as.character(selected_days)),]
interval_length=900
##temp here is the dataset for each day!
starttime = strptime("08:00:00", "%H:%M:%S")
if (interval_length ==  900) {
  endtime = strptime("19:45:00", "%H:%M:%S")
} else if (interval_length == 1800) {
  endtime = strptime("19:30:00", "%H:%M:%S")
}
## time here is the real time from 8:00 to 20:00 considering the length
time = format(seq(starttime, endtime, by = interval_length), "%H:%M:%S")
x = expand.grid(time, selected_days)

if (length(temp$date_received) == 0) {
  result = data.frame(time = as.POSIXct(paste(x[,2], x[,1], sep = " ")), arrival= NA, abandon = 0,
                      mean_ser_dur =NA, mean_wait_time = NA, SL = NA)
} else {
  result = data.frame(time = as.POSIXct(paste(x[,2], x[,1], sep = " ")), arrival = 0)
  
  }

## calculate the number of arrivals
  y = paste(format(temp$date_received, "%H"), ((as.numeric(format(temp$date_received, "%M")) %/% (interval_length / 60)) * interval_length / 60), "00", sep = ":")
  interval = paste(format(temp$date_received, "%Y-%m-%d"), format(strptime(y, "%H:%M:%S"), "%H:%M:%S"), sep = " ")
  temp$interval = factor(as.character(as.POSIXct(interval)), levels = as.character(result$time))
  temp$count = 1
  
  result$arrival = tapply(temp$count, temp$interval, sum)

  drindex=1
    if(drindex==length(temp$date_received))
    {
      temp$arrivalinterval[drindex]=0
    }else {
      temp$arrivalinterval[drindex]=as.numeric(difftime(temp$date_received[drindex+1],temp$date_received[drindex], units = "secs"))
}
  for(drindex in 2:length(temp$date_received))
    {
   
  if(drindex==length(temp$date_received))
  {
    temp$arrivalinterval[drindex]=0
  }else {
    temp$arrivalinterval[drindex]=as.numeric(difftime(temp$date_received[drindex+1],temp$date_received[drindex], units = "secs"))
  }
  }
  
  result$arrival = tapply(temp$count, temp$interval, sum)

## in subinterval 15 minutes, give the arrival process dataset to give test
  pvalue=NULL
  accu=0
  meaninterval_sub=NULL
 for(k in 1:48)
{

   testexp=NULL

  for(s in (accu+1):(accu+result$arrival[k]))
  {testexp=c(testexp,temp$arrivalinterval[s])}
  accu=accu+as.numeric(result$arrival[k])
  show(as.numeric(result$arrival[k]))
  meaninterval_sub=c(meaninterval_sub,1/mean(testexp))
  pvalue=cbind(pvalue,ks.test(testexp,"pexp",1/mean(testexp))$p.value)
 }
  
##Test for the maximal distance (small sample of arrival rates test)
  ##for(k in 1:2)
  ##{
  
  accu=0

  D_empirical_real=NULL
  D_empirical_overall_draw=NULL
 for(k in 1:40)
  { 
  D_empirical=NULL
    for(test in 1:1000000)
    {
      ## generate corresponding number of samples in this subinterval
      expsample=rexp(result$arrival[k],meaninterval_sub[k])
      ## calculate the ecdf
      max_distance=NULL
      expsample=sort(expsample)
      for(i in 1:result$arrival[k])
      max_distance=c(max_distance,max(abs((1/result$arrival[k])*(i-1)-(1-exp((-1)*meaninterval_sub[k]*(expsample[i])))),abs((1/result$arrival[k]*i)-(1-exp((-1)*(expsample[i])*meaninterval_sub[k])))))
      D_empirical=c(D_empirical,max(max_distance))
    }
  D_empirical_overall_draw=cbind(D_empirical_overall_draw,D_empirical)

  max_distance_real=NULL
  for(i in (accu+1):(accu+result$arrival[k]))
    max_distance_real=c(max_distance_real,max(abs((1/result$arrival[k])*(i-accu-1)-(1-exp((-1)*meaninterval_sub[k]*(temp$arrivalinterval[i])))),abs((1/result$arrival[k]*(i-accu)-(1-exp((-1)*(temp$arrivalinterval[i])*meaninterval_sub[k]))))))
  
  D_empirical_real=c(D_empirical_real,max(max_distance_real))
  show(max_distance_real)
  accu=accu+result$arrival[k]
 }
  setwd("C:\\Users\\lli680\\Desktop")
   write.table(D_empirical_real,"sample_Dreal.csv",sep=",")

   hist(D_empirical_overall_draw[,1])##D_empirical_real=NULL
   hist(D_empirical_overall_draw[,2])
   qqsample=NULL
   for(a in 1:11)
   qqsample=c(qqsample,temp$arrivalinterval[a])
 
###~~~~ ABOVE is analysis of arrivals
   ## Now I want to see if the fit could be better if we consider number of calls instead of number of days
   ##Current workload and service time has something similar?
     
  



patience_vector = NULL
for (index in 1:length(selected_queue_name)) {
  result = kaplan_meier_estimator(call.data[which(call.data$queue_name == selected_queue_name[index]),])
  patience_vector = c(patience_vector, sum(result$waiting_time * result$aban_prob) / 60)
}

if (Empirical_patience) {
  calculate_output_empirical_patience (call.data, selected_queue_name, 100000, "empirical patience.txt") 
}

### calculate the mean service time (unit: minutes)
## as well as the empirical service time (unit: minutes)
service_time_vector = NULL
for (index in 1:length(selected_queue_name)) {
  temp.data = call.data[which(call.data$queue_name == selected_queue_name[index]),]
  service_time = calculate_mean_ser_dur(temp.data)
  service_time_vector = c(service_time_vector, service_time / 60)
}

aht.fit.data = data.frame(date = considered_days)
if (AHT_fit) {
  for (queue_index in 1:length(selected_queue_name)) {
    temp.x = call.data[which(call.data$queue_name == selected_queue_name[queue_index]),]
    temp.x = temp.x[which(format(temp.x$date_received, "%Y-%m-%d") %in% as.character(considered_days)),]
    aht.fit = fitting_AHT (temp.x, considered_days) 
    aht.fit.data = cbind(aht.fit.data, aht.fit)
  }
}

### output the empirical HT of the whole year
if (Empirical_HT) {
  if (!AHT_per_day) {
    calculate_output_empirical_HT (call.data, selected_queue_name, wrap_up_time,
                                   wrap_up_time_consideration, "empirical HT.txt")
  }
}

### calculate the wrap-up of the whole year
wrap_up_time = as.numeric(calculate_mean_wrap_up_time (act.data) / 60)
if (wrap_up_time_consideration) {
  service_time_vector = service_time_vector + wrap_up_time
}

sim.SL.overall = sim.Ab.overall = sim.ASA.overall = NULL
act.SL.overall = act.Ab.overall = act.ASA.overall = NULL
sim.SL.up.ci = sim.SL.low.ci = sim.Ab.up.ci = NULL
sim.Ab.low.ci = sim.ASA.up.ci = sim.ASA.low.ci = NULL

service_time_df = matrix(0, nrow = length(considered_days), ncol = length(selected_queue_name)+1)
patience_df = service_time_df = data.frame(service_time_df)
colnames(patience_df) = colnames(service_time_df) = c("date", selected_queue_name)
patience_df$date = service_time_df$date = considered_days 
SL_var = ASA_var = Ab_var = 0

### store all the simulation results
Nofsimulation = 1000
sim.sl = sim.ab = sim.asa = data.frame(matrix(0, nrow = length(considered_days), ncol = Nofsimulation+1))
sim.sl[,1] = sim.ab[,1] = sim.asa[,1] = considered_days


for (day_index in 1:length(considered_days)) {
  selected_days = considered_days[day_index]
  acceptable_waiting_time = 60  ### unit is seconds
  
  ### calculate the wrap up time of each day
  if (AHT_per_day) {
    if (wrap_up_time_consideration) {
      temp.data = act.data[which(format(act.data$startdatetime, "%Y-%m-%d") == selected_days),]
      wrap_up_time = as.numeric(calculate_mean_wrap_up_time (temp.data) / 60)
    }
  }
  
  if (Patience_per_day) {
    patience_vector = NULL
    temp.a = call.data[which(format(call.data$date_received, "%Y-%m-%d") == selected_days),]
    for (index in 1:length(selected_queue_name)) { 
      temp.b = temp.a[which(temp.a$queue_name == selected_queue_name[index]),]
      result = kaplan_meier_estimator(temp.b)
      patience_vector = c(patience_vector, sum(result$waiting_time * result$aban_prob) / 60)
    }
    patience_df[day_index, -1] = patience_vector
  }
  
  ### calcualte the AHT and empirical HT of each day
  if (AHT_fit) {
    service_time_vector = as.numeric(aht.fit.data[day_index, -1] /60 )
    na.row = which(is.na(service_time_vector))
    if (length(na.row) > 0) {
      service_time_vector[na.row] = 0
    }
    if (wrap_up_time_consideration) {
      service_time_vector = service_time_vector + wrap_up_time
    }
  } else if (AHT_per_day) {
    service_time_vector = NULL
    temp.a = call.data[which(format(call.data$date_received, "%Y-%m-%d") == selected_days),]
    for (index in 1:length(selected_queue_name)) {
      temp.b = temp.a[which(temp.a$queue_name == selected_queue_name[index]),]
      service_time = calculate_mean_ser_dur(temp.b)
      service_time_vector = c(service_time_vector, service_time / 60)
    }
    if (wrap_up_time_consideration) {
      service_time_vector = service_time_vector + wrap_up_time
    }
    service_time_df[day_index, -1] = service_time_vector
  }
  if (AHT_per_interval & Empirical_HT) {
    data = call.data[which(format(call.data$date_received, "%Y-%m-%d") == selected_days),]
    calculate_output_empirical_HT_per_interval (data, selected_queue_name, wrap_up_time, service_time_vector,
                                                wrap_up_time_consideration, "empirical HT per interval.txt") 
  }
    
  
  ### generate an file, where empirical HTs are stored
  if (Empirical_HT) {
    if (AHT_per_day & !AHT_per_interval) {
      temp.a = call.data[which(format(call.data$date_received, "%Y-%m-%d") == selected_days),]
      calculate_output_empirical_HT (temp.a, selected_queue_name, wrap_up_time,
                                     wrap_up_time_consideration, "empirical HT.txt")
    } 
  } 

  
  ####### calculate the number of arrivals per interval for each skill
  if (Empirical_arrival) {
    temp.a = call.data[which(format(call.data$date_received, "%Y-%m-%d") == selected_days),]
    calculate_output_empirical_arrival_time (temp.a, selected_queue_name, "empirical arrival.txt")
  } else {
    arrival_matrix = NULL
    for (index in 1:length(selected_queue_name)) {
      K = calculate_info_15_minutes_interval (selected_days, selected_queue_name[index], interval_length)
      arrival_matrix = c(arrival_matrix, K$arrival)
    }
    arrival_matrix = matrix(arrival_matrix, ncol = length(selected_queue_name))
    arrival_matrix = data.frame(time = time, arrival_matrix)
  }
  
  #### Decide the skill groups
  ## calculate the skills of those agents who worked in the considered day
  agent.skill.data = calculate_agent_skill_matrix (selected_queue_name, selected_days, call.data) 
  
  ## calculate the skill set of the considered skills of those agents who worked in considered month
  skill.set = calculate_skill_set_matrix (selected_queue_name, agent.skill.data, F) 
  
  ## calculate the number of agents per 30 min interval per skill groups for one specific considered day.
  ### define what activities are considered as "working" activities

  staffing.data = calculate_num_agents_30_interval_skill_group (selected_days, selected_queue_name, skill.set, agent.skill.data, 
                                                                act.data, working_activity, F) 
  manpower_for_other_skills = calcualte_manpower_for_working_other_skills(selected_days, call.distribution, selected_queue_name,
                                                                          skill.set, agent.skill.data, call.data, working_activity)
  staffing.data[,-1] = staffing.data[,-1] + manpower_for_other_skills[,-1]
    
  ### calculate the SL in the selected days
  SL.result = calculate_SL_of_each_skill (selected_days, selected_queue_name, acceptable_waiting_time, interval_length) 
   
  ### generate input fot the simulation (see input explanation for details)
  
  generate_input_file (length(selected_queue_name), dim(skill.set)[2], Nofsimulation, staffing.data,
                       acceptable_waiting_time/60, patience_vector, service_time_vector, arrival_matrix) 
  
  ### run the executable file/simulation and generate output file
  if (Windows) {
    executable.file = "Test"
  } else {
    if (Empirical_HT & Empirical_patience) {
      executable.file = "./sim_empirical_HT_patience"
    } else if (Empirical_HT) {
      executable.file = "./sim_empiricalHT"
    } else if (Empirical_patience) {
      executable.file = "./sim_empirical_patience"
    } else {
      executable.file = "./sim"
    }
  }
  
  if (act_arrival) {
    executable.file = "./sim_act_arrival"
  }
  if (Empirical_arrival) {
    executable.file = "./sim_empirical_arrival"
  }
  if (Empirical_arrival & AHT_per_interval) {
    executable.file = "./sim_empirical_HT_per_interval_patience"
  }
  
  # generate_output_file (executable.file) ## generate output in a txt file
  # run simulation
  #executable.file = "Test alex.exe"
  #sim.result.list = run_simulation_alex (executable.file, staffing.data, selected_queue_name)  
  output.file = c("output.txt")
  sim.result.list = run_simulation_wouter (executable.file, staffing.data, selected_queue_name, output.file)
  sim.SL.overall = c(sim.SL.overall, sim.result.list$SL.overall)
  sim.Ab.overall = c(sim.Ab.overall, sim.result.list$Ab.overall)
  sim.ASA.overall = c(sim.ASA.overall, sim.result.list$ASA.overall * 60) ## sice the time unit is minute, 
  quantile = 0.95
  sim.SL.up.ci = c(sim.SL.up.ci, quantile(sim.result.list$SL.per.simulation, quantile))
  sim.SL.low.ci = c(sim.SL.low.ci, quantile(sim.result.list$SL.per.simulation, 1-quantile))
  sim.Ab.up.ci = c(sim.Ab.up.ci, quantile(sim.result.list$Ab.per.simulation, quantile))
  sim.Ab.low.ci = c(sim.Ab.low.ci, quantile(sim.result.list$Ab.per.simulation, 1-quantile))
  sim.ASA.up.ci = c(sim.ASA.up.ci, quantile(sim.result.list$ASA.per.simulation*60, quantile))
  sim.ASA.low.ci = c(sim.ASA.low.ci, quantile(sim.result.list$ASA.per.simulation*60, 1-quantile))
  #note that this is the ASA of the served customers
  
  ### store all sim results in the data frame
  sim.sl[day_index,2:length(sim.sl[day_index,])] = sim.result.list$SL.per.simulation
  sim.ab[day_index,2:length(sim.ab[day_index,])] = sim.result.list$Ab.per.simulation
  sim.asa[day_index,2:length(sim.asa[day_index,])] = sim.result.list$ASA.per.simulation
    
  ##### compute the SL, Ab, ASA, etc from data
  result = calculate_info_15_minutes_interval(selected_days, selected_queue_name, interval_length)
  #act.SL.per.interval = data.frame(time = result$time, SL = result$SL)
  #act.Ab.per.interval = data.frame(time = result$time, Ab = result$abandon / result$arrival)
  act.SL.overall = c(act.SL.overall, sum(result$SL * (result$arrival-result$abandon) / sum(result$arrival-result$abandon, na.rm=T), na.rm=T))
  act.Ab.overall = c(act.Ab.overall, sum(result$abandon, na.rm = T) / sum(result$arrival, na.rm=T))
  act.ASA.overall = c(act.ASA.overall, sum(result$mean_wait_time * (result$arrival-result$abandon) / sum(result$arrival-result$abandon, na.rm=T), na.rm = T))

  ### calcualte the variability (WAPE) of the SL, ASA, Ab
  SL_var = sum(result$arrival - result$abandon) * sum(abs(sim.result.list$SL.per.simulation - mean(sim.result.list$SL.per.simulation)))
  ASA_var = sum(result$arrival - result$abandon) * sum(abs(sim.result.list$ASA.per.simulation - mean(sim.result.list$ASA.per.simulation)))
  Ab_var = sum(result$arrival) * sum(abs(sim.result.list$Ab.per.simulation - mean(sim.result.list$Ab.per.simulation)))
  
}

###### caculate the performance measures
data = call.data[which(call.data$queue_name %in% selected_queue_name),]
data = data[which(as.Date(format(data$date_received, "%Y-%m-%d")) %in% considered_days),]
wape = calculate_wape (data, sim.SL.overall, act.SL.overall, sim.Ab.overall, act.Ab.overall,
                       sim.ASA.overall, act.ASA.overall)
inclusion = calculate_inclusion (sim.SL.low.ci, sim.SL.up.ci, act.SL.overall, sim.Ab.low.ci, 
                     sim.Ab.up.ci, act.Ab.overall, sim.ASA.low.ci, sim.ASA.up.ci, act.ASA.overall) 

#### calcualte the variability of the SL, ASA, Ab
SL_var = SL_var / (dim(call.data)[1] - length(which(is.na(call.data$answered))))
ASA_var = ASA_var / (dim(call.data)[1] - length(which(is.na(call.data$answered))))
Ab_var = Ab_var / dim(call.data)[1]

### calculate the percentage of actual being above the median of sim
sl.above = length(which(act.SL.overall > apply(sim.sl[,-1], 1, median))) / length(considered_days)
ab.above = length(which(act.Ab.overall > apply(sim.ab[,-1], 1, median))) / length(considered_days)
asa.above = length(which(act.ASA.overall > apply(sim.asa[,-1], 1, median)*60)) / length(considered_days)


