rm(list=ls())
set.seed(1234567890)
library("geosphere")
#setwd("~/R/lab3")
stations=read.csv("stations.csv", dec=".", sep=",", fileEncoding = 'latin1')
temps=read.csv("temps50k.csv", dec=".", sep=",", fileEncoding = 'latin1')
st=merge(stations,temps,by="station_number")

#geometry distance
loc_cal=function(p1, p2){
  res= distHaversine(p1,p2, r=6378137)
  return(res)
}

#day difference
date_cal=function(d1,d2){
  day_diff=as.Date(as.character(d1), format="%Y-%m-%d")-as.Date(as.character(d2), format="%Y-%m-%d")
  day_diff=as.numeric(day_diff) %% (365*4+1)
  day_diff=as.numeric(day_diff) %% 365
  if(day_diff > 182.5){
    day_diff=(365/2)-(day_diff %% (365/2))
  }
  return(day_diff)
}

#time difference, only calculates hour difference
time_cal=function(t1,t2){
  t1_hour=as.numeric(as.difftime(t1, format="%H"))
  t2_hour=as.numeric(as.difftime(t2, format="%H"))
  time_diff=abs(t1_hour-t2_hour)
  if(time_diff > 12){
    time_diff = 12 - (time_diff %% 12)
  }
  return(time_diff)
}

#Gaussian kernel
gaus=function(distance, h){
  res=exp(-distance^2/h)
  return(res)
}

a = 58.4030713
b = 15.5970199
p1 = c(a,b)
date = "2008-07-14"
times = c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")

# 3 Gaussian kernels k(u)=exp(-||u||^2)  
h_distance=175e9
h_date=17500
h_time=5

temp <- vector(length=length(times))
# Students??? code here

kernel_function=function(date, p1, time){
  data=st[as.Date(st$date) < date,]
  date_dist=c(1:length(data[,1]))
  loc_dist=c(1:length(data[,1]))
  time_dist=c(1:length(data[,1]))
  res=c(1:length(data[,1]))
  
  #calculate distances from predict
  for (i in 1:length(data[,1])){
    date_dist[i]=date_cal(data$date[i], date)
    date_dist[i]=gaus(date_dist[i], h_date)
    
    loc_dist[i]=loc_cal(p1, c(data$latitude[i], data$longitude[i]))
    loc_dist[i]=gaus(loc_dist[i], h_distance) 
    
    time_dist[i]=time_cal(as.character(data$time[i]), time)
    time_dist[i]=gaus(time_dist[i], h_time)
    
    res[i]=date_dist[i]+loc_dist[i]+time_dist[i]
    #res[i]=date_dist[i]*loc_dist[i]*time_dist[i]
  }
  wam=sum(res*data$air_temperature)/sum(res)
  return(wam)
}

#call the kernel function with the different times
pred=c(1:length(times))
for(i in 1:length(times)){
  pred[i]=kernel_function(date,p1,times[i])
}

plot(pred, xlab="", ylab="temperature", ylim=c(2,18), xaxt="n")
axis(side=1, at=2, lab=times[2])
axis(side=1, at=5, lab=times[5])
axis(side=1, at=8, lab=times[8])
axis(side=1, at=11, lab=times[11])
#compiletime: 10min