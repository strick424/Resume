library(queueing)
library(MASS)
library(compare)
library(ggplot2)
library(ggrepel)

rm(list=ls())
setwd("/Users/colemanstrickland/Documents/Coleman/GradSchool/Stochs2/Final_Project")

fire<-read.csv("Original.csv", as.is=T)
#str(fire)
#2011: F:1100001   L:1122044
#2012: F:1200001   L:1222041
#2013: F:1300012   L:1322183
#2014: F:1400015   L:1423231
#2015: F:1500005   L:1523757
#2016: F:1523762   L:1624587
first_incident<-1100001
last_incident<-1624587
equipment1<-"A4"
equipment2<-"T4"
  
fire<-subset(fire, (fire$Incident.>=first_incident & fire$Incident.<=last_incident) & (fire$Apparatus==equipment1 | fire$Apparatus==equipment2))
#head(fire)
fire$Alarm_Time
Alarm_Time_New<-c()
Arrival_Time_New<-c()
Dispatch_Time_New<-c()
Next_Dispatch_Time_New<-c()
Clear_Time_New<-c()
Incident<-c()
Incident_Type<-c()
Equipment<-c()
District<-c()
for (i in 1:length(fire$Incident.)){
  test<-toString(fire$Alarm_Time[i])
  z<-strptime(test, "%m/%d/%y  %H:%M:%S")
  Alarm_Time_New[i]<-as.numeric(z, units="secs")
  
  test<-toString(fire$Dispatch_Time[i])
  z<-strptime(test, "%m/%d/%y  %H:%M:%S")
  z<-as.POSIXlt(z, tz="EST" )
  Dispatch_Time_New[i]<-as.numeric(z, units="secs")
  
  test<-toString(fire$Dispatch_Time[i+1])
  z<-strptime(test, "%m/%d/%y  %H:%M:%S")
  z<-as.POSIXlt(z, tz="EST" )
  Next_Dispatch_Time_New[i]<-as.numeric(z, units="secs")
  
  test<-toString(fire$Arrival_Time[i])
  z<-strptime(test, "%m/%d/%y  %H:%M:%S")
  z<-as.POSIXlt(z, tz="EST" )
  Arrival_Time_New[i]<-as.numeric(z, units="secs")
  
  test<-toString(fire$Clear_Time[i])
  z<-strptime(test, "%m/%d/%y  %H:%M:%S")
  z<-as.POSIXlt(z, tz="EST" )
  Clear_Time_New[i]<-as.numeric(z, units="secs")
  
  Incident[i]<-fire$Incident.[i]
  Equipment[i]<-fire$Apparatus[i]
  District[i]<-fire$District[i]
  Incident_Type[i]<-fire$Incident.Type.Code[i]
}

fire_new<-c()
fire_new <- data.frame(Incident, Incident_Type, Equipment, Alarm_Time_New, Dispatch_Time_New, Arrival_Time_New, Clear_Time_New, Next_Dispatch_Time_New, District)
#head(fire_new)
#tail(fire_new) #check for next_dispatch time
#head(fire_new)
#fire_new$Next_Dispatch_Time_New
#head(fire_new)
#calculate the difference of times between columns and place values in the data frame
fire_new$Avg_Trans<-(fire_new$Arrival_Time_New-fire_new$Dispatch_Time_New)/60
fire_new$Avg_Dispatch<-(fire_new$Dispatch_Time_New-fire_new$Alarm_Time_New)/60
fire_new$Avg_Clear<-(fire_new$Clear_Time_New-fire_new$Arrival_Time_New)/60
fire_new$Avg_Clear2<-(fire_new$Clear_Time_New-fire_new$Dispatch_Time_New)/60
fire_new$Avg_Next_Call<-(fire_new$Next_Dispatch_Time_New-fire_new$Dispatch_Time_New)/60
fire_new$Avg_Cancelled<-(fire_new$Clear_Time_New-fire_new$Dispatch_Time_New)/60



#filter the times that are negative or unreasonable
average_transition_times<-fire_new$Avg_Trans[(fire_new$Avg_Trans>0 & fire_new$Avg_Trans<60)]# & fire_new$Equipment=="T4"]
average_dispatch_times<-fire_new$Avg_Dispatch[fire_new$Avg_Dispatch>=0 & fire_new$Avg_Dispatch<60]
average_clear_times<-fire_new$Avg_Clear[(fire_new$Avg_Clear>=0 & fire_new$Avg_Clear<720)]# & (fire_new$Equipment=="T4")]
average_clear_times2<-fire_new$Avg_Clear2[fire_new$Avg_Clear2>=0 & fire_new$Avg_Clear2<720]
average_next_call_times<-fire_new$Avg_Next_Call[fire_new$Avg_Next_Call>=0 & fire_new$Avg_Next_Call<720]


#average_clear_times2<-fire_new$Avg_Clear2[fire_new$Avg_Clear2>=0 & fire_new$Avg_Clear2<200]
#average_next_call_times<-fire_new$Avg_Next_Call[fire_new$Avg_Next_Call>2 & fire_new$Avg_Next_Call<600]
#find the averages of these times
trans_time<-mean(na.omit(average_transition_times))
trans_time

dispatch_time<-mean(na.omit(average_dispatch_times))
dispatch_time

clear_time<-mean(na.omit(average_clear_times))
clear_time

clear_time2<-mean(na.omit(average_clear_times2))
clear_time2

next_call_time<-mean(na.omit(average_next_call_times))
next_call_time

#plots
average_clear_times3<-na.omit(fire_new$Avg_Clear2[fire_new$Avg_Clear2>=0 & fire_new$Avg_Clear2<200])
dd<-data.frame(average_clear_times3)
ggplot(dd, aes(x=average_clear_times3)) + 
  geom_histogram(aes(y=..density..) , color="darkblue", fill="white", bins = 250) +
  stat_function(fun = dexp, 
                args = list(rate=(1/29.88)), na.rm = T, lwd = .75, col="red") +
  ggtitle("Service Times (Station 4)") +
  labs(y='Density', x='Service Time (min)')+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=28, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", size=20)) +
  theme(axis.text.x = element_text(size=12),
      axis.text.y=element_text(size=12))

average_next_call3<-na.omit(fire_new$Avg_Next_Call[fire_new$Avg_Next_Call>2 & fire_new$Avg_Next_Call<600])
dd<-data.frame(average_next_call3)
ggplot(dd, aes(x=average_next_call3)) + 
  geom_histogram(aes(y=..density..) , color="darkblue", fill="white", bins = 250) +
  stat_function(fun = dexp, 
                args = list(rate=(1/109.75)), na.rm = T, lwd = .75, col="red") +
  ggtitle("Inter-Arrival Times (Station 4)") +
  labs(y='Density', x='Inter-Arrival Time (min)')+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=28, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", size=20)) +
  theme(axis.text.x = element_text(size=12),
      axis.text.y=element_text(size=12))

par(mfrow = c(1,1))
par(bg = 'grey')
t<-na.omit(average_transition_times)
hist(average_transition_times, prob=T, breaks = 100, col='4', 
     main="Histogram of Transition Times", xlab="Transition Times (min)", ylab="Probability",
     xlim=c(0,20))
xfit<-seq(min(t),max(t),length=length(t)) 
yfit<-dnorm(xfit,mean=mean(t), sd=sd(t))
lines(xfit, yfit, col="red", lwd=2)
legend('topright', yfit, lty=1, lwd=2,col="red", cex=.8,
       c("Normal Distribution\nDensity Curve (mean=4.84)", mean(t)), text.col = "Black") # gives the legend lines the correct color and width

#t<-na.omit(average_dispatch_times)
#hist(average_dispatch_times, prob=T, breaks=100, col='4')
#xfit<-seq(min(t),5,length=1000) 
#yfit<-dexp(xfit,rate=1/mean(t))
#lines(xfit, yfit, col="red", lwd=2)

t<-na.omit(average_clear_times)
hist(average_clear_times, prob=T, breaks=250, col='4',
     main="Histogram of Clear Times (transit times excluded)", xlab="Clear Times (min)", ylab="Probability")
xfit<-seq(min(t),200,length=100) 
yfit<-dexp(xfit,rate=1/mean(t))
lines(xfit, yfit, col="red", lwd=2)
legend('topright', yfit, lty=1, lwd=2, col="red", cex=.8,
        c("Exponential Distribution\nDensity Curve (mean=28.37)"), text.col = "Black")

t<-na.omit(average_clear_times2)
hist(average_clear_times2, prob=T, breaks=200, col='4',
     main="Service Times (A4)", xlab="Service Time (min)", ylab="Probability")
xfit<-seq(min(t),200,length=100) 
yfit<-dexp(xfit,rate=1/mean(t))
lines(xfit, yfit, col="red", lwd=2)
axis(side=1,at=seq(0,200,50),lwd=3)
axis(side=2,at=seq(0,.03,.01),lwd=3)
legend('topright', yfit, lty=1, lwd=2,col="red", cex=.8,
       c("Exponential Distribution\nDensity Curve (mean=38.51)"), text.col = "Black") 

t<-na.omit(average_next_call_times)
hist(average_next_call_times, prob=T, breaks=100, col='4',
     main="Inter-Arrival Times (A4)", xlab="Inter-Arrival Time (min)", ylab="Probability")
xfit<-seq(min(t),max(t),length=length(t)) 
yfit<-dexp(xfit,rate=1/mean(t))
lines(xfit, yfit, col="red", lwd=2)
axis(side=1,at=seq(0,600,50),lwd=3)
axis(side=2,at=seq(0,.01,.001),lwd=3)
legend('topright', yfit, lty=1, lwd=2,col="red", cex=.8,
       c("Exponential Distribution\nDensity Curve (mean=196.27)"), text.col = "Black")

#overall service for District 4 including T4 and A4
m<-NewInput.MMC(lambda=1/next_call_time, mu=1/(clear_time2), c=1)
ro<-RO(QueueingModel(m))
lq<-Lq(QueueingModel(m))
w<-W(QueueingModel(m))

return_values<-c()
return_values[1]<-ro
return_values[2]<-lq
return_values[3]<-w
return_values

average_next_call_times<-na.omit(fire_new$Avg_Next_Call)[fire_new$Avg_Next_Call>720]
average_next_call_times
length(average_next_call_times)

total<-26291
x<-1152
p<-x/total
p
13801/(5*365)
v<-na.omit(average_next_call_times)
ex <- rexp(length(v), rate = 113)
ks.test(ex, "pexp", v)













x<-fire_new$Avg_Next_Call[fire_new$Avg_Next_Call>147 & fire_new$Avg_Next_Call<12*60]
x

length(na.omit(unique(fire_new$Incident)))
length(na.omit(unique(fire_new$Incident[fire_new$District==4 | fire_new$District!=4]))) 
length(na.omit(unique(fire_new$Incident[fire_new$District==4 & !is.na(fire_new$District)]))) 
length(na.omit(unique(fire_new$Incident[fire_new$District!=4 & !is.na(fire_new$District)]))) 

#ALL
19280-19221
19221-(13353+5868)

#A4
13267-13223
13223-(10005+3218)

#T4
11908-(8649+3259)
11937-11908

length(na.omit(unique(fire_new$Incident[fire_new$District==0])))
length(na.omit(unique(fire_new$Incident[fire_new$District==1])))
length(na.omit(unique(fire_new$Incident[fire_new$District==2])))
length(na.omit(unique(fire_new$Incident[fire_new$District==3])))
length(na.omit(unique(fire_new$Incident[fire_new$District==4])))
length(na.omit(unique(fire_new$Incident[fire_new$District==5])))
length(na.omit(unique(fire_new$Incident[fire_new$District==6])))
length(na.omit(unique(fire_new$Incident[fire_new$District==7])))
length(na.omit(unique(fire_new$Incident[fire_new$District==8])))
length(na.omit(unique(fire_new$Incident[fire_new$District==9])))
length(na.omit(unique(fire_new$Incident[fire_new$District==10])))
length(na.omit(unique(fire_new$Incident[fire_new$District==11])))
77+3+1780+565+53+9+1495+31+679+1185+89-10
19338-19309
19248+89
13372-19
v<-c()
i<-1
#df$Var[df$Var==""] <- "NA"
#fire_new$District[fire_new$District==""]<-"NA"
for (i in 1:26477){
#while (fire_new$District[i]!=1624587){
  n<-i+1
  if (fire_new$District[i]!=fire_new$District[n]){
    cat(i)
    cat(",")
    cat(n)
    cat(",")
    if (fire_new$Incident[i] == fire_new$Incident[n]){
      v<-fire_new$Incident[i]
    }
  }
#i<-i+1
}
v
length(fire_new$Incident)

#Incident Numbers that had differing districts
1108108  9
1109240  1
1116320  1
1116371  4
1116393
1116393  4
1119523  1,4
1120048  4
1121061  9
1121573
1121573  4
1121574
1121574  4
1201938  4
1202766  4
1204163  4
1204511  1
1204934  9
1205845  4 
1208865  1
1210438  5
1217110  4
1217213  4
1217462  4
1221655  5
1302566  4
1302695  4
1406480  4
1418627  4
1418632  4
1418645  4
1610114  5

fire_model<-na.omit(fire_new)
head(fire_model)
fire.lm<-lm(fire$Clear_Time~., data = fire_model)
summary(fire.lm)


length(na.omit(unique(unique(fire$Incident.))))
length(na.omit(unique(fire$Incident.[((fire$District==4) | (fire$District!=4))])))
length(na.omit(unique(fire$Incident.[fire$District==4])))
length(na.omit(unique((fire$Incident.[fire$District!=4]))))
19882-(13681+6144) #Total

length(na.omit(unique(unique(fire$Incident.[fire$Apparatus=="A4"]))))
length(na.omit(unique(fire$Incident.[((fire$District==4) | (fire$District!=4)) & fire$Apparatus=="A4"])))
length(na.omit(unique(fire$Incident.[fire$District==4 & fire$Apparatus=="A4"])))
length(na.omit(unique((fire$Incident.[fire$District!=4 & fire$Apparatus=="A4"]))))
12428-(9011+3388) #T4
13801-(10339+3420) #A4