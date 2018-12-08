library(ggplot2)
library(dplyr)
library(car)
library(minpack.lm)
library(statmod)
library(growthcurver)
#Labeling raw data with times and treatment types
#Attention:Download file alongside script from Github
#Adjust import to your own computer download location
NOV1518rawdata<-as.data.frame(read.csv("~/Documents/15NOV18SPdata.csv"))
NOV1518timedata<-NOV1518rawdata
NOV1518timedata$Time<-c(0,0.25,0.5,0.75,
                       1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5,5.25,5.5,5.75,6,6.25,6.5,6.75,7,
                       7.25,7.5,7.75,8,8.25,8.5,8.75,9,9.25,9.5,9.75,10,10.25,10.5,10.75,11,11.25,11.5,11.75,12,12.25,12.5,12.75,13,13.25,
                       13.5,13.75,14,14.25,14.5,14.75,15,15.25,15.5,15.75,16)
LabeledGCdata<-NOV1518timedata
names(LabeledGCdata)<-c("Time","WtCON","WtCON","WtCON","WtUV30","WtUV30","WtUV30","WtUV60","WtUV60","WtUV60","blank",
                            "WtCON","WtCON","WtCON","WtUV30","WtUV30","WtUV30","WtUV60","WtUV60","WtUV60",
                            "MtCON","MtCON","MtCON","MtUV30","MtUV30","MtUV30","MtUV60","MtUV60","MtUV60","blank",
                            "MtCON","MtCON","MtCON","MtUV30","MtUV30","MtUV30","MtUV60","MtUV60","MtUV60")
GCnames<-c("WtCON","WtCON","WtCON","WtUV30","WtUV30","WtUV30","WtUV60","WtUV60","WtUV60",
           "WtCON","WtCON","WtCON","WtUV30","WtUV30","WtUV30","WtUV60","WtUV60","WtUV60",
           "MtCON","MtCON","MtCON","MtUV30","MtUV30","MtUV30","MtUV60","MtUV60","MtUV60",
           "MtCON","MtCON","MtCON","MtUV30","MtUV30","MtUV30","MtUV60","MtUV60","MtUV60")
#Data adjusted for anaylseGrothCurves function for pairwise t-tests
transposedGCdata<-t(NOV1518timedata)
transposedGCdatanoblank<-transposedGCdata[c(2:10, 12:29, 31:39),]
colnames(transposedGCdatanoblank)<-c("0","0.25","0.5","0.75","1","1.25","1.5","1.75","2","2.25","2.5","2.75","3","3.25","3.5","3.75",
                           "4","4.25","4.5","4.75","5","5.25","5.5","5.75","6","6.25","6.5","6.75","7",
                           "7.25","7.5","7.75","8","8.25","8.5","8.75","9","9.25","9.5","9.75","10","10.25","10.5","10.75","11","11.25",
                           "11.5","11.75","12","12.25","12.5","12.75","13","13.25",
                           "13.5","13.75","14","14.25","14.5","14.75","15","15.25","15.5","15.75","16")
rownames(transposedGCdatanoblank)<-c("WtCON","WtCON","WtCON","WtUV30","WtUV30","WtUV30","WtUV60","WtUV60","WtUV60",
                             "WtCON","WtCON","WtCON","WtUV30","WtUV30","WtUV30","WtUV60","WtUV60","WtUV60",
                             "MtCON","MtCON","MtCON","MtUV30","MtUV30","MtUV30","MtUV60","MtUV60","MtUV60",
                             "MtCON","MtCON","MtCON","MtUV30","MtUV30","MtUV30","MtUV60","MtUV60","MtUV60")
#Warning 10000 permutations requires lots of computing power
Pairwisedata<-as.data.frame(compareGrowthCurves(GCnames, transposedGCdatanoblank, nsim=10000))
#Making Averages matrix for graphing
YeastGcresults<-LabeledGCdata
YGCAverages<-as.data.frame(sapply(unique(names(YeastGcresults)), function(col) rowMeans(YeastGcresults[names(YeastGcresults) == col])))
trampYGCAverages<-t(YGCAverages)
#Creating data frames for error bars
c2<-as.data.frame(LabeledGCdata[,2:4])
c3<-as.data.frame(LabeledGCdata[,12:14])
WtCONstats<-cbind(c2,c3)
WtCONstats$means<-(rowMeans(WtCONstats, na.rm = FALSE, dims = 1))
WtCONstats$sd<-apply(WtCONstats[,1:6], 1, sd)
WtCONhourly<-WtCONstats[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),]
WtCONhourlybars<-rbind(WtCONhourly[1,],NA,NA,NA,WtCONhourly[2,],NA,NA,NA,WtCONhourly[3,],NA,NA,NA,WtCONhourly[4,],NA,NA,NA,
                       WtCONhourly[5,],NA,NA,NA,WtCONhourly[6,],NA,NA,NA,WtCONhourly[7,],NA,NA,NA,WtCONhourly[8,],NA,NA,NA,
                       WtCONhourly[9,],NA,NA,NA,WtCONhourly[10,],NA,NA,NA,WtCONhourly[11,],NA,NA,NA,WtCONhourly[12,],NA,NA,NA,
                       WtCONhourly[13,],NA,NA,NA,WtCONhourly[14,],NA,NA,NA,WtCONhourly[15,],NA,NA,NA,WtCONhourly[16,],NA,NA,NA,
                       WtCONhourly[17,])

c301<-as.data.frame(LabeledGCdata[,5:7])
c302<-as.data.frame(LabeledGCdata[,15:17])
WtUV30stats<-cbind(c301,c302)
WtUV30stats$means<-(rowMeans(WtUV30stats, na.rm = FALSE, dims = 1))
WtUV30stats$sd<-apply(WtUV30stats[,1:6], 1, sd)
WtUV30hourly<-WtUV30stats[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),]
WtUV30hourlybars<-rbind(WtUV30hourly[1,],NA,NA,NA,WtUV30hourly[2,],NA,NA,NA,WtUV30hourly[3,],NA,NA,NA,WtUV30hourly[4,],NA,NA,NA,
                        WtUV30hourly[5,],NA,NA,NA,WtUV30hourly[6,],NA,NA,NA,WtUV30hourly[7,],NA,NA,NA,WtUV30hourly[8,],NA,NA,NA,
                        WtUV30hourly[9,],NA,NA,NA,WtUV30hourly[10,],NA,NA,NA,WtUV30hourly[11,],NA,NA,NA,WtUV30hourly[12,],NA,NA,NA,
                        WtUV30hourly[13,],NA,NA,NA,WtUV30hourly[14,],NA,NA,NA,WtUV30hourly[15,],NA,NA,NA,WtUV30hourly[16,],NA,NA,NA,
                        WtUV30hourly[17,])

c601<-as.data.frame(LabeledGCdata[,8:10])
c602<-as.data.frame(LabeledGCdata[,18:20])
WtUV60stats<-cbind(c601,c602)
WtUV60stats$means<-(rowMeans(WtUV60stats, na.rm = FALSE, dims = 1))
WtUV60stats$sd<-apply(WtUV60stats[,1:6], 1, sd)
WtUV60hourly<-WtUV60stats[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),]
WtUV60hourlybars<-rbind(WtUV60hourly[1,],NA,NA,NA,WtUV60hourly[2,],NA,NA,NA,WtUV60hourly[3,],NA,NA,NA,WtUV60hourly[4,],NA,NA,NA,
                        WtUV60hourly[5,],NA,NA,NA,WtUV60hourly[6,],NA,NA,NA,WtUV60hourly[7,],NA,NA,NA,WtUV60hourly[8,],NA,NA,NA,
                        WtUV60hourly[9,],NA,NA,NA,WtUV60hourly[10,],NA,NA,NA,WtUV60hourly[11,],NA,NA,NA,WtUV60hourly[12,],NA,NA,NA,
                        WtUV60hourly[13,],NA,NA,NA,WtUV60hourly[14,],NA,NA,NA,WtUV60hourly[15,],NA,NA,NA,WtUV60hourly[16,],NA,NA,NA,
                        WtUV60hourly[17,])

m1<-as.data.frame(LabeledGCdata[,21:23])
m2<-as.data.frame(LabeledGCdata[,31:33])
MtCONstats<-cbind(m1,m2)
MtCONstats$means<-(rowMeans(MtCONstats, na.rm = FALSE, dims = 1))
MtCONstats$sd<-apply(MtCONstats[,1:6], 1, sd)
MtCONhourly<-MtCONstats[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),]
MtCONhourlybars<-rbind(MtCONhourly[1,],NA,NA,NA,MtCONhourly[2,],NA,NA,NA,MtCONhourly[3,],NA,NA,NA,MtCONhourly[4,],NA,NA,NA,
                    MtCONhourly[5,],NA,NA,NA,MtCONhourly[6,],NA,NA,NA,MtCONhourly[7,],NA,NA,NA,MtCONhourly[8,],NA,NA,NA,
                    MtCONhourly[9,],NA,NA,NA,MtCONhourly[10,],NA,NA,NA,MtCONhourly[11,],NA,NA,NA,MtCONhourly[12,],NA,NA,NA,
                    MtCONhourly[13,],NA,NA,NA,MtCONhourly[14,],NA,NA,NA,MtCONhourly[15,],NA,NA,NA,MtCONhourly[16,],NA,NA,NA,
                    MtCONhourly[17,])

m301<-as.data.frame(LabeledGCdata[,24:26])
m302<-as.data.frame(LabeledGCdata[,34:36])
MtUV30stats<-cbind(m301,m302)
MtUV30stats$means<-(rowMeans(MtUV30stats, na.rm = FALSE, dims = 1))
MtUV30stats$sd<-apply(MtUV30stats[,1:6], 1, sd)
MtUV30hourly<-MtUV30stats[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),]
MtUV30hourlybars<-rbind(MtUV30hourly[1,],NA,NA,NA,MtUV30hourly[2,],NA,NA,NA,MtUV30hourly[3,],NA,NA,NA,MtUV30hourly[4,],NA,NA,NA,
                        MtUV30hourly[5,],NA,NA,NA,MtUV30hourly[6,],NA,NA,NA,MtUV30hourly[7,],NA,NA,NA,MtUV30hourly[8,],NA,NA,NA,
                        MtUV30hourly[9,],NA,NA,NA,MtUV30hourly[10,],NA,NA,NA,MtUV30hourly[11,],NA,NA,NA,MtUV30hourly[12,],NA,NA,NA,
                        MtUV30hourly[13,],NA,NA,NA,MtUV30hourly[14,],NA,NA,NA,MtUV30hourly[15,],NA,NA,NA,MtUV30hourly[16,],NA,NA,NA,
                        MtUV30hourly[17,])

m601<-as.data.frame(LabeledGCdata[,27:29])
m602<-as.data.frame(LabeledGCdata[,37:39])
MtUV60stats<-cbind(m601,m602)
MtUV60stats$means<-(rowMeans(MtUV60stats, na.rm = FALSE, dims = 1))
MtUV60stats$sd<-apply(MtUV60stats[,1:6], 1, sd)
MtUV60hourly<-MtUV60stats[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),]
MtUV60hourlybars<-rbind(MtUV60hourly[1,],NA,NA,NA,MtUV60hourly[2,],NA,NA,NA,MtUV60hourly[3,],NA,NA,NA,MtUV60hourly[4,],NA,NA,NA,
                        MtUV60hourly[5,],NA,NA,NA,MtUV60hourly[6,],NA,NA,NA,MtUV60hourly[7,],NA,NA,NA,MtUV60hourly[8,],NA,NA,NA,
                        MtUV60hourly[9,],NA,NA,NA,MtUV60hourly[10,],NA,NA,NA,MtUV60hourly[11,],NA,NA,NA,MtUV60hourly[2,],NA,NA,NA,
                        MtUV60hourly[13,],NA,NA,NA,MtUV60hourly[14,],NA,NA,NA,MtUV60hourly[15,],NA,NA,NA,MtUV60hourly[16,],NA,NA,NA,
                        MtUV60hourly[17,])

#Plot if averages without offset error bars
Averagesplot<-ggplot(YGCAverages, aes(Time))+
  geom_line(aes(y=WtCON, colour="WtCON"), color="orange", position = pd)+
  geom_line(aes(y=WtUV30, colour="WtUV30"), color="black", position = pd)+
  geom_line(aes(y=WtUV60, colour="WtUV60"), color="cyan", position = pd)+
  geom_line(aes(y=MtCON, colour="MtCON"), color="green", position = pd)+
  geom_line(aes(y=MtUV30, colour="MtUV30"), color="blue", position = pd)+
  geom_line(aes(y=MtUV60, colour="MtUV60"), color="red", position = pd)+
  geom_errorbar(aes(ymin=MtUV60hourlybars$means-MtUV60hourlybars$sd, ymax=MtUV60hourlybars$means+MtUV60hourlybars$sd), color="red", position = pd)+
                  geom_errorbar(aes(ymin=MtUV30hourlybars$means-MtUV30hourlybars$sd, ymax=MtUV30hourlybars$means+MtUV30hourlybars$sd), color="blue", position = pd)+
                  geom_errorbar(aes(ymin=MtCONhourlybars$means-MtCONhourlybars$sd, ymax=MtCONhourlybars$means+MtCONhourlybars$sd), color="green", position = pd)+
                  geom_errorbar(aes(ymin=WtCONhourlybars$means-WtCONhourlybars$sd, ymax=WtCONhourlybars$means+WtCONhourlybars$sd), color="orange", position = pd)+
                  geom_errorbar(aes(ymin=WtUV60hourlybars$means-WtUV60hourlybars$sd, ymax=WtUV60hourlybars$means+WtUV60hourlybars$sd), color="cyan", position = pd)+
                  geom_errorbar(aes(ymin=WtUV30hourlybars$means-WtUV30hourlybars$sd, ymax=WtUV30hourlybars$means+WtUV30hourlybars$sd), position = pd)

#Data frames to create offset error bars for easier graph viewing
MtUV30hourlyoffset<-MtUV30stats[c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62),]
MtUV30hourlybarsoffset<-rbind(NA,MtUV30hourlyoffset[1,],NA,NA,NA,MtUV30hourlyoffset[2,],NA,NA,NA,MtUV30hourlyoffset[3,],NA,NA,NA,MtUV30hourlyoffset[4,],NA,NA,NA,
                        MtUV30hourlyoffset[5,],NA,NA,NA,MtUV30hourlyoffset[6,],NA,NA,NA,MtUV30hourlyoffset[7,],NA,NA,NA,MtUV30hourlyoffset[8,],NA,NA,NA,
                        MtUV30hourlyoffset[9,],NA,NA,NA,MtUV30hourlyoffset[10,],NA,NA,NA,MtUV30hourlyoffset[11,],NA,NA,NA,MtUV30hourlyoffset[12,],NA,NA,NA,
                        MtUV30hourlyoffset[13,],NA,NA,NA,MtUV30hourlyoffset[14,],NA,NA,NA,MtUV30hourlyoffset[15,],NA,NA,NA,MtUV30hourlyoffset[16,],NA,NA,NA)

WtUV60hourlyoffset<-WtUV60stats[c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62),]
WtUV60hourlybarsoffset<-rbind(NA,WtUV60hourlyoffset[1,],NA,NA,NA,WtUV60hourlyoffset[2,],NA,NA,NA,WtUV60hourlyoffset[3,],NA,NA,NA,WtUV60hourlyoffset[4,],NA,NA,NA,
                        WtUV60hourlyoffset[5,],NA,NA,NA,WtUV60hourlyoffset[6,],NA,NA,NA,WtUV60hourlyoffset[7,],NA,NA,NA,WtUV60hourlyoffset[8,],NA,NA,NA,
                        WtUV60hourlyoffset[9,],NA,NA,NA,WtUV60hourlyoffset[10,],NA,NA,NA,WtUV60hourlyoffset[11,],NA,NA,NA,WtUV60hourlyoffset[12,],NA,NA,NA,
                        WtUV60hourlyoffset[13,],NA,NA,NA,WtUV60hourlyoffset[14,],NA,NA,NA,WtUV60hourlyoffset[15,],NA,NA,NA,WtUV60hourlyoffset[16,],NA,NA,NA)


MtCONhourlyoffset<-MtCONstats[c(3,7,11,15,19,23,27,31,35,39,43,47,51,55,59,63),]
MtCONhourlybarsoffset<-rbind(NA,NA,MtCONhourlyoffset[1,],NA,NA,NA,MtCONhourlyoffset[2,],NA,NA,NA,MtCONhourlyoffset[3,],NA,NA,NA,MtCONhourlyoffset[4,],NA,NA,NA,
                       MtCONhourlyoffset[5,],NA,NA,NA,MtCONhourlyoffset[6,],NA,NA,NA,MtCONhourlyoffset[7,],NA,NA,NA,MtCONhourlyoffset[8,],NA,NA,NA,
                       MtCONhourlyoffset[9,],NA,NA,NA,MtCONhourlyoffset[10,],NA,NA,NA,MtCONhourlyoffset[11,],NA,NA,NA,MtCONhourlyoffset[12,],NA,NA,NA,
                       MtCONhourlyoffset[13,],NA,NA,NA,MtCONhourlyoffset[14,],NA,NA,NA,MtCONhourlyoffset[15,],NA,NA,NA,MtCONhourlyoffset[16,],NA,NA)

WtCONhourlyoffset<-WtCONstats[c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64),]
WtCONhourlybarsoffset<-rbind(NA,NA,NA,WtCONhourlyoffset[1,],NA,NA,NA,WtCONhourlyoffset[2,],NA,NA,NA,WtCONhourlyoffset[3,],NA,NA,NA,WtCONhourlyoffset[4,],NA,NA,NA,
                       WtCONhourlyoffset[5,],NA,NA,NA,WtCONhourlyoffset[6,],NA,NA,NA,WtCONhourlyoffset[7,],NA,NA,NA,WtCONhourlyoffset[8,],NA,NA,NA,
                       WtCONhourlyoffset[9,],NA,NA,NA,WtCONhourlyoffset[10,],NA,NA,NA,WtCONhourlyoffset[11,],NA,NA,NA,WtCONhourlyoffset[12,],NA,NA,NA,
                       WtCONhourlyoffset[13,],NA,NA,NA,WtCONhourlyoffset[14,],NA,NA,NA,WtCONhourlyoffset[15,],NA,NA,NA,WtCONhourlyoffset[16,],NA)
#Graph with offset error bars
Averagesplotoffset<-ggplot(YGCAverages, aes(Time))+
  geom_line(aes(y=WtCON, color="WtCON"))+
  geom_line(aes(y=WtUV30, color="WtUV30"))+
  geom_line(aes(y=WtUV60, color="WtUV60"))+
  geom_line(aes(y=MtCON, color="MtCON"))+
  geom_line(aes(y=MtUV30, color="MtUV30"))+
  geom_line(aes(y=MtUV60, color="MtUV60"))+
  geom_errorbar(aes(ymin=MtUV60hourlybars$means-MtUV60hourlybars$sd, ymax=MtUV60hourlybars$means+MtUV60hourlybars$sd, color="MtUV60"))+
  geom_errorbar(aes(ymin=MtUV30hourlybarsoffset$means-MtUV30hourlybarsoffset$sd, ymax=MtUV30hourlybarsoffset$means+MtUV30hourlybarsoffset$sd, color="MtUV30"))+
  geom_errorbar(aes(ymin=MtCONhourlybarsoffset$means-MtCONhourlybarsoffset$sd, ymax=MtCONhourlybarsoffset$means+MtCONhourlybarsoffset$sd, color="MtCON"))+
  geom_errorbar(aes(ymin=WtCONhourlybarsoffset$means-WtCONhourlybarsoffset$sd, ymax=WtCONhourlybarsoffset$means+WtCONhourlybarsoffset$sd, color="WtCON"))+
  geom_errorbar(aes(ymin=WtUV60hourlybarsoffset$means-WtUV60hourlybarsoffset$sd, ymax=WtUV60hourlybarsoffset$means+WtUV60hourlybarsoffset$sd, color="WtUV60"))+
  geom_errorbar(aes(ymin=WtUV30hourlybars$means-WtUV30hourlybars$sd, ymax=WtUV30hourlybars$means+WtUV30hourlybars$sd, color="WtUV30"))+ggtitle("Average Yeast Growth Curves")+
  ylab("Optical Density")+xlab("Time (hours)")
#Plot without any error bars
Averagesnoerrorplot<-ggplot(YGCAverages, aes(Time))+
  geom_point(aes(y=WtCON, colour="WtCON"), color="orange")+
  geom_point(aes(y=WtUV30, colour="WtUV30"), color="black")+
  geom_point(aes(y=WtUV60, colour="WtUV60"), color="cyan")+
  geom_point(aes(y=MtCON, colour="MtCON"), color="green")+
  geom_point(aes(y=MtUV30, colour="MtUV30"), color="blue")+
  geom_point(aes(y=MtUV60, colour="MtUV60"), color="red")

#Creation of linear model for each growth curve for potential future analysis
lm(YGCAverages$WtCON~YGCAverages$Time)
theabsolutewitWtCON<-nls(log(WtCON)~log(ph1/(1+exp(-(ph2+ph3*Time)))),
                        start=list(ph1=1.346266666,ph2=-0.14438,ph3=0.089442),data=YGCAverages,trace=TRUE)
newwitWtCON<-nls(WtCON~(ph1/(1+exp(-(ph2+ph3*Time)))), YGCAverages, start = coef(theabsolutewitWtCON))
a<-coef(newwitWtCON)[1]
b<-coef(newwitWtCON)[2]
c<-coef(newwitWtCON)[3]
x<-c(min(YGCAverages$Time):max(YGCAverages$Time))
y<-a/(1+exp(-(b+c*x)))
predict1<-data.frame(x,y)
ggplot(data=YGCAverages,aes(x=Time,y=WtCON))+geom_point()+geom_line(data = predict1,aes(x=x,y=y), size=1)

lm(YGCAverages$WtUV30~YGCAverages$Time)
theabsolutewitWtUV30<-nlsLM(log(WtUV30)~log(ph1/(1+exp(-(ph2+ph3*Time)))),
                             start=list(ph1=1.3666,ph2=0.25414,ph3=0.08389),data=YGCAverages,trace=TRUE)
newwitWtUV30<-nls(WtUV30~(ph1/(1+exp(-(ph2+ph3*Time)))), YGCAverages, start = coef(theabsolutewitWtUV30))
aa<-coef(newwitWtUV30)[1]
bb<-coef(newwitWtUV30)[2]
cc<-coef(newwitWtUV30)[3]
xx<-c(min(YGCAverages$Time):max(YGCAverages$Time))
yy<-aa/(1+exp(-(bb+cc*x)))
predict2<-data.frame(xx,yy)
ggplot(data=YGCAverages,aes(x=Time,y=WtUV30))+geom_point()+geom_line(data = predict2,aes(x=xx,y=yy), size=1)

lm(YGCAverages$WtUV60~YGCAverages$Time)
theabsolutewitWtUV60<-nlsLM(log(WtUV60)~log(ph1/(1+exp(-(ph2+ph3*Time)))),
                             start=list(ph1=1.355,ph2=-0.17114,ph3=0.08678),data=YGCAverages,trace=TRUE)
newwitWtUV60<-nls(WtUV60~(ph1/(1+exp(-(ph2+ph3*Time)))), YGCAverages, start = coef(theabsolutewitWtUV60))
ab<-coef(newwitWtUV60)[1]
bc<-coef(newwitWtUV60)[2]
cd<-coef(newwitWtUV60)[3]
x1<-c(min(YGCAverages$Time):max(YGCAverages$Time))
y1<-ab/(1+exp(-(bc+cd*x)))
predict3<-data.frame(x1,y1)
ggplot(data=YGCAverages,aes(x=Time,y=WtUV60))+geom_point()+geom_line(data = predict3,aes(x=x1,y=y1), size=1)

lm(YGCAverages$MtCON~YGCAverages$Time)
theabsolutewitMtCON<-nlsLM(log(MtCON)~log(ph1/(1+exp(-(ph2+ph3*Time)))),
                                start=list(ph1=1.35,ph2=-0.05414,ph3=0.09187),data=YGCAverages,trace=TRUE)
newwitMtCON<-nls(MtCON~(ph1/(1+exp(-(ph2+ph3*Time)))), YGCAverages, start = coef(theabsolutewitMtCON))
aMC<-coef(newwitMtCON)[1]
bMC<-coef(newwitMtCON)[2]
cMC<-coef(newwitMtCON)[3]
xMC<-c(min(YGCAverages$Time):max(YGCAverages$Time))
yMC<-aMC/(1+exp(-(bMC+cMC*x)))
predictMC<-data.frame(xMC,yMC)
ggplot(data=YGCAverages,aes(x=Time,y=MtCON))+geom_point()+geom_line(data = predictMC,aes(x=xMC,y=yMC), size=1)

lm(YGCAverages$MtUV30~YGCAverages$Time)
theabsolutewitMtUV30<-nlsLM(log(MtUV30)~log(ph1/(1+exp(-(ph2+ph3*Time)))),
                                start=list(ph1=1.355,ph2=-0.25110,ph3=0.08579),data=YGCAverages,trace=TRUE)
newwitMtUV30<-nls(MtUV30~(ph1/(1+exp(-(ph2+ph3*Time)))), YGCAverages, start = coef(theabsolutewitMtUV30))
aM3<-coef(newwitMtUV30)[1]
bM3<-coef(newwitMtUV30)[2]
cM3<-coef(newwitMtUV30)[3]
xM3<-c(min(YGCAverages$Time):max(YGCAverages$Time))
yM3<-aM3/(1+exp(-(bM3+cM3*x)))
predictM3<-data.frame(xM3,yM3)
ggplot(data=YGCAverages,aes(x=Time,y=MtUV30))+geom_point()+geom_line(data = predictM3,aes(x=xM3,y=yM3), size=1)

lm(YGCAverages$MtUV60~YGCAverages$Time)
theabsolutewitMtUV60<-nlsLM(log(MtUV60)~log(ph1/(1+exp(-(ph2+ph3*Time)))),
                                start=list(ph1=0.135,ph2=-0.015283,ph3=0.003767),data=YGCAverages,trace=TRUE)
newwitMtUV60<-nlsLM(MtUV60~(ph1/(1+exp(-(ph2+ph3*Time)))), YGCAverages, start = coef(theabsolutewitMtUV60))
aM6<-coef(newwitMtUV60)[1]
bM6<-coef(newwitMtUV60)[2]
cM6<-coef(newwitMtUV60)[3]
xM6<-c(min(YGCAverages$Time):max(YGCAverages$Time))
yM6<-aM6/(1+exp(-(bM6+cM6*x)))
predictM6<-data.frame(xM6,yM6)
ggplot(data=YGCAverages,aes(x=Time,y=MtUV60))+geom_point()+geom_line(data = predictM6,aes(x=xM6,y=yM6), size=1)


