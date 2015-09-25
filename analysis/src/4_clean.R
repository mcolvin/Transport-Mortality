trans[trans==-99]<-NA
weath[weath==-99]<-NA
flow[flow==-99]<-NA
temp.data[temp.data==-99]<-NA
WF.count[WF.count==-99]<-NA
trap.cnt[trap.cnt==-99]<-NA
truck_volumes[truck_volumes==-99]<-NA


######################## first trap day
this<-as.data.frame(table(trap.cnt$year,trap.cnt$location))
this<-subset(this,Freq>0)[,1:2]
first<-function(x){min(x[x[,5]>0,2])}
fst.day.trp<-NULL
for(i in 1:nrow(this))
	{
	work<-subset(trap.cnt,year==this[i,1] & location == this[i,2])
	fst.day.trp<-c(fst.day.trp, first(work))
	}

first.trap<-cbind(this,fst.day.trp)
colnames(first.trap)=c("year","location","fst.day.trp")

## 50% trapped at each dam
tots<-aggregate(trap.cnt[c(5)], by= trap.cnt[c(1,4)],sum)
names(tots)[3] = "gnd.tot"
tots<-merge(trap.cnt,tots)
tots$pct.trp<-tots$cum.tot/tots$gnd.tot

tots<-subset(tots,pct.trp>0.49)

trap50<-aggregate(tots[c(3)], by= tots[c(1,2)], min)
names(trap50)[3]= "trap50"


## WF count data
zeros<-subset(WF.count,spr_chinook_a_cum== 0)
run.strt<-aggregate(zeros[c(3)],by= zeros[c(1)], max)
colnames(run.strt)[2]="run.strt"
run.strt[,2]<- run.strt[,2]+1

## find value nearest 50%
doy50<-unlist(tapply(WF.count$prop_passed,WF.count$year, function(x){ which(abs(x-0.5)==min(abs(x-0.5)))}))
year<-as.data.frame(table(WF.count$year))[1:8,]
doy50<-cbind(year,doy50)[,-2]
names(doy50)[1]="year"    
run.size<- aggregate(WF.count[c(5)], by = WF.count[c(1)], max)  
names(run.size)[2]= "run.size"


# temperature data

## degree days first over to first trap
pre.dex<-merge(temp.data,run.strt)
pre.dex<-subset(pre.dex, (doy >= run.strt & doy <= run.strt+32)  & location == "Albany")
dex.main.dd<-aggregate(pre.dex[c(8)], by=pre.dex[c(1)],sum)
ff=merge(run.strt,first.trap)
ff=subset(ff, location == "Dexter Dam")
pre.dex<-merge(temp.data, ff)
pre.dex<-subset(pre.dex,doy > run.strt+32 & doy <= fst.day.trp)
dex.trib.dd<-aggregate(pre.dex[c(9)], by=pre.dex[c(1,2)],sum)

dex.trib.dd$DD.first<-dex.trib.dd$temp + dex.main.dd$temp[1:8]

pre.fos<-merge(temp.data,run.strt)
pre.fos<-subset(pre.fos, (doy >= run.strt & doy <= run.strt+16)  & location == "Albany")
fos.main.dd<-aggregate(pre.fos[c(8)], by=pre.fos[c(1)],sum)
ff=merge(run.strt,first.trap)
ff=subset(ff, location == "Foster Dam")
pre.fos<-merge(temp.data, ff)
pre.fos<-subset(pre.fos,doy > run.strt+16 & doy <= fst.day.trp )
fos.trib.dd<-aggregate(pre.fos[c(9)], by=pre.fos[c(1,2)],sum)

fos.trib.dd$DD.first<-fos.trib.dd$temp + fos.main.dd$temp[1:8]

DD.first<-rbind(dex.trib.dd,fos.trib.dd)[,-3]



## degree days 50% past WF to 50% trap
pre.dex<-merge(temp.data,doy50)
pre.dex<-subset(pre.dex, (doy >= doy50 & doy <= doy50+32)  & location == "Albany")
dex.main.dd<-aggregate(pre.dex[c(8)], by=pre.dex[c(1)],sum)
ff=merge(doy50,trap50)
ff=subset(ff, location == "Dexter Dam")
pre.dex<-merge(temp.data, ff)
pre.dex<-subset(pre.dex,doy > doy50+32 & doy <= trap50)
dex.trib.dd<-aggregate(pre.dex[c(9)], by=pre.dex[c(1,2)],sum)

dex.trib.dd$DD50<-dex.trib.dd$temp + dex.main.dd$temp[1:8]

pre.fos<-merge(temp.data,doy50)
pre.fos<-subset(pre.fos, (doy >= doy50 & doy <= doy50+16)  & location == "Albany")
fos.main.dd<-aggregate(pre.fos[c(8)], by=pre.fos[c(1)],sum)
ff=merge(doy50,trap50)
ff=subset(ff, location == "Foster Dam")
pre.fos<-merge(temp.data, ff)
pre.fos<-subset(pre.fos,doy > doy50+16 & doy <= trap50)
fos.trib.dd<-aggregate(pre.fos[c(9)], by=pre.fos[c(1,2)],sum)

fos.trib.dd$DD50<-fos.trib.dd$temp + fos.main.dd$temp[1:8]

DD50<-rbind(dex.trib.dd,fos.trib.dd)[,-3]


# discharge

## average discharge at dam first over plus travel to first trap
ff=merge(run.strt,first.trap)
ff=subset(ff, location == "Dexter Dam")
pre.dex<-merge(flow, ff)
pre.dex<-subset(pre.dex,doy > run.strt+32 & doy <= fst.day.trp)
dex.trib.Q<-aggregate(pre.dex$discharge, by=pre.dex[c(1,2)],mean)

dex.trib.Q$Q.first<-round(dex.trib.Q$x*0.0283168)
dex.trib.Q$x<- NULL

ff=merge(run.strt,first.trap)
ff=subset(ff, location == "Foster Dam")
pre.fos<-merge(flow, ff)
pre.fos<-subset(pre.fos,doy > run.strt+16 & doy <= fst.day.trp )
fos.trib.Q<-aggregate(pre.fos$discharge, by=pre.fos[c(1,2)],mean)

fos.trib.Q$Q.first<-round(fos.trib.Q$x*0.0283168)
fos.trib.Q$x<- NULL

Q.first<-rbind(dex.trib.Q,fos.trib.Q)

## average discharge 50% past WF to 50% trap
ff=merge(doy50,trap50)
ff=subset(ff, location == "Dexter Dam")
pre.dex<-merge(flow, ff)
pre.dex<-subset(pre.dex,doy > doy50+32 & doy <= trap50)
dex.trib.Q<-aggregate(pre.dex$discharge, by=pre.dex[c(1,2)],mean)

dex.trib.Q$Q50<-round(dex.trib.Q$x*0.0283168)
dex.trib.Q$x<- NULL

ff=merge(doy50,trap50)
ff=subset(ff, location == "Foster Dam")
pre.fos<-merge(flow, ff)
pre.fos<-subset(pre.fos,doy > doy50+16 & doy <= trap50)
fos.trib.Q<-aggregate(pre.fos$discharge, by=pre.fos[c(1,2)],mean)

fos.trib.Q$Q50<-round(fos.trib.Q$x*0.0283168)
fos.trib.Q$x<- NULL

Q50<-rbind(dex.trib.Q,fos.trib.Q)


Q.preds<-merge(Q.first,Q50)



##################################################combine annual data
annual.prds<-merge(doy50,run.size)

annual.prds<-merge(annual.prds,DD.first)

annual.prds<-merge(annual.prds,DD50)   

#### First add 2006 and 2007 to Dexter
dd<-annual.prds[1:2,]
dd$location <-"Dexter Dam"
dd$DD.first <- dd$DD.first*0.341 + 284
dd$DD50 <- 8.6 + 0.7*dd$DD50

annual.prds<-rbind(annual.prds,dd)   #################################### data you want

annual.prds<- merge(annual.prds,Q.preds, all =T)

## for now fill in  missing
#annual.prds[,7]<-ifelse(is.na(annual.prds[,7])==T,mean(annual.prds[,7],na.rm = T),annual.prds[,7])
#annual.prds[,8]<-ifelse(is.na(annual.prds[,8])==T,mean(annual.prds[,8],na.rm = T),annual.prds[,8])

## weather data
weath$date <- as.Date(as.character(weath$date), format="%m/%d/%Y")
weath$doy<-as.numeric(strftime(weath$date, format = "%j"))
weath$year<-as.numeric(strftime(weath$date, format = "%Y"))
weath$cloudcover <- weath$cloudcover/max(weath$cloudcover)
weath$maxT.C<-(weath$max.temperaturef -32)*5/9
weath<-weath[,c(1,25,24,26,22)] # get rid of garbage

## transport data
trans$date <- as.Date(as.character(trans$date), format="%m/%d/%Y")
trans$doy<-as.numeric(strftime(trans$date, format = "%j"))
trans$year<-as.numeric(strftime(trans$date, format = "%Y"))
trans.too<-merge(trans,weath,by=c("location","year","doy"), all.x=TRUE)
#truck_volumes$truckVolume<- as.numeric(as.character(truck_volumes$truckVolume))
#trans.too<- merge(trans.too,truck_volumes, by='trucknum',all.x=TRUE)

# get mean tank volume to assign to missing values
#def<-tapply(trans.too$truckVolume,trans.too$location, mean, na.rm = T)
#for(z in 1:2)
#	{
#	trans.too$truckVolume<-ifelse(trans.too$location == names(def)[z] & is.na(trans.too$truckVolume) == T,
#		as.numeric(def[z]),trans.too$truckVolume)}
#		
#		
#def<-tapply(trans.too$waterTempRelease,trans.too$location, mean, na.rm = T)
#for(z in 1:2)
#	{
#	trans.too$waterTempRelease<-ifelse(trans.too$location == names(def)[z] & is.na(trans.too$waterTempRelease) == T,
#		as.numeric(def[z]),trans.too$waterTempRelease)
#	}

## get rid of some garbage
#trans.too<-trans.too[,c(1:13,16,19:26)]

## gallons to m3
trans.too$truckVolume<-trans.too$truckVolume*0.00378541
## temp F to C
F2C<-function(x){ round((x-32)*5/9,2)}
trans.too$waterTempCollSite<-F2C(trans.too$waterTempCollSite)
trans.too$waterTempStart<-F2C(trans.too$waterTempStart)
trans.too$waterTempEnd<-F2C(trans.too$waterTempEnd)
trans.too$waterTempRelease<-F2C(trans.too$waterTempRelease)


## Crazy trapping covariates
haul.days<-unique(trans.too[,1:3])
trap.days<- trap.cnt
trap.days$day.bet<-0
for(z in 2:nrow(trap.days))
	{
	if(trap.days$location[z] == trap.days$location[z-1] & trap.days$year[z] == trap.days$year[z-1])
		{
		trap.days$day.bet[z]<- trap.days$doy[z]-trap.days$doy[z-1]
		}
	}
trans.too$av.day.bet.trp <-NA
trans.too$av.no.trp.bef <- NA

for(z in 1:nrow(trans.too)){
  work<- subset(trap.days,trap.days$doy <= trans.too$doy[z] & trap.days$location == trans.too$location[z] & 
                  trap.days$year == trans.too$year[z])
  trans.too$av.day.bet.trp[z] <- mean(work$day.bet)
  trans.too$av.no.trp.bef[z] <- mean(work$trap_total)
}
test<-merge(trans.too,annual.prds,by=c("location","year"))

### More fill in  missing with aves
#def<-tapply(test$av.day.bet.trp,test$location, mean, na.rm = T)
#test$av.day.bet.trp<-ifelse(test$location == names(def)[1] & is.nan(test$av.day.bet.trp) == T,as.numeric(def[1]),test$av.day.bet.trp)

#def<-tapply(test$av.no.trp.bef,test$location, mean, na.rm = T)
#test$av.no.trp.bef<-ifelse(test$location == names(def)[1] & is.nan(test$av.no.trp.bef) == T,as.numeric(def[1]),test$av.no.trp.bef)


test$tot.time<- test$loadingTime+test$haulingTime
#def<-tapply(test$tot.time,test$location, mean, na.rm = T)
#test[test$location == "Dexter Dam" & is.na(test$tot.time),]$tot.time<- def[1]
#test[test$location == "Foster Dam" & is.na(test$tot.time),]$tot.time<- def[2]

#test$tot.time<-ifelse(test$location == names(def)[1] & is.na(test$tot.time) == T,as.numeric(def[1]),test$tot.time)
#test[test$location == "Dexter Dam" & is.na(test$tot.time)]

test$delta.temp<- test$waterTempEnd- test$waterTempCollSite
test$fish.per.vol<- test$nFish/test$truckVolume

## create response variable DO THIS BEFORE STANDARDIZING DATA!!!
test$mort<-cbind((test$nLoss + test$nLikelyLoss), test$nFish - (test$nLoss + test$nLikelyLoss))



### now standardize predictors
prds<-c("location","waterbody","doy", "year","mort","trip.no", "waterTempCollSite", "truckVolume",
	"loadingTime","haulingTime","tot.time", "nFish", "maxT.C", 
	"cloudcover", "av.day.bet.trp", "av.no.trp.bef", "doy50", "run.size", 
	"DD.first", "DD50",  "Q.first", "Q50", "delta.temp","fish.per.vol")
dat<- test[,match(prds,names(test))]
dat_unstd<- dat
indx<- match(prds[-c(1,2,4,5)],names(dat))# columns to standardize
for(i in indx)
	{
	mn<- mean(dat[,i],na.rm=TRUE)
	sdd<- sd(dat[,i],na.rm=TRUE)
	dat[,i] <-ifelse(is.na(dat[,i]),0,(dat[,i]-mn)/sdd)
	
	}

	
	
	
dat<- dat[dat$waterbody!=-99,]
dat<- dat[!(is.na(dat$year)),]
dat$waterbody<- factor(dat$waterbody)
## create two variables to handle overdispersion
dat$site_yr = as.factor(paste(dat$year,dat$location,sep = "_"))
dat<- dat[!is.na(dat$mort[,1]),]
dat$samp = as.factor(c(1:nrow(dat)))


dat_unstd<- dat_unstd[dat_unstd$waterbody!=-99,]
dat_unstd<- dat_unstd[!(is.na(dat_unstd$year)),]
dat_unstd$waterbody<- factor(dat_unstd$waterbody)
## create two variables to handle overdispersion
dat_unstd$site_yr = as.factor(paste(dat_unstd$year,dat_unstd$location,sep = "_"))
dat_unstd<- dat_unstd[!is.na(dat_unstd$mort[,1]),]
dat_unstd$samp = as.factor(c(1:nrow(dat_unstd)))

