trans[trans==-99]<-NA
weath[weath==-99]<-NA
flow[flow==-99]<-NA
temp_data[temp_data==-99]<-NA
WF_count[WF_count==-99]<-NA
trap_cnt[trap_cnt==-99]<-NA
trap_cnt<- trap_cnt[order(trap_cnt$location,trap_cnt$year,trap_cnt$doy),]


######################## first trap day
trap_cnt$cum_total<- unlist(tapply(trap_cnt$trap_total, 
	paste(as.character(trap_cnt$location),trap_cnt$year),cumsum))
first_trap<-aggregate(doy~year+location,trap_cnt,min,subset=cum_total>0)

## 50% trapped at each dam
tmp<- aggregate(cum_total~year+location,trap_cnt,max,subset=cum.tot>0)
trap_cnt<- merge(trap_cnt,tmp, by=c("location","year"), all.x=TRUE)
trap_cnt$p<- trap_cnt$cum_total.x/trap_cnt$cum_total.y
fifty_trap<- aggregate(doy~year+location,trap_cnt,max, subset=p<=0.5)
annual_values<- merge(first_trap, fifty_trap, by=c("location","year"))
# NUMBER OF DAYS BETWEEN TRAP OPERATIONS
trap_cnt$day_bet<- c(0,trap_cnt$doy[-1]-trap_cnt$doy[-nrow(trap_cnt)])* c(1,ifelse(trap_cnt$year[-1]==trap_cnt$year[-nrow(trap_cnt)],1,0))
names(annual_values)[3]= "trap01"
names(annual_values)[4]= "trap50"


# WILLAMETTE FALLS COUNT DATA
tmp<-aggregate(doy~year,WF_count,min,subset=prop_passed>0)
names(tmp)[2]<- "doy01"
annual_values<- merge(annual_values,tmp,by=c("year"),all.x=TRUE)
tmp<-aggregate(doy~year,WF_count,min,subset=prop_passed>=0.5)
names(tmp)[2]<- "doy50"
annual_values<- merge(annual_values,tmp,by=c("year"),all.x=TRUE)
tmp<-aggregate(spr_chinook_a_cum~year,WF_count,max)
names(tmp)[2]<- "run_size"
annual_values<- merge(annual_values,tmp,by=c("year"),all.x=TRUE)

# temperature data

## degree days first over willamette falls to first trap
annual_values$Q_50<-annual_values$Q_01<-annual_values$dd_50<-annual_values$dd<-annual_values$dd_alb<-NA
for(i in 1:nrow(annual_values))
	{
	tdat<- subset(temp_data, doy%in% c(annual_values$doy01[i]:annual_values$trap01[i]) & 
		location==as.character(annual_values$location[i]))
	fill<- approxfun(tdat$doy,tdat$temp)	
	 tdat$temp<-ifelse(is.na(tdat$temp), fill(tdat$doy),tdat$temp)
	annual_values$dd_01[i]<- sum(tdat$temp)
	
	tdat<- subset(temp_data, doy%in% c(annual_values$doy01[i]:annual_values$trap01[i]) & 
		location=="Albany")
	fill<- approxfun(tdat$doy,tdat$temp)	
	 tdat$temp<-ifelse(is.na(tdat$temp), fill(tdat$doy),tdat$temp)
	annual_values$dd_alb[i]<- sum(tdat$temp)

	tdat<- subset(temp_data, doy%in% c(annual_values$doy50[i]:annual_values$trap50[i]) & 
		location==as.character(annual_values$location[i]))
	fill<- approxfun(tdat$doy,tdat$temp)	
	tdat$temp<-ifelse(is.na(tdat$temp), fill(tdat$doy),tdat$temp)
	annual_values$dd_50[i]<- sum(tdat$temp)
	
	# discharge
	tdat<- subset(flow, doy%in% c(annual_values$doy01[i]:annual_values$trap01[i]) & 
		location==as.character(annual_values$location[i]))
	fill<- approxfun(tdat$doy,tdat$discharge)	
	tdat$temp<-ifelse(is.na(tdat$discharge), fill(tdat$doy),tdat$discharge)
	annual_values$Q_01[i]<- mean(tdat$discharge)	
	
	tdat<- subset(flow, doy%in% c(annual_values$doy50[i]:annual_values$trap50[i]) & 
		location==as.character(annual_values$location[i]))
	fill<- approxfun(tdat$doy,tdat$discharge)	
	tdat$temp<-ifelse(is.na(tdat$discharge), fill(tdat$doy),tdat$discharge)
	annual_values$Q_50[i]<- mean(tdat$discharge)	
	
	}

# DAILY AND LOCATION DATA

## weather data
weath$date <- as.Date(as.character(weath$date), format="%m/%d/%Y")
weath$doy<-as.numeric(strftime(weath$date, format = "%j"))
weath$year<-as.numeric(strftime(weath$date, format = "%Y"))
weath$cloudcover <- weath$cloudcover/max(weath$cloudcover)
weath$maxT_C<-(weath$max.temperaturef -32)*5/9
weath<-weath[,c(1,25,24,26,22)] # get rid of garbage

## transport data
trans$date <- as.Date(as.character(trans$date), format="%m/%d/%Y")
trans$doy<-as.numeric(strftime(trans$date, format = "%j"))
trans$year<-as.numeric(strftime(trans$date, format = "%Y"))
trans<-merge(trans,weath,by=c("location","year","doy"), all.x=TRUE)

## CONVERT GALLONS TO M^3 
trans$truckVolume<-trans$truckVolume*0.00378541
## CONVERT TEMPERATURE FROM F TO C
F2C<-function(x){round((x-32)*5/9,2)}
trans$waterTempCollSite<-F2C(trans$waterTempCollSite)
trans$waterTempStart<-F2C(trans$waterTempStart)
trans$waterTempEnd<-F2C(trans$waterTempEnd)
trans$waterTempRelease<-F2C(trans$waterTempRelease)
trans$trip_no<-1
for(i in 2:nrow(trans))
	{
	trans$trip_no[i]<-ifelse(trans$doy[i]==trans$doy[i-1],trans$trip_no[i-1]+1,1)	
	}
trans$tot_time<- trans$loadingTime+trans$haulingTime
trans$delta_temp<- trans$waterTempEnd- trans$waterTempCollSite
trans$fish_per_vol<- trans$nFish/trans$truckVolume

## create response variable DO THIS BEFORE STANDARDIZING DATA!!!
trans$mort<-cbind((trans$nLoss + trans$nLikelyLoss), trans$nFish - (trans$nLoss + trans$nLikelyLoss))
trans<- merge(trans,annual_values, by=c("location","year"))
trans<- merge(trans, trap_cnt[,c("location","year","doy","trap_total","day_bet")],by=c("location","year","doy"))
dat_unstd<- trans	


## STANDARDIZE DATA PRIOR TO ANALYSIS

out<- trans
out<- out[out$waterbody!=-99,]		
out$waterbody<- factor(out$waterbody)
## create two variables to handle overdispersion
out$site_yr = as.factor(paste(out$year,out$location,sep = "_"))
out<- out[!is.na(out$mort[,1]),]
out$samp = as.factor(c(1:nrow(out)))
indx<- match(prds[prds$standardize==1,1],names(dat_unstd))# columns to standardize

# get means and sdd to standarize with same as tbl 2
# FOSTER
mn<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,mean,na.rm=TRUE)
sdd<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,sd,na.rm=TRUE)
mnn<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,min,na.rm=TRUE)
mxx<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,max,na.rm=TRUE)
fos<- as.data.frame(cbind(indx,mn,sdd,mnn,mxx))
# DEXTER			
mn<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,mean,na.rm=TRUE)
sdd<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,sd,na.rm=TRUE)
mnn<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,min,na.rm=TRUE)
mxx<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,max,na.rm=TRUE)
dex<- as.data.frame(cbind(indx,mn,sdd,mnn,mxx))


for(i in indx)
	{
	out[out$location=="Foster Dam",i]<- scale(out[out$location=="Foster Dam",i],
		center= fos[which(fos[,1]==i),2],
		scale = fos[which(fos[,1]==i),3])
	out[out$location=="Dexter Dam",i]<- scale(out[out$location=="Dexter Dam",i],
		center= dex[which(dex[,1]==i),2],
		scale = dex[which(dex[,1]==i),3])
	out[,i]<-ifelse(is.na(out[,i]),0,out[,i])
	}
out$doy_sqrd<- out$doy^2
out$year<- as.factor(out$year)
out$samp<-as.factor(out$samp)
out$trip_no<- as.factor(out$trip_no)
dat<- out

prds<-rbind(prds,data.frame(pred="1",
	predictor="Intercept only",standardize=0))

#aggregate(fish_per_vol~location,dat_unstd,max)