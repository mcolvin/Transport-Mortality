mort_risk<- function(x,nn,p)
	{# FUNCTION THAT RETURNS THE PROBABILITY OF 
	#  A MORTALITY GIVEN THE NUMBER OF FISH IN 
	# THE TRANSPORT AND PREDICTED MORTALITY PROBABILITY
	x<-1-pbinom(0,x[nn],prob=x[p])
	return(x)
	}
	
	
optimal<- function(mortWght=0.5,maxDens=200,maxEffort=200,location="Dexter Dam")
	{
	ms<- tables(3)
	confModSet<- ms[ms$location==location,]	
	confModSet<- confModSet[1:(which(confModSet$predictor=="Intercept only")-1),]	
	confModSet$w<-confModSet$w/sum(confModSet$w)	

	minPerFish<- ifelse(location=="Foster Dam",4.6,0.25) #Foster median nFish/loading time

	opt_dat<- expand.grid(
		n= seq(10,400,by=10),
		n_trips=c(1:20),
		truckVolume=seq(1,11,by=0.5))
	opt_dat$left_over<- opt_dat$n%%opt_dat$n_trips # number left over
	opt_dat$fishPerHaul<- round(opt_dat$n/opt_dat$n_trips)
	opt_dat<- opt_dat[order(opt_dat$n,
		opt_dat$truckVolume,opt_dat$n_trips),]
	opt_dat$combo_id<- paste(opt_dat$n,opt_dat$truckVolume,sep="-")		
	opt_dat$id<-1:nrow(opt_dat)


	fish<-as.data.frame(lapply(opt_dat,
			   function(x) rep(x,opt_dat$n_trips)))
	fish$add<- 0
	fish$add<-unlist(lapply(opt_dat$n_trips,function(x)
		{c(1:x)}))
	fish$nfishx<-ifelse(fish$left_over >= fish$add & fish$left_over >0,
		fish$fishPerHaul+1,fish$fishPerHaul)

	# center variables prior to model fitting
	if(location=="Dexter Dam")
		{
		fish$loadingTime_raw<- fish$nfishx*minPerFish
		fish$fish_per_vol_raw<- fish$nfishx/fish$truckVolume
		fish$fish_per_vol <-scale(fish$fish_per_vol_raw,
			center=dex['fish_per_vol',]$mn, 
				scale=dex['fish_per_vol',]$sdd)
		fish$y<-predict(out_dexter[[confModSet$model_indx[1]]],
			fish,re.form=NA)*confModSet$w[1] 	
		}
	
	if(location=="Foster Dam")
		{
		fish$loadingTime_raw<- fish$nfishx*minPerFish
		fish$fish_per_vol<- fish$nfishx/fish$truckVolume
		fish$haulingTime_raw<- fos['loadingTime',]$mn
		fish$dd_01_raw<- fos['dd_01',]$mn
		fish$Q_01_raw<- fos['Q_01',]$mn
		
		fish$loadingTime<-scale(fish$loadingTime_raw,
			center=fos['loadingTime',]$mn, 
				scale=fos['loadingTime',]$sdd)
		fish$tot_time <-scale((fish$loadingTime_raw+fish$loadingTime_raw),
			center=fos['tot_time',]$mn, 
			scale=fos['tot_time',]$sdd) 
		fish$dd_01 <-scale((fish$dd_01_raw),
			center=fos['dd_01',]$mn, 
			scale=fos['dd_01',]$sdd) 
		fish$Q_01 <-scale((fish$Q_01_raw),
			center=fos['Q_01',]$mn, 
			scale=fos['Q_01',]$sdd) 
			
				
		fish$y1<-predict(out_foster[[confModSet$model_indx[1]]],
			fish,re.form=NA)*confModSet$w[1] 	
		fish$y2<-predict(out_foster[[confModSet$model_indx[2]]],
			fish,re.form=NA)*confModSet$w[2] 	
		fish$y3<-predict(out_foster[[confModSet$model_indx[3]]],
			fish,re.form=NA)*confModSet$w[3] 				
		fish$y4<-predict(out_foster[[confModSet$model_indx[4]]],
			fish,re.form=NA)*confModSet$w[4] 	
		fish$y<- fish$y1+fish$y2+fish$y3+fish$y4
		}
		
	fish$p<- plogis(fish$y)		
	fish$risk<- pbinom(0,fish$nfishx,prob=fish$p,lower.tail=TRUE)
	
	# CALCULATE RISK FOR EACH COMBO AS THE PRODUCT FOR EACH TRIP
	mdat<- 1-tapply(fish$risk, fish$id, prod)
	mdat<- data.frame(id=row.names(mdat),risk=c(mdat))
	
	# PROOF FOR RISK CALCULATION
	#p<- c(0.08,0.03,0.05)
	#n<- 25
	#out<-matrix(0,3,1000000)
	#for(i in 1:1000000)
	#{
	#out[,i]<- rbinom(length(p),n,p)
	#}
	
	#out[out>0]<-1
	#x<-apply(out,2,max)
	#table(x)/length(x)
	
	#risk<- 1-prod(pbinom(0,n,prob=p,lower.tail=TRUE))
	#prod(pbinom(0,n,prob=p,lower.tail=TRUE))
	
	
	opt_dat<- merge(opt_dat,mdat,by="id")	
	opt_dat$loadingTime<- opt_dat$n*minPerFish
	opt_dat$haulingTime<- ifelse(location=="Dexter Dam", 77.3,37.4)
	#opt_dat$haulingTime<- ifelse(location=="Dexter Dam", 15,15)
	opt_dat$dailyTime<-(opt_dat$loadingTime+
		opt_dat$n_trips*opt_dat$haulingTime*2)/60
	opt_dat$risk_u<- (opt_dat$risk-max(opt_dat$risk))/(min(opt_dat$risk)-max(opt_dat$risk))
	opt_dat$time_u<- (opt_dat$dailyTime-max(opt_dat$dailyTime))/(min(opt_dat$dailyTime)-max(opt_dat$dailyTime))
	opt_dat$U<- mortWght*opt_dat$risk_u+(1-mortWght)*opt_dat$time_u
	opt_dat$max_fish_hauled<- (opt_dat$fishPerHaul+ifelse(opt_dat$left_over>0,1,0))
	opt_dat$fishDens<- opt_dat$max_fish_hauled/opt_dat$truckVolume
	opt_dat$U<- ifelse(opt_dat$fishDens>maxDens | opt_dat$dailyTime>maxEffort, -1, opt_dat$U)
	opt_dat<-subset(opt_dat,U>=0)
	opt_dat<- opt_dat[order(opt_dat$n,opt_dat$truckVolume,opt_dat$n_trips),]
	ppp<-ddply(opt_dat,.(n,truckVolume),summarize,
		n_trips= n_trips[which.max(U)],
		dailyTime= dailyTime[which.max(U)],
		fishDens= fishDens[which.max(U)])
	ppp$location=location
	return(ppp)
	}

	