tables<- function(n,model_fits=NULL, modsel_foster=NULL, modsel_dexter=NULL){
	if(n==2)
		{# TABLE OF MEANS, STD. DEVS, MINIMUMS, AND MAXIMUMS
		### now standardize predictors
			
		indx<- match(prds[,1],names(dat_unstd))# columns to standardize
		
		# FOSTER
		mn<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,mean,na.rm=TRUE)
		sdd<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,sd,na.rm=TRUE)
		mnn<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,min,na.rm=TRUE)
		mxx<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,max,na.rm=TRUE)
		tmp<- data.frame(Predictor=prds[,2],
			V1=paste(round(mn,2)," (", round(sdd,2),")",sep=""),
			Minimum=round(mnn,2),
			Maximum=round(mxx,2))
		names(tmp)[2]<- "Mean (SD)"
		# DEXTER			
		mn<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,mean,na.rm=TRUE)
		sdd<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,sd,na.rm=TRUE)
		mnn<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,min,na.rm=TRUE)
		mxx<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,max,na.rm=TRUE)
		tmpp<- data.frame(Predictor=prds[,2],
			V1=paste(round(mn,2)," (", round(sdd,2),")",sep=""),
			Minimum=round(mnn,2),
			Maximum=round(mxx,2))			
		names(tmpp)[2]<- "Mean (SD)"
		out<- cbind(tmp,tmpp[,-1])
		return(out)
		}
	if(n==3)
		{# TABLE OF MODEL SELECTION 
		## FOSTER
		tmp<-as.data.frame(t(sapply(1:length(out_foster), function(x) summary(out_foster[[x]])$AICtab)))
		tmp$model<- sapply(1:length(out_foster), function(x){
			tmp<- paste(as.character(summary(out_foster[[x]])$call$formula),collapse="~")
			return(substr(tmp,2,nchar(tmp)))})
		tmp$model_indx<- c(1:length(out_foster))
		tmp$k<- nrow(dat)-tmp$df.resid
		tmp$AICc<- tmp$AIC+((2*tmp$k*(tmp$k+1))/(nrow(dat)-tmp$k-1))
		tmp$dAICc<- tmp$AICc-min(tmp$AICc)
		tmp<- tmp[order(tmp$dAICc, decreasing=FALSE),]
		tmp$lik<- exp(-0.5*tmp$dAICc)
		tmp$w<- tmp$lik/sum(tmp$lik)
		tmp$cum_w<- cumsum(tmp$w)
		tmp$location<-"Foster Dam"
		out<- tmp
		
		## DEXTER
		tmp<-as.data.frame(t(sapply(1:length(out_dexter), function(x) summary(out_dexter[[x]])$AICtab)))
		tmp$model<- sapply(1:length(out_dexter), function(x){
			tmp<- paste(as.character(summary(out_dexter[[x]])$call$formula),collapse="~")
			return(substr(tmp,2,nchar(tmp)))})
		tmp$model_indx<- c(1:length(out_dexter))
		tmp$k<- nrow(dat)-tmp$df.resid
		tmp$AICc<- tmp$AIC+((2*tmp$k*(tmp$k+1))/(nrow(dat)-tmp$k-1))
		tmp$dAICc<- tmp$AICc-min(tmp$AICc)
		tmp<- tmp[order(tmp$dAICc, decreasing=FALSE),]
		tmp$lik<- exp(-0.5*tmp$dAICc)
		tmp$w<- tmp$lik/sum(tmp$lik)
		tmp$cum_w<- cumsum(tmp$w)
		tmp$location<-"Dexter Dam"
		out<-rbind(out,tmp)
		xx<- strsplit(out$model,"~")		
		out$pred<- sapply(1:nrow(out), function(x) unlist(strsplit(xx[[x]][3],"[ +]"))[1])
		out<- merge(out,prds,by="pred")
		out<- out[order(out$location, out$dAICc),]
		indx<- match(c("model_indx","predictor","location","k","AICc","dAICc","lik","w","cum_w"),names(out))
		out<- out[,indx]
		return(out)
		}
	if(n==4)
	{
	# FIXED AND RANDOM EFFECTS FOR CONFIDENCE MODEL SETS
	# FOR BEST MODEL
	print("This will take a few minutes to run and profile the CIs")
	ms<- tables(3)
	confModSet<- subset(ms[ms$location=="Foster Dam",],(w>0.95 | cum_w<=0.95))
	out<- data.frame()
	for(i in 1:nrow(confModSet))
		{
		mod<- confModSet$model_indx[i]
		parameterEstimates<- data.frame(model=mod,pred=confModSet$predictor[i],
			Parameter=c(row.names(summary(out_foster[[mod]])$coefficients),".sig01"),
			Estimate=c(summary(out_foster[[mod]])$coefficients[,1],
				sqrt(as.vector(summary(out_foster[[mod]])$varcor[[1]]))))
				
		ci<- as.data.frame(confint(out_foster[[mod]]))
		ci$Parameter<- row.names(ci)
		parameterEstimates<- merge(parameterEstimates,ci,by="Parameter",
			all.x=TRUE)
		parameterEstimates$location<- "Foster Dam"
		out<- rbind(out, parameterEstimates)			
		}

		
	# DEXTER DAM	
	confModSet<- subset(ms[ms$location=="Dexter Dam",],(w>0.95 | cum_w<=0.95))
	for(i in 1:nrow(confModSet))
		{
		mod<- confModSet$model_indx[i]
		parameterEstimates<- data.frame(model=mod,pred=confModSet$predictor[i],
			Parameter=c(row.names(summary(out_dexter[[mod]])$coefficients),".sig01"),
			Estimate=c(summary(out_dexter[[mod]])$coefficients[,1],
				sqrt(as.vector(summary(out_dexter[[mod]])$varcor[[1]]))))
				
		ci<- as.data.frame(confint(out_dexter[[mod]]))
		ci$Parameter<- row.names(ci)
		parameterEstimates<- merge(parameterEstimates,ci,by="Parameter",
			all.x=TRUE)
		parameterEstimates$location<- "Dexter Dam"
		out<- rbind(out, parameterEstimates)	
		}
	return(out)
	}
	if(n==5)
		{# OPTIMAL DECISIONS FOR FOSTER
		minPerFish<- 4.6#Foster median nFish/loading time
		opt_dat<- expand.grid(n= seq(10,350,by=25),
			fish_per_vol=c(seq(0.1,0.9,by=0.1), seq(1,58,by=1)),
			truckVolume=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),
			haulingTime=c(15,30,45,60,75,90,105,120),
			loadingTime=c(15,30,45,60,75,90,105,120))
		# EXPECTED LOADING TIME
		opt_dat$fishPerHaul<- round(opt_dat$truckVolume*opt_dat$fish_per_vol,0)
		opt_dat$n_trips<- ceiling(opt_dat$n/opt_dat$fishPerHaul)
		opt_dat$loadingTime<- ifelse(opt_dat$n_trips==1,opt_dat$n*minPerFish, opt_dat$fishPerHaul*minPerFish)
		opt_dat$tot_time<- scale(opt_dat$loadingTime+opt_dat$haulingTime ,center=160.16, scale=66.99)
		opt_dat$fish_per_vol<-opt_dat$fishPerHaul
		
		# CONFIDENCE MODEL SET.
		ms<- tables(3)
		confModSet<- subset(ms[ms$location=="Foster Dam",],(w>0.95 | cum_w<=0.95))	
		confModSet$w<-confModSet$w/sum(confModSet$w)
		opt_dat$y<- predict(out_foster[[confModSet$model_indx[1]]],
			opt_dat,re.form=NA)*confModSet$w[1] 

		opt_dat$p<- plogis(opt_dat$y)
		opt_dat$nn<- ifelse(opt_dat$n_trips==1, opt_dat$n,opt_dat$fishPerHaul)
		opt_dat$risk<- 1 - pbinom(0,opt_dat$nn,prob=opt_dat$p,lower.tail=TRUE)
		# DETERMINE HOW LONG PROCESS WILL TAKE IN HOURS
		opt_dat$dailyTime<- ((opt_dat$loadingTime+opt_dat$haulingTime*2)*opt_dat$n_trips)/60
		opt_dat<-subset(opt_dat, opt_dat$dailyTime<=10)
		opt_dat$risk_u<- (opt_dat$risk-max(opt_dat$risk))/(min(opt_dat$risk)-max(opt_dat$risk))
		opt_dat$time_u<- (opt_dat$dailyTime-max(opt_dat$dailyTime))/(min(opt_dat$dailyTime)-max(opt_dat$dailyTime))
		opt_dat$truckVolume_gal<- round(opt_dat$truckVolume*264.172,0)
		opt_dat$U<- 0.5*opt_dat$risk_u+0.5*opt_dat$time_u

		xxx<-dcast(opt_dat,n+truckVolume_gal~"U",max,value.var="U")
		xxx$dec<-NA
		for(i in 1:nrow(xxx))
			{
			x<- opt_dat[opt_dat$n==xxx$n[i] & 
				opt_dat$truckVolume_gal==xxx$truckVolume_gal[i] &
				opt_dat$U == xxx$U[i],] 
			if(nrow(x)==1){x<-x}
			if(nrow(x)>1){x<-x[which.max(x$density),]}
			xxx$dec[i]<-paste(c(x$fishPerHaul, x$n_trips, ceiling(x$loadingTime)),collapse="|")
			}
		yyy<- dcast(xxx,n~truckVolume_gal, value.var="dec")
		return(yyy)
		}
	if(n==6)
		{# DEXTER DAM
		minPerFish<- 0.25#dexter
		opt_dat<- expand.grid(n= seq(10,350,by=25),
			fish_per_vol=c(seq(0.1,0.9,by=0.1), seq(1,58,by=1)),
			truckVolume=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),
			haulingTime=c(15,30,45,60,75,90,105,120))
		# EXPECTED LOADING TIME
		opt_dat$fishPerHaul<- round(opt_dat$truckVolume*opt_dat$fish_per_vol,0)
		opt_dat$n_trips<- ceiling(opt_dat$n/opt_dat$fishPerHaul)
		opt_dat$loadingTime<- ifelse(opt_dat$n_trips==1,opt_dat$n*minPerFish, opt_dat$fishPerHaul*minPerFish)
		# CONFIDENCE MODEL SET.
		ms<- tables(3)
		confModSet<- subset(ms[ms$location=="Dexter Dam",],(w>0.95 | cum_w<=0.95))	
		confModSet$w<-confModSet$w/sum(confModSet$w)
		opt_dat$y<- predict(out_dexter[[confModSet$model_indx[1]]],opt_dat,re.form=NA)*confModSet$w[1] 
		opt_dat$p<- plogis(opt_dat$y)
		opt_dat$nn<- ifelse(opt_dat$n_trips==1, opt_dat$n,opt_dat$fishPerHaul)
		opt_dat$risk<- 1 - pbinom(0,opt_dat$nn,prob=opt_dat$p,lower.tail=TRUE)
		# DETERMINE HOW LONG PROCESS WILL TAKE IN HOURS
		opt_dat$dailyTime<- ((opt_dat$loadingTime+opt_dat$haulingTime*2)*opt_dat$n_trips)/60
		opt_dat<-subset(opt_dat, opt_dat$dailyTime<=10)
		opt_dat$risk_u<- (opt_dat$risk-max(opt_dat$risk))/(min(opt_dat$risk)-max(opt_dat$risk))
		opt_dat$time_u<- (opt_dat$dailyTime-max(opt_dat$dailyTime))/(min(opt_dat$dailyTime)-max(opt_dat$dailyTime))
		opt_dat$truckVolume_gal<- round(opt_dat$truckVolume*264.172,0)
		opt_dat$U<- 0.5*opt_dat$risk_u+0.5*opt_dat$time_u

		xxx<-dcast(opt_dat,n+truckVolume_gal~"U",max,value.var="U")
		xxx$dec<-NA
		for(i in 1:nrow(xxx))
			{
			x<- opt_dat[opt_dat$n==xxx$n[i] & 
				opt_dat$truckVolume_gal==xxx$truckVolume_gal[i] &
				opt_dat$U == xxx$U[i],] 
			if(nrow(x)==1){x<-x}
			if(nrow(x)>1){x<-x[which.max(x$fish_per_vol),]}
			xxx$dec[i]<-paste(c(x$fishPerHaul, x$n_trips, ceiling(x$loadingTime)),collapse="|")
			}
		yyy<- dcast(xxx,n~truckVolume_gal, value.var="dec")
		return(yyy)
		}
		if(n==99)
		{# SUMMARY OF NUMBERS OF FISH OUPLANTED 
		# DAILY SUMMARY
		daily<- ddply(trans, .(location,year,doy),summarize,
			n=sum(nFish))
		overall<- ddply(daily, .(location),summarize,
			mean_n=mean(n),
			min_n=min(n),
			max_n=max(n))
		overall$type<- "Number"
		# ADD DENSITIES TO THE SUMMARY
		overall2<- ddply(trans, .(location),summarize,
			mean_n=mean(na.omit(fish_per_vol)),
			min_n=min(na.omit(fish_per_vol)),
			max_n=max(na.omit(fish_per_vol)))
		overall2$type<- "Density"	
		overall<- rbind(overall,overall2)
		# ADD TOTAL TIME
		overall2<- ddply(trans, .(location),summarize,
			mean_n=mean(na.omit(loadingTime)),
			min_n=min(na.omit(loadingTime)),
			max_n=max(na.omit(loadingTime)))
		overall2$type<- "loadingTime"	
		overall<- rbind(overall,overall2)	
		}
	if(n=="analysis data")
		{# THIS CODE STANDARIZES PREDICTION TO MEAN 0 AND SD=1
		out<- trans
		out<- out[out$waterbody!=-99,]		
		out$waterbody<- factor(out$waterbody)
		## create two variables to handle overdispersion
		out$site_yr = as.factor(paste(out$year,out$location,sep = "_"))
		out<- out[!is.na(out$mort[,1]),]
		out$samp = as.factor(c(1:nrow(out)))
		indx<- match(prds[,1],names(dat_unstd))# columns to standardize
		
		# get means and sdd to standarize with same as tbl 2
		# FOSTER
		mn<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,mean,na.rm=TRUE)
		sdd<- apply(dat_unstd[dat_unstd$location=="Foster Dam",indx],2,sd,na.rm=TRUE)
		fos<- as.matrix(cbind(indx,mn,sdd))
		# DEXTER			
		mn<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,mean,na.rm=TRUE)
		sdd<- apply(dat_unstd[dat_unstd$location=="Dexter Dam",indx],2,sd,na.rm=TRUE)
		dex<- as.matrix(cbind(indx,mn,sdd))
		
		
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
		return(out)
		}
}
