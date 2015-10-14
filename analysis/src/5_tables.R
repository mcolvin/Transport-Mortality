tables<- function(n){
	if(n==1)
		{# TABLE OF MODEL SELECTION 
		tmp<- as.data.frame(t(sapply(1:length(out), function(x) out[[x]]$AICtab)))
		tmp$model<- unlist((sapply(1:length(out), function(x) out[[x]]$call$formula)))
		tmp$k<- nrow(dat)-tmp$df.resid
		tmp$AICc<- tmp$AIC+((2*tmp$k*(tmp$k+1))/(nrow(dat)-tmp$k-1))
		tmp$dAICc<- tmp$AICc-min(tmp$AICc)
		tmp<- tmp[order(tmp$dAICc, decreasing=FALSE),]
		tmp$lik<- exp(-0.5*tmp$dAICc)
		tmp$w<- tmp$lik/sum(tmp$lik)
		return(tmp)
		}


if(n==111)
	{
	# TABLE OF MAXIMUM DENSITIES TRANSPORTED BY 
	# LOCATION 
	aggregate(fish_per_vol~location,test,max)
	}
if(n==2)
	{
	# TABLE OF MAXIMUM NUMBER TRANSPORTED BY 
	# LOCATION BY VOLUME (LINEAR REGRESSION WITH VOLUME? 
	plot(nFish~truckVolume,dat_unstd,subset=location=="Foster Dam")
	plot(nFish~truckVolume,dat_unstd,subset=location=="Dexter Dam")

	}
if(n==3)
	{
	# CALCUALTE SUMMARY STATISTICS FOR PREDICTORS
	nnam<-names(test)
prds<-c("doy", "trip.no", "waterTempCollSite", "truckVolume","loadingTime","haulingTime","tot.time", "nFish", "maxT.C", "cloudcover", "av.day.bet.trp", "av.no.trp.bef", "doy50", "run.size", 
"DD.first", "DD50",  "Q.first", "Q50", "delta.temp","fish.per.vol")


xxx<- NULL
for(z in 1:length(prds)){
  zz<- test[, which(nnam == prds[z])]
  xxx<-rbind(xxx,c(round(mean(zz,na.rm=TRUE),1),round(sd(zz,na.rm=TRUE),2),round(min(zz,na.rm=TRUE)),round(max(zz,na.rm=TRUE))))
parm.summary<-cbind(as.data.frame(prds),xxx)

  }

	}

}