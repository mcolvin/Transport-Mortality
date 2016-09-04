tables<- function(n,model_fits=NULL, modsel_foster=NULL, modsel_dexter=NULL){
	if(n==1)
		{
		tbl1$Predictor.type[duplicated(tbl1$Predictor.type)]<-NA
		return(tbl1)
		}
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
		tmp<-as.data.frame(t(sapply(1:length(out_foster), 
			function(x) summary(out_foster[[x]])$AICtab)))
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
		out<- merge(out,prds,by="pred",all=TRUE)
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
	confModSet<- subset(ms[ms$location=="Foster Dam",])
	
	confModSet<- confModSet[1:which(confModSet$predictor=="Intercept only"),]
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
	confModSet<- subset(ms[ms$location=="Dexter Dam",])
	confModSet<- confModSet[1:which(confModSet$predictor=="Intercept only"),]
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
		out<- optimal(mortWght=0.5,maxDens=maxDensity)
		
		xxx_fos<- out$opt_fos
		yy<-dcast(xxx_fos,n~truckVolume,value.var="n_trips")
		x<- as.numeric(names(yy)[-1])
		y<- yy$n
		z<-as.matrix(yy[,-1])
		cols<-rev(c(1:max(na.omit(z)))/(max(na.omit(z))+1))	
		image.plot(x,y,t(z), col=grey(cols),las=1,
			xlab="",
			ylab="",cex.lab=1.5,xaxt='n')
		axis(1, at=axTicks(1))
		abline(v=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),col="white",lty=3)
		mtext(side=2, "Number of fish to outplant",outer=TRUE,line=0.5,cex=1.3)
		mtext(side=1, "Transport volume (cubic meters)",outer=TRUE,line=0,cex=1.3)
		

		yy<-dcast(out$opt_dex,n~truckVolume,value.var="n_trips")
		x<- as.numeric(names(yy)[-1])
		y<- yy$n
		z<-as.matrix(yy[,-1])
		cols<-rev(c(1:max(na.omit(z)))/(max(na.omit(z))+1))	
		image.plot(x,y,t(z), col=grey(cols),las=1,
			xlab="",
			ylab="",cex.lab=1.5,xaxt='n')
		axis(1, at=axTicks(1))
		abline(v=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),col="white",lty=3)
		mtext(side=2, "Number of fish to outplant",outer=TRUE,line=0.5,cex=1.3)
		mtext(side=1, "Transport volume (cubic meters)",outer=TRUE,line=0,cex=1.3)	
		
		
		return(out)
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
	
}
