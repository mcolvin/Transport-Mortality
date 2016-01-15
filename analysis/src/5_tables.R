tables<- function(n,model_fits=NULL){
	if(n==2)
		{# TABLE OF MEANS, STD. DEVS, MINIMUMS, AND MAXIMUMS
		### now standardize predictors
		prds<-matrix(c( 
		# Trip and truck specific variables
			"loadingTime","Loading time (min)*", 
			"haulingTime","Hauling Time (min)",
			"tot_time","Total time handling (min)",
			"trip_no","Trip number for the day",
			"truckVolume","Truck volume (m3)",
			"nFish" ,"Number of fish transported",
			"fish_per_vol","Number of fish per truck volume (no./m3)",
			"delta_temp","Difference in temperature between collection facility and tank (C)",
		# Trap and loading conditions
			"doy","Day of the year transported",
			"trap_total","Number of fish in trap",# number in trap
			"waterTempCollSite","Water temperature at collection facility (C)",
			"maxT_C",	"Maximum daily air temperature (C)",	
			"day_bet","Average number of days since last trap tending",
			"cloudcover","Cloud cover index",
			# Run size, timing, and tributary conditions
			"run_size","Run size",	
			"doy50","Day of the year 50% of run passed Willamette Falls",
			"dd_01","Degree days from first fish (C)",
			"dd_50","Degree days 50% fish (C)",
			"Q_01", "Mean daily discharge first (m3/s)",
			"Q_50","Mean daily discharge 50% fish (m3/s)"),ncol=2, byrow=TRUE)

			
			
		indx<- match(prds[,1],names(dat_unstd))# columns to standardize
		mn<- apply(dat_unstd[,indx],2,mean,na.rm=TRUE)
		sdd<- apply(dat_unstd[,indx],2,sd,na.rm=TRUE)
		mnn<- apply(dat_unstd[,indx],2,min,na.rm=TRUE)
		mxx<- apply(dat_unstd[,indx],2,max,na.rm=TRUE)

		tmp<- data.frame(Predictor=prds[,2],
			V1=paste(round(mn,2)," (", round(sdd,2),")",sep=""),
			Minimum=round(mnn,2),
			Maximum=round(mxx,2))
		names(tmp)[2]<- "Mean (SD)"
	return(tmp)
	}
	
	if(n==3)
		{# TABLE OF MODEL SELECTION 
		tmp<-as.data.frame(t(sapply(1:length(model_fits), function(x) summary(model_fits[[x]])$AICtab)))
		tmp$model<- sapply(1:length(model_fits), function(x){
			tmp<- paste(as.character(summary(model_fits[[x]])$call$formula),collapse="~")
			return(substr(tmp,2,nchar(tmp)))})
		tmp$model_indx<- c(1:length(model_fits))
		tmp$k<- nrow(dat)-tmp$df.resid
		tmp$AICc<- tmp$AIC+((2*tmp$k*(tmp$k+1))/(nrow(dat)-tmp$k-1))
		tmp$dAICc<- tmp$AICc-min(tmp$AICc)
		tmp<- tmp[order(tmp$dAICc, decreasing=FALSE),]
		tmp$lik<- exp(-0.5*tmp$dAICc)
		tmp$w<- tmp$lik/sum(tmp$lik)
		tmp$cum_w<- cumsum(tmp$w)
		tmp<- as.data.table(tmp)
		return(tmp)
		}
	if(n==4)
	{
	# FIXED AND RANDOM EFFECTS
	# FOR BEST MODEL
	print("This will take a few minutes to run and profile the CIs")
	fin<-glmer(mort ~ fish_per_vol+tot_time+doy + I(doy^2)+trap_total + (1 | samp), 
		data = dat,
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
	parameterEstimates<- data.frame(type=c(rep("Fixed",6),"Random"),
		Parameter=c(row.names(summary(fin)$coefficients),".sig01"),
		Estimate=c(summary(fin)$coefficients[,1],
			sqrt(as.vector(summary(fin)$varcor[[1]]))))
	ci<- as.data.frame(confint(fin))
	ci$Parameter<- row.names(ci)
	parameterEstimates<- merge(parameterEstimates,ci,by="Parameter",
		all.x=TRUE)
	return(parameterEstimates)
	}
	if(n=="a")
		{ 
		# TABLE OF PREDICTED VALUES FOR THE SUITE OF VARIABLES INCLUDED IN FINAL MODEL
		fin<-glmer(mort ~ fish_per_vol+tot_time+doy + 
			I(doy^2)+ trap_total + (1 | samp), 
			data = dat,
			family=binomial,
			control=glmerControl(optimizer="bobyqa"))
		pdat<- expand.grid(fish_per_vol=c(-1,0,1),
			doy=seq(-1,1.5,0.025),
			trap_total=c(-1,0,1),
			tot_time=c(-1,0,1),
			volume=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607))
		
		# PREDICTION INTERVALS FOR PLOTS...	
		# http://www.r-bloggers.com/confidence-intervals-for-prediction-in-glmms/
		mm<- model.matrix(~ fish_per_vol+tot_time+doy + I(doy^2)+ trap_total,pdat)	
		pdat$y_hat<- mm%*%fixef(fin)# predict(fin, pdat, re.form=~0)				
		pvar1 <- diag(mm %*% tcrossprod(vcov(fin),mm))
		tvar1 <- pvar1+VarCorr(fin)$samp[1]  ## must be adapted for more complex models

		# OTHER BITS TO ADD TO PREDICTED DATASET
		pdat$y_hat_lo<- pdat$y_hat-1.96*(sqrt(tvar1))
		pdat$y_hat_hi<- pdat$y_hat+1.96*(sqrt(tvar1))	
					
		# OTHER BITS TO ADD TO PREDICTED DATASET
		pdat$p_hat<- plogis(pdat$y_hat)
		pdat$p_hat_lo<- plogis(pdat$y_hat-1.96*(sqrt(tvar1)))
		pdat$p_hat_hi<- plogis(pdat$y_hat+1.96*(sqrt(tvar1)))

		
		# PUT PREDICTORS BACK ON UNSTANDARDIZED SCALE
		pdat$nfish<-  round(((pdat$fish_per_vol * 8.42)+14.17)*pdat$volume,0)
		pdat$doy<- pdat$doy*31+201.99
		# CALCULATE THE RISK OF A SINGLE MORTALITYH
		pdat$risk<- 1 - pbinom(0,pdat$nfish,prob=pdat$p_hat,lower.tail=TRUE)
		return(pdat)
		}
	if(n==5)
		{
		# TABLE OF PREDICTIONS FOR FOSTER AND DEXTER RUN AS SEPARATE
		grr<- tables(3,model_fits=out_foster)	
		n<-40000	# do not set above 50K, it throws an erro in tcrossprod	
		y_hat<- y_lo<- y_hi<- matrix(0, n,nrow(grr))
		pdat<- data.frame(
			doy=runif(n,-2,2),
			trap_total=runif(n,-2,2),
			tot_time=runif(n,-2,2),# need to do sample xxx with reasonable values
			waterTempCollSite=runif(n,-2,2),
			trip_no=runif(n,-2,2),
			cloudcover=runif(n,-2,2),
			fish_per_vol=runif(n,-2,2),
			maxT_C=runif(n,-2,2),
			dd_01=runif(n,-2,2),
			Q_50=runif(n,-2,2),
			doy50=runif(n,-2,2),
			dd_50=runif(n,-2,2),
			run_size=runif(n,-2,2),
			delta_temp=runif(n,-2,2),
			Q_01=runif(n,-2,2),
			day_bet=runif(n,-2,2),				
			volume=sample(c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),n,replace=TRUE))

		for(i in 1:nrow(grr))
			{
			mod<- grr$model_indx[i]
			mm<- paste("~",paste(names(fixef(out_foster[[mod]]))[-1],collapse="+"))
			mm<- model.matrix(as.formula(mm),pdat)	
			y_hat[,i]<- mm%*%fixef(out_foster[[mod]])*grr$w[i]				
			pvar1 <- diag(mm %*% tcrossprod(vcov(out_foster[[mod]]),mm))
			tvar1 <- pvar1+VarCorr(out_foster[[mod]])$samp[1] 

			# OTHER BITS TO ADD TO PREDICTED DATASET
			y_lo[,i]<- y_hat[,i]-1.96*(sqrt(tvar1))*grr$w[i]
			y_hi[,i]<- y_hat[,i]+1.96*(sqrt(tvar1))*grr$w[i]	
			}
			
		pdat$y_hat<-apply(y_hat,1,sum)
		pdat$y_hi<- apply(y_hi,1,sum)
		pdat$y_lo<- apply(y_lo,1,sum)

		pdat$p_hat<-plogis(pdat$y_hat)
		pdat$p_hi<- plogis(pdat$y_hi)
		pdat$p_lo<- plogis(pdat$y_lo)
		}
		
	if(n==6)
		{
		# TABLE OF PREDICTIONS FOR FOSTER AND DEXTER RUN AS SEPARATE
		grr<- tables(3,model_fits=out_dexter)	
		n<-40000	# do not set above 50K, it throws an error in tcrossprod	
		y_hat<- y_lo<- y_hi<- matrix(0, n,nrow(grr))
		pdat<- data.frame(
			doy=runif(n,-2,2),
			trap_total=runif(n,-2,2),
			tot_time=runif(n,-2,2),# need to do sample xxx with reasonable values
			waterTempCollSite=runif(n,-2,2),
			trip_no=runif(n,-2,2),
			cloudcover=runif(n,-2,2),
			fish_per_vol=runif(n,-2,2),
			maxT_C=runif(n,-2,2),
			dd_01=runif(n,-2,2),
			Q_50=runif(n,-2,2),
			doy50=runif(n,-2,2),
			dd_50=runif(n,-2,2),
			run_size=runif(n,-2,2),
			delta_temp=runif(n,-2,2),
			Q_01=runif(n,-2,2),
			day_bet=runif(n,-2,2),				
			volume=sample(c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),n,replace=TRUE))

		for(i in 1:nrow(grr))
			{
			mod<- grr$model_indx[i]
			mm<- paste("~",paste(names(fixef(out_dexter[[mod]]))[-1],collapse="+"))
			mm<- model.matrix(as.formula(mm),pdat)	
			y_hat[,i]<- mm%*%fixef(out_dexter[[mod]])*grr$w[i]				
			pvar1 <- diag(mm %*% tcrossprod(vcov(out_dexter[[mod]]),mm))
			tvar1 <- pvar1+VarCorr(out_dexter[[mod]])$samp[1] 

			# OTHER BITS TO ADD TO PREDICTED DATASET
			y_lo[,i]<- y_hat[,i]-1.96*(sqrt(tvar1))*grr$w[i]
			y_hi[,i]<- y_hat[,i]+1.96*(sqrt(tvar1))*grr$w[i]	
			}
			
		pdat$y_hat<-apply(y_hat,1,sum)
		pdat$y_hi<- apply(y_hi,1,sum)
		pdat$y_lo<- apply(y_lo,1,sum)

		pdat$p_hat<-plogis(pdat$y_hat)
		pdat$p_hi<- plogis(pdat$y_hi)
		pdat$p_lo<- plogis(pdat$y_lo)
		}		
	
	if(n==7)
		{
		# TABLE GENERATING SIMULATED VALUES AS MORTALITIES TO DEMONSTRATE 
		# WHAT CAN BE LEARNED
				# TABLE OF PREDICTIONS FOR FOSTER AND DEXTER RUN AS SEPARATE
		grr<- tables(3,model_fits=out_dexter)	
		n<-10	# do not set above 50K, it throws an error in tcrossprod	
		
		pdat<- data.frame(
			doy=runif(n,-2,2),
			trap_total=runif(n,-2,2),
			tot_time=runif(n,-2,2),# need to do sample xxx with reasonable values
			waterTempCollSite=runif(n,-2,2),
			trip_no=runif(n,-2,2),
			cloudcover=runif(n,-2,2),
			fish_per_vol=runif(n,-2,2),
			maxT_C=runif(n,-2,2),
			dd_01=runif(n,-2,2),
			Q_50=runif(n,-2,2),
			doy50=runif(n,-2,2),
			dd_50=runif(n,-2,2),
			run_size=runif(n,-2,2),
			delta_temp=runif(n,-2,2),
			Q_01=runif(n,-2,2),
			day_bet=runif(n,-2,2),				
			volume=sample(c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),n,replace=TRUE))
		y_hat<- matrix(0, n,nrow(grr))
		for(i in 1:nrow(grr))
			{
			mod<- grr$model_indx[i]
			mm<- paste("~",paste(names(fixef(out_dexter[[mod]]))[-1],collapse="+"))
			mm<- model.matrix(as.formula(mm),pdat)	
			y_hat[,i]<- (mm%*%fixef(out_dexter[[mod]])+rnorm(n,0,sqrt(VarCorr(out_dexter[[mod]])$samp[[1]])))#*grr$w[i]				
			}
		pdat$y_hat<-y_hat %*% grr$w	
		pdat$p_hat<-plogis(pdat$y_hat)
		pdat$morts<- rbinom(n,100,pdat$p_hat)
		}
	trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)# make grey color with some transparency
	plot(morts~doy,pdat,pch=19,col=trans_grey)
	
	if(n==7)
	{
	# UTILITIY TABLE
		tmp<- tables('a')
		tmp$risk_score<- (pdat$risk-max(pdat$risk))/(0-max(pdat$risk))
		n_fish_in_trap<- c(50,100,200,300,400,500,600,700,800,900,1000)
		tmp$n_trips<- 500/(tmp$volume*tmp$fish_per_vol)
		fish_per_min<- 1
	}	
		
		
		
		plot(risk_score~p_hat,tmp)
		
		}

}