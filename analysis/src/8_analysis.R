	
	# AMONG PREDICTOR CORRELATIONS
	## LOOK AT CORRELATIONS PRIOR TO ANALYSIS AND RETAIN ANYTHING LESS THAN 0.6
	correlations<- cor(dat[,c(5:22)])
	correlations[correlations<0.6|correlations==1]<-NA
	
	
	# RANDOM EFFECS MODEL SELECTION
	grr<- tables(3,model_fits=out_re)
	grr
	summary(out_re[[4]])
	
	
	## FULL MODEL SELECTION
	grr<- tables(3,model_fits=out_all)
	#grr<- tables(1,model_fits=out_dexter)
	summary(out_all[[5]])$coefficients[,1]
	summary(out_all[[1]])$coefficients[,1]
	summary(out_all[[2]])$coefficients[,1]

	
	
	write.csv(as.data.table(grr),"./output/mod-sel.csv")
	## MODEL SELECTION TABLE 
	###  FOR ALL DATA
	grr<- tables(1,model_fits=out_all)
	
	
	
	# THIS IS THE FINAL MODEL
	fin<-glmer(mort ~ fish_per_vol+tot_time+doy + I(doy^2) + trap_total+ (1 | samp), 
		data = dat,
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
		
		
		
		
		
	# PREDICTION 
	
	
	re<-as.vector(unlist(ranef(fin)))
		hist(re)
	dat_unstd$re<- re
	plot(re~trip_no,dat_unstd)

		
	confint(fin)
	summary(fin)
	dat$p_hat<- plogis(predict(fin, dat,re.form=~0))
	dat$p_xx<- fitted(fin)
	dat$p<- dat$mort[,1]/dat$mort[,2]
	plot(p~p_hat,dat)
	plot(p~p_xx,dat)
	hist(ranef(fin))
	
	
	
	

### For HL GOF test below
p.mort<-test$mort[,1]/rowSums(test$mort)
aggregate(p.mort,by =test[c("year","location")], summary)
for(gg in 3:30){
	hosmerlem <-function (y, yhat, g) 
		{
		cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)), include.lowest = T)
        obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
        expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
        chisq <- sum((obs - expect)^2/expect)
        P <- 1 - pchisq(chisq, g - 2)
        c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
		}
print(hosmerlem(test$mort[,1]/rowSums(test$mort), fitted(outz),gg))
}


	
	
	
	### PREDICTIONS	
	#### UNCONTROLABLE: doy AND Q_01
	#### CONTROLLABLE:  fish_per_vl + tot_time
	dat_unstd$nfish<- round(dat_unstd$fish_per_vol*dat_unstd$truckVolume,0)
	density<- ddply(dat_unstd, .(location), summarize,
		mn_dens= mean(na.omit(fish_per_vol)),
		min_dens= min(na.omit(fish_per_vol)),
		max_dens= max(na.omit(fish_per_vol)),
		mn_doy= mean(na.omit(doy)),
		min_doy= min(na.omit(doy)),
		max_doy= max(na.omit(doy)),
		mn_fish= mean(na.omit(nfish)),
		min_fish= min(na.omit(nfish)),
		max_fish= max(na.omit(nfish))	
		
		)		
	
	pred_dat<- expand.grid(fish_per_vol=seq(-3,3,by=0.25),
		tot_time=seq(-3,3,by=0.25),
		doy=seq(-3,3,by=0.25),
		n=c(10,25,50,75,100,200,300))	
	pred_dat$p_hat<- plogis(predict(fin, pred_dat, re.form=NA))
	pred_dat$risk<-apply(pred_dat,1, mort_risk, nn="n" , p="p_hat")
	# optimal loading rows: DOY, truck volume, 
	
	# what about relative risk as risk value/value at lowest density and time... for each doy...
	# WHAT LOADING TIME GIVEN DENSITY
	
	
	
	# FIGURE OUT WHAT LOADING TIME IS WITHIN RISK LEVE
	xx<-ddply(pred_dat, .(doy,n),summarize,
		val= max(tot_time))
	
	dcast(xx, doy~n, value.var="val")# WORKS, 3 FOR MOST, UNTIL MIDSEASON FOR AVERAGE DENSITIES

	
	
	
	
	
	
	
	
	
	





	
	###  FOR FOSTER DATA
	grr<- tables(3,model_fits=out_foster)	
	
	
	# FOR DEXTER	
	grr<- tables(3,model_fits=out_dexter)
	grr$index<- rownames(grr)
	pred_dat<- expand.grid(fish_per_vol=seq(-2,3,0.1))
	

	
	out_dexter[[5]]

	fin<-glmer(mort ~ fish_per_vol,# + (1 | samp), 
		data = dat_dexter,
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
	
	re<-as.vector(unlist(ranef(fin)))
		hist(re)
	dat_dexter$re<- re
	plot(re~trip_no,dat_dexter)
	fin<- glm(mort~fish_per_vol, dat_dexter, family="binomial")
	dat_dexter$p_hat<- plogis(predict(fin, dat_dexter,re.form=~0))
	dat_dexter$fit<- fitted(fin)
	dat_dexter$p<- dat_dexter$mort[,1]/dat_dexter$mort[,2]
	plot(p~p_hat,dat_dexter)
	plot(p~fit,dat_dexter)
	hist(ranef(fin))
	
	
	x<- rbinom(100,40,0.4)
	x2<- rep(40,100)-x
	fit<- glm(cbind(x,x2)~1, family="binomial")
	
	
	
	vars<- c("tot_time","doy","trip_no","dd_50",
	"fish_per_vol","waterTempCollSite","cloudcover",
	"maxT_C","dd_01","Q_50","doy50","run_size","delta_temp","Q_01",
	"day_bet")
	
	test<- matrix(0,nrow=length(vars),ncol=2)
	for(i in 1:length(vars))
		{
		indx<-match(vars[i],names(dat_dexter))
		test[i,]<- c(min(dat_dexter[,indx]),max(dat_dexter[,indx]))
		}
	
	n<- 500000
	pred_dat<-matrix(0,n,length(vars))
	for(i in 1:length(vars))
		{
		pred_dat[,i]<-runif(n,test[i,1],test[i,2])
		}
	
	pred_dat<- as.data.frame(pred_dat)
	names(pred_dat)<- vars
	
	wtd_preds<-matrix(0,n,nrow(grr)) 
	for(i in 1:nrow(grr))
		{
		tmp<-predict(out_dexter[[5]],pred_dat,re.form=NA)+rnorm(n,0,as.data.frame(summary(out_dexter[[5]])$varcor)$sdcor[1])
		wtd_preds[,i]<-tmp*grr$w[i]
		}
	pred_dat$p_hat<- plogis(apply(wtd_preds,1,sum))
	
	bins<- as.data.frame(matrix(0,n,length(vars)))
		names(bins)<- vars
	for(i in 1:length(vars))
		{
		xx<- seq(test[i,1],test[i,2],length=10)
		bins[,i]<- cut(pred_dat[,i],xx,labels=paste(round(xx[-10],3),round(xx[-1],3),sep=":"))
		}

	bins$p_hat<- plogis(apply(wtd_preds,1,sum))

	pp<- dcast(bins, doy~fish_per_vol,value.var="p_hat",mean)
	apply(pp[,-1],1,which.min)
	
	

	
	
	# MODEL SPECIFICATION AND FIT
	dat_dexter$p<-dat_dexter$mort[,1]/dat_dexter$mort[,2]
	dat_dexter$p_hat<- fitted(out_dexter[[5]])
	plot(p~p_hat, dat_dexter,xlim=c(0,1),ylim=c(0,1))
	
	
	dat_dexter<- dat_dexter[order(dat_dexter$year, dat_dexter$doy),]
	
	xyplot(p~fish_per_vol, dat_dexter,groups=year,type='p')
	plot(p~loadingTime,dat_foster)
	
	
	
	
	dat_foster<-subset(dat_unstd, location=="Foster Dam")
	loadingTime<- 
	summary(dat_foster$tot_time)
	summary(dat_foster$doy)
	summary(dat_foster$waterTempCollSite)
	
	# GET PREDICTIONS
	test<- list()
	test$m1<- glmer(mort~tot_time+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))

	# PREDICTIONS FROM MODEL STUFF STORED IN A LIST...
y<-predict(test$m1,dat, re.form=NA,type="response")	
y<- cbind(y,predict(test$m1,dat, re.form=~(1|samp)+ (1|waterbody) +(1|site_yr),type="response"	))
y<- cbind(y, fitted(test$m1))	
	
	
	
	out$fit1
	#total time
	x<-runif(10000,-1,1)
	y<- -7.2388+rnorm(10000,0,0.9696)+rnorm(10000,0,1.7799) + 0.7572*x
	p<- plogis(y)
	hist(p)
	
	
	out <- cut(x, quantile(x, seq(0, 1, len = 4)), labels=c("low","med","hi"),include.lowest = TRUE) 

	
	summary(glmer(mort~tot_time+
		(1|year)+(1|samp), 
		data = dat_foster,
		family=binomial,
		control=glmerControl(optimizer="bobyqa")))
	
	
	
	
	N<- 1000
	grr<- data.frame(tot_time=runif(N, -1,1),
		# year effect
		year=sample(c(2006:2014), N,replace=TRUE),
		# overdispersion
		samp=c(1:N))
	
	year_effect<- data.frame(year=c(2006:2014),er=rnorm(9,0,1))
	grr<- merge(grr, year_effect, by="year")
	grr$err<- rnorm(N,0,1.7799)	# OVERDISPERSION

	grr$b0<- -7.2388+grr$er
	grr$y<-  (grr$b0+0.7572*grr$tot_time)+grr$err
	grr$p<- plogis(grr$y)
	grr$N<- rpois(N,100)
	grr$mort<- cbind(rbinom(N,grr$N, grr$p),grr$N)
	summary(glmer(mort~tot_time+
		(1|samp)+(1|year), 
		data = grr,
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))) 
		
		
	
	
	dat_foster$year<-as.factor(dat_foster$year)
	
	fitx16<-summary(glmer(mort~1+
	(1|samp), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))  
	
	fitx17<-summary(glmer(mort~1+
	(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))  
	fitx18<-summary(glmer(mort~1+
	(1|year)+(1|samp), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))  
	
	fitx16
	fitx17
	fitx18





tables(1) # AIC model selection





outz<-glmer(mort~(1|samp)+ (1|waterbody) +(1|site_yr) + 
	fish.per.vol, 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))
AICc(outz)
	
	fuck<- AIC(outz)
outz2<-glmer(mort~(1|samp)+ (1|waterbody) +(1|site_yr) + 
	tot.time, 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))	
	fuck<- c(fuck,AIC(outz2))
	
	summary(outz)
	summary(outz2)


	
	
##### Best model
outz<-glmer(mort~(1|samp)+ (1|waterbody) +(1|site_yr) + 
	trip.no + tot.time + doy + I(doy^2) + DD50, 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))
summary(outz)
	confint(outz)
# EVALUATE RANDOM EFFECTS
re<-ranef(outz)
hist(re$site_yr[[1]])
hist(re$waterbody[[1]])
hist(re$samp[[1]])

pred_dat<-expand.grid(samp=levels(test$samp),
	waterbody=levels(test$waterbody),
	site_yr=levels(test$site_yr), 
	trip.no= quantile(test$trip.no,probs=c(0.5)),
	tot.time=seq(min(test$trip.no),max(test$trip.no),0.5),
	doy= quantile(test$doy,probs=c(0.5)),
	DD50 = quantile(test$DD50,probs=c(0.5)))

# EMPERICAL BAYES PREDICTIONS (AT LEAST I THINK SO...)
pred_dat$pred<- predict(outz,pred_dat, re.form=NULL,
	type="response")

pred_dat<-expand.grid(#samp=levels(test$samp),
	waterbody=levels(test$waterbody),
	#site_yr=levels(test$site_yr), 
	trip.no= quantile(test$trip.no,probs=c(0.5)),
	tot.time=seq(min(test$trip.no),max(test$trip.no),0.5),
	doy= quantile(test$doy,probs=c(0.5)),
	DD50 = quantile(test$DD50,probs=c(0.5)))
pred_dat$pred<- predict(outz,pred_dat, re.form=~(1|waterbody),
	type="response")	
	
	
library(lattice)
xyplot(pred~tot.time,pred_dat, group=waterbody,type="l")
	

summary(outz)
qqnorm(resid(outz))



