correlations<- cor(dat[,c(5:22)])
correlations[correlations<0.6|correlations==1]<-NA

out<-list()
out$fit1<-summary(glmer(mort~1+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit2<-summary(glmer(mort~1+
	(1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))

out$fit3<-summary(glmer(mort~1+
	(1|samp)+(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit4<-summary(glmer(mort~1+
	(1|samp)+ (1|waterbody), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit5<-summary(glmer(mort~1+
	(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit6<-summary(glmer(mort~1+
	(1|waterbody), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit7<-summary(glmer(mort~1+
	(1|samp), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
# OVERWHELMING SUPPORT FOR ALL 3
# FROM JPs ANALYSIS
# Error structure
# best with Wi 0.8948 (1|site_yr)+(1|samp)+(1|waterbody)
# second best Wi 0.0468 (1|samp)+(1|driver)+(1|waterbody)


out<-list()
out$fit1<-summary(glmer(mort~tot_time+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit2<-summary(glmer(mort~doy+I(doy^2)+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit3<-summary(glmer(mort~trip_no+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit4<-summary(glmer(mort~dd_50+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit5<-summary(glmer(mort~fish_per_vol+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))	
out$fit6<-summary(glmer(mort~1+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))	
out$fit7<-summary(glmer(mort~waterTempCollSite+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit8<-summary(glmer(mort~cloudcover+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit9<-summary(glmer(mort~maxT_C+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
	
# NOT INCLUDED IN ORIGINAL ANALYSIS, CORRELATED WITH SOMETHING?
#out$fit10<-summary(glmer(mort~av.no.trp.bef+
#	(1|samp)+ (1|waterbody) +(1|site_yr), 
#	data = dat,
#	family=binomial,
#	control=glmerControl(optimizer="bobyqa")))

	
out$fit11<-summary(glmer(mort~dd_01+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))		
out$fit12<-summary(glmer(mort~Q_50+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))	
out$fit13<-summary(glmer(mort~doy50+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit14<-summary(glmer(mort~run_size+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))	)
out$fit15<-summary(glmer(mort~delta_temp+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit16<-summary(glmer(mort~Q_01+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
 out$fit16<-summary(glmer(mort~day_bet+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))  

dat_foster<-subset(dat, location=="Foster Dam")
out<-list()
out$fit1<-summary(glmer(mort~tot_time+
	(1|year)+(1|samp), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit2<-summary(glmer(mort~doy+I(doy^2)+
	(1|year)+(1|samp), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit3<-summary(glmer(mort~trip_no+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit4<-summary(glmer(mort~dd_50+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit5<-summary(glmer(mort~fish_per_vol+
	(1|samp)+(1|year),  
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))	
out$fit6<-summary(glmer(mort~1+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))	
out$fit7<-summary(glmer(mort~waterTempCollSite+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit8<-summary(glmer(mort~cloudcover+
	(1|samp)+(1|year),  
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit9<-summary(glmer(mort~maxT_C+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
	
# NOT INCLUDED IN ORIGINAL ANALYSIS, CORRELATED WITH SOMETHING?
#out$fit10<-summary(glmer(mort~av.no.trp.bef+
#	(1|samp)+ (1|waterbody) +(1|site_yr), 
#	data = dat,
#	family=binomial,
#	control=glmerControl(optimizer="bobyqa")))

	
out$fit11<-summary(glmer(mort~dd_01+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))		
out$fit12<-summary(glmer(mort~Q_50+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))	
out$fit13<-summary(glmer(mort~doy50+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit14<-summary(glmer(mort~run_size+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))	)
out$fit15<-summary(glmer(mort~delta_temp+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit16<-summary(glmer(mort~Q_01+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
 out$fit16<-summary(glmer(mort~day_bet+
	(1|samp)+(1|year), 
	data = dat_foster,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))  
	
	grr<- tables(1)
	grr$cum_w<- cumsum(grr$w)
	dat_foster$p<-dat_foster$mort[,1]/dat_foster$mort[,2]
	dat_foster<- dat_foster[order(dat_foster$year, dat_foster$doy),]
	xyplot(p~doy, dat_foster,groups=year,type='b')
	plot(p~loadingTime,dat_foster)
	
	dat_foster<-subset(dat_unstd, location=="Foster Dam")
	loadingTime<- 
	summary(dat_foster$tot_time)
	summary(dat_foster$doy)
	summary(dat_foster$waterTempCollSite)
	
	
	
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