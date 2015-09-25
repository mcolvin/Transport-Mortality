
out<-list()
out$fit1<-summary(glmer(mort~tot.time+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit2<-summary(glmer(mort~doy+I(doy^2)+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit3<-summary(glmer(mort~trip.no+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit4<-summary(glmer(mort~DD50+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit5<-summary(glmer(mort~fish.per.vol+
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
out$fit9<-summary(glmer(mort~maxT.C+
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

	
out$fit11<-summary(glmer(mort~DD.first+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))		
out$fit12<-summary(glmer(mort~Q50+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))	
out$fit13<-summary(glmer(mort~doy50+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit14<-summary(glmer(mort~run.size+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))	)
out$fit15<-summary(glmer(mort~delta.temp+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
out$fit16<-summary(glmer(mort~Q.first+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))
 out$fit16<-summary(glmer(mort~av.day.bet.trp+
	(1|samp)+ (1|waterbody) +(1|site_yr), 
	data = dat,
	family=binomial,
	control=glmerControl(optimizer="bobyqa")))  

   
