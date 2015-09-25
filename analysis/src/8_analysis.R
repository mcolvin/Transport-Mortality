





# Error structure
# best with Wi 0.8948 (1|site_yr)+(1|samp)+(1|waterbody)
# second best Wi 0.0468 (1|samp)+(1|driver)+(1|waterbody)


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




### For HL GOF test below
p.mort<-test$mort[,1]/rowSums(test$mort)
aggregate(p.mort,by =test[c("year","location")], summary)
for(gg in 3:30){
	hosmerlem <-
	function (y, yhat, g) 
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


