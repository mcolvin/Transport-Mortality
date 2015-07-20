
# Error structure
# best with Wi 0.8948 (1|site_yr)+(1|samp)+(1|waterbody)
# second best Wi 0.0468 (1|samp)+(1|driver)+(1|waterbody)

### For HL GOF test below
p.mort<-test$mort[,1]/rowSums(test$mort)
aggregate(p.mort,by =test[c("year","location")], summary)

##### Best model
outz<-glmer(mort~(1|samp)+ (1|waterbody) +(1|site_yr) , 
	data = test,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))


outz<-glmer(mort~(1|samp)+ (1|waterbody) +(1|site_yr) + trip.no+ tot.time + doy + I(doy^2) + DD50, 
	data = test,
	family=binomial,
	control=glmerControl(optimizer="bobyqa"))
summary(outz)

qqnorm(resid(outz))
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


