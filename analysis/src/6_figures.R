figures<- function(n){

if(n==1)
	{# study area
	spfiles()
	par(mar=c(5,6,1,1.5))
	plot(will_bndry,col="grey90",axes=TRUE,las=1,xlab="Longitude", ylab="",cex.lab=1.5)
		mtext(side=2, "Lattitude", line=4,cex=1.5)
	plot(full,add=TRUE,lwd=4)
	plot(alll,add=TRUE,col="black",lwd=4)
	plot(red, add=TRUE,col="black",lwd=4)
	plot(full[full$NAME=="Willamette River",],add=TRUE,col="black",lwd=4)
	plotDams<<- c("Dexter","Foster Reservoir","Cougar Reservoir","Big Cliff Dam","Fall Creek Res",
		"Trail Bridge Reg. Res.")
	plot(oreg_dams_ll[oreg_dams_ll$damname%in%plotDams,],add=TRUE,cex=1.5, col="black",pch=22,bg="white")
	plot(oreg_dams_ll[oreg_dams_ll$damname%in%"Willamette Falls",],add=TRUE,cex=1.5, col="black",pch=24,bg="white")
	text(-124, 43.8,label="N")	
	arrows(-124, 43.46,-124,43.75)	
	# INSET OREGON IN THE UPPER RIGHT
	par(new=TRUE,oma=c(24,18,0,0))
	plot(oreg_bndry_ll,axes=FALSE);box()
	plot(will_bndry,add=TRUE,axes=FALSE,col="grey90")
	}
if(n==2)
	{
	# FIGURE OF PREDICTED INTERVALS SUITE OF MODELS 
	
	## MODEL SELECTION TABLES

	grr_foster<- subset(tables(3),location=="Foster Dam")
	
	## CREATE DATASET TO PLOT FOR FOSTER
	pdat<- data.frame(x=seq(-3,5,0.01))
	ntop<-3
	y_hat<-y_lo<- y_hi<- matrix(0,nrow(pdat), ntop)
	for(i in 1:ntop)
		{
		mod<- grr_foster$model_indx[i]
		mm<- paste("~",paste(names(fixef(out_foster[[mod]]))[-1],collapse="+"))
		names(pdat)<- names(fixef(out_foster[[mod]]))[-1]		
		mm<- model.matrix(as.formula(mm),pdat)	
		y_hat[,i]<- mm%*%fixef(out_foster[[mod]])	
		pvar1 <- diag(mm %*% tcrossprod(as.matrix(vcov(out_foster[[mod]])),mm)) # prediction variance
		tvar1 <- pvar1+VarCorr(out_foster[[mod]])$samp[1] 
		y_lo[,i]<- y_hat[,i]-2*(sqrt(tvar1))
		y_hi[,i]<- y_hat[,i]+2*(sqrt(tvar1))
		}
	pdat$y_hat<-y_hat %*% (grr_foster$w[1:ntop]/sum(grr_foster$w[1:ntop]))
	pdat$y_hi<- y_hi %*% (grr_foster$w[1:ntop]/sum(grr_foster$w[1:ntop]))
	pdat$y_lo<- y_lo %*% (grr_foster$w[1:ntop]/sum(grr_foster$w[1:ntop]))	
	pdat$p_hat<-plogis(pdat$y_hat)
	pdat$p_hat<-plogis(pdat$y_hat)
	pdat$p_hi<- plogis(pdat$y_hi)
	pdat$p_lo<- plogis(pdat$y_lo)	
	
	par(mfrow=c(2,1),mar=c(4,3,0.25,0),oma=c(1,2,1,1))
	mn<-1.227848e+02
	sdd<- 6.839026e+01
	xlims=c(60,345)	
	x<-(pdat[,1]*sdd+mn)
	indx<- which(x>=xlims[1] & x<=xlims[2])
	plot(x[indx],plogis(y_hat[indx,1]),ylim=c(0,1),type='n',las=1,ylab="",
		xlab="Total time (minutes)") # total time
	panLab("a) Foster dam")
	polygon(c(x[indx],rev(x[indx])),c(plogis(y_hi[indx,1]),rev(plogis(y_lo[indx,1]))), col="lightgrey",border="lightgrey")
	points(x[indx],plogis(y_hat[indx,1]),type='l',lwd=3)

	# DEXTER
	grr_dexter<- subset(tables(3),location=="Dexter Dam")	
	pdat<- data.frame(X=seq(-4,4,0.01))
	ntop<-1
	y_hat<-y_lo<- y_hi<- matrix(0,nrow(pdat), ntop)
	for(i in 1:ntop)
		{
		mod<- grr_dexter$model_indx[i]
		mm<- paste("~",paste(names(fixef(out_dexter[[mod]]))[-1],collapse="+"))
		names(pdat)<- names(fixef(out_dexter[[mod]]))[-1]	
		mm<- model.matrix(as.formula(mm),pdat)	
		y_hat[,i]<- mm%*%fixef(out_dexter[[mod]])	
		pvar1 <- diag(mm %*% tcrossprod(as.matrix(vcov(out_dexter[[mod]])),mm))
		tvar1 <- pvar1+VarCorr(out_dexter[[mod]])$samp[1] 
		y_lo[,i]<- y_hat[,i]-2*(sqrt(tvar1))
		y_hi[,i]<- y_hat[,i]+2*(sqrt(tvar1))
		}
	mn<-17.01 
	sdd<- 7.34
	xlims=c(1.76,37.86)	
	x<-(pdat[,1]*sdd+mn)
	indx<- which(x>=xlims[1] & x<=xlims[2])	
	plot(x[indx],plogis(y_hat[indx,1]),ylim=c(0,1),type='n',las=1,ylab="",
		xlab="Number of fish per truck volume (no./m3)") 
	panLab("d) Dexter dam")
	axis(2,at=axTicks(2), labels=FALSE,las=1)
	polygon(c(x[indx],rev(x[indx])),c(plogis(y_hi[indx,1]),rev(plogis(y_lo[indx,1]))), col="lightgrey",border="lightgrey")
	points(x[indx],plogis(y_hat[indx,1]),type='l',lwd=3)
	mtext(side=2, "Estimated probability of mortality",outer=TRUE, line=0)
	
	}
if(n==3)
	{
	# OPTIMAL DECISIONS FOR FOSTER
	## POLICY PLOT
	minPerFish<- 4.6 #Foster median nFish/loading time
	opt_dat<- expand.grid(n= seq(10,150,by=1),
		density=c(seq(0.1,0.9,by=0.1), seq(1,58,by=1)),
		truckVolume=seq(1,11,by=0.5),#1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),
		haulingTime=c(15,30,45,60,75,90,105,120),
		Q_01=c(0) ,									# 4810.05 (656.22)
		dd_50=c(0))

	# EXPECTED LOADING TIME
	opt_dat$fishPerHaul<- round(opt_dat$truckVolume*opt_dat$density,0)
	opt_dat$n_trips<- ceiling(opt_dat$n/opt_dat$fishPerHaul)
	opt_dat$loadingTime<- ifelse(opt_dat$n_trips==1,
		opt_dat$n*minPerFish, 
		opt_dat$fishPerHaul*minPerFish)
	opt_dat$nn<- ifelse(opt_dat$n_trips==1, opt_dat$n,opt_dat$fishPerHaul)		

	opt_dat$loadingTime<- scale(opt_dat$loadingTime,center=1.227848e+02, scale=6.839026e+01)
	# CONFIDENCE MODEL SET.
	ms<- tables(3)
	confModSet<- subset(ms[ms$location=="Foster Dam",],(w>0.95 | cum_w<=0.95))	
	confModSet$w<-confModSet$w/sum(confModSet$w)
	opt_dat$y<- predict(out_foster[[confModSet$model_indx[1]]],opt_dat,re.form=NA)*confModSet$w[1]
	opt_dat$p<- plogis(opt_dat$y)

	
	# PROBALITY OF OBSERVING 1 OR MORE MORTALITIES
	opt_dat$risk<- 1 - pbinom(0,opt_dat$nn,prob=opt_dat$p,lower.tail=TRUE)
	opt_dat$risk<-ifelse(opt_dat$n_trips==Inf,1,opt_dat$risk)
	opt_dat$risk_u<- (opt_dat$risk-max(opt_dat$risk))/(min(opt_dat$risk)-max(opt_dat$risk))		
	
	# DETERMINE HOW LONG PROCESS WILL TAKE IN HOURS
	opt_dat$dailyTime<- ((opt_dat$loadingTime+opt_dat$haulingTime*2)*opt_dat$n_trips)/60
	opt_dat<-subset(opt_dat, opt_dat$dailyTime<=10 & n_trips!=Inf)
	opt_dat$time_u<- (opt_dat$dailyTime-max(opt_dat$dailyTime))/(min(opt_dat$dailyTime)-max(opt_dat$dailyTime))
	
	opt_dat$truckVolume_gal<- round(opt_dat$truckVolume*264.172,0)
	opt_dat$U<- 0.5*opt_dat$risk_u+0.5*opt_dat$time_u
	opt_dat$id<- c(1:nrow(opt_dat))
	
	xxx<-dcast(opt_dat,n+truckVolume_gal~"U",max,value.var="U")
	xxx$rid<- c(1:nrow(xxx))
	xxxx<-merge(xxx,opt_dat, by=c("n","truckVolume_gal","U"))
	xxxx<- xxxx[order(xxxx$rid,xxxx$density),]
	xxxx$id<- c(1:nrow(xxxx))
	out<- ddply(xxxx,.(rid,n,truckVolume),summarize,
		id=max(id))
	out<-merge(out,xxxx[,c("n_trips","haulingTime","id")],by="id",all.x=TRUE)
	out$truckVolume_gal<- round(out$truckVolume*264.172,0)
	yy<- dcast(out,n~truckVolume,value.var="n_trips")
	x<- as.numeric(names(yy)[-1])
	y<- yy$n
	z<-as.matrix(yy[,-1])
	cols<- rev(c(1:max(na.omit(z)))/(max(na.omit(z))+1))
	cols<-c(1:max(na.omit(z)))/(max(na.omit(z))+1)
	image.plot(x,y,t(z), col=grey(cols),las=1,
		xlab="Transport volume (cubic meters)",
		ylab="Number of fish to outplant",cex.lab=1.5)
	abline(v=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),col="white",lty=3)
	
	}
if(n==4)
	{
	# OPTIMAL DECISIONS FOR DEXTER
	## POLICY PLOT
		minPerFish<- 0.25#dexter
	opt_dat<- expand.grid(n= seq(10,400,by=1),
		density=seq(1,38,by=1),
		truckVolume=seq(1,11,by=0.5),#1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),
		haulingTime=c(15,30,45,60,75,90,105,120))
	opt_dat$fish_per_vol<- scale(opt_dat$density,center=17.01, scale=7.34)
	# EXPECTED LOADING TIME
	opt_dat$fishPerHaul<- round(opt_dat$truckVolume*opt_dat$density,0)
	opt_dat$n_trips<- ceiling(opt_dat$n/opt_dat$fishPerHaul)
	opt_dat$loadingTime<- ifelse(opt_dat$n_trips==1,opt_dat$n*minPerFish, opt_dat$fishPerHaul*minPerFish)
	opt_dat$tot_time<- scale(opt_dat$loadingTime+opt_dat$haulingTime ,center=160.16, scale=66.99)
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
	opt_dat$U<- 0.35*opt_dat$risk_u+0.65*opt_dat$time_u
	opt_dat$id<- c(1:nrow(opt_dat))
	
	xxx<-dcast(opt_dat,n+truckVolume_gal~"U",max,value.var="U")
	xxx$rid<- c(1:nrow(xxx))
	xxxx<-merge(xxx,opt_dat, by=c("n","truckVolume_gal","U"))
	xxxx<- xxxx[order(xxxx$rid,xxxx$fish_per_vol),]
	xxxx$id<- c(1:nrow(xxxx))
	out<- ddply(xxxx,.(rid,n,truckVolume),summarize,
		id=max(id))
	out<-merge(out,xxxx[,c("n_trips","id")],by="id",all.x=TRUE)
	out$truckVolume_gal<- round(out$truckVolume*264.172,0)
	yy<- dcast(out,n~truckVolume,value.var="n_trips")
	x<- as.numeric(names(yy)[-1])
	y<- yy$n
	z<-as.matrix(yy[,-1])
	cols<-c(1:max(na.omit(z)))/(max(na.omit(z))+1)
	image.plot(x,y,t(z), col=grey(cols),las=1,
		xlab="Transport volume (cubic meters)",
		ylab="Number of fish to outplant",cex.lab=1.5)

	abline(v=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),col="white",lty=3)
	}
		
	
}