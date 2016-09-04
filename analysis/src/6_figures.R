figures<- function(n){
op <- par(no.readonly = TRUE)
if(n==1)
	{# study area
	spfiles()
	par(mar=c(5,6,1,1.5))
	plot(will_bndry,col="grey90",axes=FALSE,las=1,xlab="", ylab="",cex.lab=1.5)
		#mtext(side=2, "Lattitude", line=4,cex=1.5)
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
	# LABEL SOUTH SANTIAM
	text(-122.6703,44.45 , "South Santiam",pos=4,cex=0.8)
	text(-122.6703,44.35 , "River",pos=4,cex=0.8)
	# LABEL MF WILLAMTEE
	
	text(-123.0,43.93 , "M.F. Willamette",pos=4, 
		cex=0.8,srt=-45)
	text(-122.5,43.58 , "River",pos=4, 
		cex=0.8,srt=-25)		
	# LABEL WILLAMETTE FALLS
	text(-122.6179,45.35312, "Willamette \n Falls",pos=4, 
		cex=0.8,srt=-45)

	
	# INSET OREGON IN THE UPPER RIGHT
	par(new=TRUE,oma=c(24,18,0,0))
	plot(oreg_bndry_ll,axes=FALSE)
	plot(will_bndry,add=TRUE,axes=FALSE,col="grey90")
	}
if(n==2.1)
	{
	# FIGURE OF PREDICTED INTERVALS SUITE OF MODELS 
	## FOSTER DAM
	## MODEL SELECTION TABLES

	grr_foster<- subset(tables(3),location=="Foster Dam")
	
	## CREATE DATASET TO PLOT FOR FOSTER
	pdat<- data.frame(x=seq(-3,5,0.01))
	ntop<-1:(which(grr_foster$predictor=="Intercept only")-1)
	y_hat<-y_lo<- y_hi<- matrix(0,nrow(pdat), length(ntop))
	vv<-c()
	for(i in ntop)
		{
		mod<- grr_foster$model_indx[i]
		mm<- paste("~",paste(names(fixef(out_foster[[mod]]))[-1],collapse="+"))
		names(pdat)<- names(fixef(out_foster[[mod]]))[-1]	
		vv<-c(vv,mod)
		mm<- model.matrix(as.formula(mm),pdat)	
		y_hat[,i]<- mm%*%fixef(out_foster[[mod]])	
		pvar1 <- diag(mm %*% tcrossprod(as.matrix(vcov(out_foster[[mod]])),mm)) # prediction variance
		tvar1 <- pvar1+VarCorr(out_foster[[mod]])$samp[1] 
		y_lo[,i]<- y_hat[,i]-2*(sqrt(tvar1))
		y_hi[,i]<- y_hat[,i]+2*(sqrt(tvar1))
		}
		pdat$y_hat<-y_hat %*% (grr_foster$w[ntop]/sum(grr_foster$w[ntop]))
		pdat$y_hi<- y_hi %*% (grr_foster$w[ntop]/sum(grr_foster$w[ntop]))
		pdat$y_lo<- y_lo %*% (grr_foster$w[ntop]/sum(grr_foster$w[ntop]))	
		pdat$p_hat<-plogis(pdat$y_hat)
		pdat$p_hat<-plogis(pdat$y_hat)
		pdat$p_hi<- plogis(pdat$y_hi)
		pdat$p_lo<- plogis(pdat$y_lo)

		par(mfrow=c(2,2),mar=c(4,3,0.25,0),oma=c(1,2,1,1))	
		# FOSTER 1
		mm<-1
		mn<-fos$mn[vv[mm]]
		sdd<- fos$sdd[vv[mm]]
		xlims=c(fos$mnn[vv[mm]],fos$mxx[vv[mm]])	
		x<-(pdat[,1]*sdd+mn)
		indx<- which(x>=xlims[1] & x<=xlims[2])
		plot(x[indx],
			plogis(y_hat[indx,mm]),
			ylim=c(0,1),
			type='n',las=1,ylab="",
			xlab="Minutes",
			cex.lab=1.3) # total time
		panLab("a) Loading time")
		polygon(c(x[indx],rev(x[indx])),c(plogis(y_hi[indx,mm]),
			rev(plogis(y_lo[indx,mm]))), col="lightgrey",border="lightgrey")
		points(x[indx],plogis(y_hat[indx,mm]),type='l',lwd=3)	

		# FOSTER 2
		mm<-2
		mn<-fos$mn[vv[mm]]
		sdd<- fos$sdd[vv[mm]]
		xlims=c(fos$mnn[vv[mm]],fos$mxx[vv[mm]])	
		x<-(pdat[,1]*sdd+mn)
		indx<- which(x>=xlims[1] & x<=xlims[2])
		plot(x[indx],
			plogis(y_hat[indx,mm]),
			ylim=c(0,1),
			type='n',las=1,ylab="",
			xlab="Minutes",
			cex.lab=1.3) # total time
		panLab("b) Total time")
		polygon(c(x[indx],rev(x[indx])),c(plogis(y_hi[indx,mm]),
			rev(plogis(y_lo[indx,mm]))), col="lightgrey",border="lightgrey")
		points(x[indx],plogis(y_hat[indx,mm]),type='l',lwd=3)	
		
		# FOSTER 3
		mm<-3
		mn<-fos$mn[vv[mm]]
		sdd<- fos$sdd[vv[mm]]
		xlims=c(fos$mnn[vv[mm]],fos$mxx[vv[mm]])	
		x<-(pdat[,1]*sdd+mn)
		indx<- which(x>=xlims[1] & x<=xlims[2])
		plot(x[indx],
			plogis(y_hat[indx,mm]),
			ylim=c(0,1),
			type='n',las=1,ylab="",
			xlab=expression(paste("Mean daily discharge (m"^3,"/s)")),
			cex.lab=1.3) # total time
		panLab("c) Mean daily discharge first")
		polygon(c(x[indx],rev(x[indx])),c(plogis(y_hi[indx,mm]),
			rev(plogis(y_lo[indx,mm]))), col="lightgrey",border="lightgrey")
		points(x[indx],plogis(y_hat[indx,mm]),type='l',lwd=3)	
		
		# FOSTER 4
		mm<-4
		mn<-fos$mn[vv[mm]]
		sdd<- fos$sdd[vv[mm]]
		xlims=c(fos$mnn[vv[mm]],fos$mxx[vv[mm]])	
		x<-(pdat[,1]*sdd+mn)
		indx<- which(x>=xlims[1] & x<=xlims[2])
		plot(x[indx],
			plogis(y_hat[indx,mm]),
			ylim=c(0,1),
			type='n',las=1,ylab="",
			xlab="Degree days",
			cex.lab=1.3) # total time
		panLab("d) Degree days from first fish")
		polygon(c(x[indx],rev(x[indx])),c(plogis(y_hi[indx,mm]),
			rev(plogis(y_lo[indx,mm]))), col="lightgrey",border="lightgrey")
		points(x[indx],plogis(y_hat[indx,mm]),type='l',lwd=3)	
		mtext(side=2, "Predicted probability of mortality",outer=TRUE,line=0,cex=1.3)
		}
	if(n==2.2)
		{
		# DEXTER
		grr_dexter<- subset(tables(3),location=="Dexter Dam")	
		pdat<- data.frame(X=seq(-4,4,0.01))
		ntop<-1
		y_hat<-y_lo<- y_hi<- matrix(0,nrow(pdat), ntop)
		vv<-c()
		for(i in 1:ntop)
			{
			mod<- grr_dexter$model_indx[i]
			vv<-c(vv,mod)
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
		plot(x[indx],plogis(y_hat[indx,1]),ylim=c(0,1),type='n',las=1,
			xlab=expression(paste("Number of fish per truck volume (no./m"^3,")")),
			ylab="Predicted probability of mortality",
			cex.lab=1.3) 
		#panLab("b) Dexter dam")
		axis(2,at=axTicks(2), labels=FALSE,las=1)
		polygon(c(x[indx],rev(x[indx])),
			c(plogis(y_hi[indx,1]),rev(plogis(y_lo[indx,1]))), 
			col="lightgrey",border="lightgrey")
		points(x[indx],plogis(y_hat[indx,1]),type='l',lwd=3)
		}
if(n==3)
	{
	# OPTIMAL DECISIONS FOR FOSTER
	## POLICY PLOT

	# FOSTER DAM
	
	## NO DENSITY CONSTRAINT
	out1<- optimal(mortWght=0.5,maxDens=8000,maxEffort=12,location="Foster Dam")
	out2<- optimal(mortWght=0.5,maxDens=maxDensity,maxEffort=12,location="Foster Dam")
	out3<- optimal(mortWght=0.5,maxDens=8000,maxEffort=12,location="Dexter Dam")
	out4<- optimal(mortWght=0.5,maxDens=maxDensity,maxEffort=12,location="Dexter Dam")
	yyfos1<-dcast(out1,n~truckVolume,value.var="n_trips")
	yyfos2<-dcast(out2,n~truckVolume,value.var="n_trips")
	yydex1<-dcast(out3,n~truckVolume,value.var="n_trips")
	yydex2<-dcast(out4,n~truckVolume,value.var="n_trips")
	dexmax<-max(na.omit(c(unlist(yydex1[,-1]),unlist(yydex2[,-1]))))
	fosmax<-max(na.omit(c(unlist(yyfos1[,-1]),unlist(yyfos2[,-1]))))
	
	par(mfrow=c(2,2),oma=c(2,2,1,6),mar=c(2,2,0,0))
	x<- as.numeric(names(yyfos1)[-1])
	y<- yyfos1$n
	z<-as.matrix(yyfos1[,-1])
	brks<- c(0:5)
	cols<-rev(grey(seq(0,0.9,length.out=length(brks)-1)))
	image(x,y,t(z), col=cols,las=1,breaks=brks,
		xlab="",
		ylab="",cex.lab=1.5,xaxt='n',ylim=c(5,160))
	mtext(side=3, "No density constraint")
	axis(1, at=axTicks(1),labels=FALSE)
	abline(v=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),col="white",lty=3)
	box()
	## NOAA DENSITY CONSTRAINT
	x<- as.numeric(names(yyfos2)[-1])
	y<- yyfos2$n
	z<-as.matrix(yyfos2[,-1])
	image(x,y,t(z), col=cols,las=1,breaks=brks,
		xlab="",

		ylab="",cex.lab=1.5,xaxt='n',yaxt='n',ylim=c(5,160))
	mtext(side=3,"NOAA Density Constraint")
	abline(v=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),col="white",lty=3)
	axis(1, at=axTicks(1),labels=FALSE)
	axis(2, at=axTicks(2),labels=FALSE)
	box()

	# DEXTER
	## NO DENSITY CONSTRAINT
	x<- as.numeric(names(yydex1)[-1])
	y<- yydex1$n
	z<-as.matrix(yydex1[,-1])
	#brks<- c(0:dexmax)
	#cols<-rev(grey(seq(0,0.9,length.out=length(brks)-1)))
	image(x,y,t(z), col=cols,las=1,breaks=brks,
		xlab="",
		ylab="",cex.lab=1.5)
	abline(v=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),col="white",lty=3)
	box()
	## NOAA DENSITY CONSTRAINT
	x<- as.numeric(names(yydex2)[-1])
	y<- yydex2$n
	z<-as.matrix(yydex2[,-1])
	image(x,y,t(z), col=cols,las=1,breaks=brks,
			xlab="",
			ylab="",cex.lab=1.5,yaxt='n')
	axis(2, at=axTicks(2),labels=FALSE)
	abline(v=c(1.135623,4.542492,5.678115,7.57082,9.463525,10.220607),col="white",lty=3)	
	mtext(side=2, "Number of fish to translocate",outer=TRUE,line=0.75,cex=1.3)
	mtext(side=1, "Transport truck volume (cubic meters)",outer=TRUE,line=0.8,cex=1.3)
	box()
	axis(2, at=axTicks(2),labels=FALSE)
	
	# add legend
	par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
	plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
	legend("right",legend= c(1:5),fill=cols,xpd=TRUE,bty='n',
		inset = c(0,0),cex=1.5,title="Number \n of \n hauls")
	}
on.exit(par(op))
}