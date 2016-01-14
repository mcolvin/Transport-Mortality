figures<- function(n){

if(n==1)
	{# study area
	par(mar=c(5,6,1,1.5))
	plot(will_bndry,col="grey90",axes=TRUE,las=1,xlab="Longitude", ylab="",cex.lab=1.5)
		mtext(side=2, "Lattitude", line=4,cex=1.5)
	plot(full,add=TRUE,lwd=4)
	plot(alll,add=TRUE,col="black",lwd=4)
	plot(red, add=TRUE,col="black",lwd=4)
	plot(full[full$NAME=="Willamette River",],add=TRUE,col="black",lwd=4)
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
	# FIGURE OF PREDICTED EFFECT OF VARIABLES 
	## FIT MODEL
	plotDat<- tables('a')
	plotDat<- subset(plotDat, volume <2 )
	plotDat<- plotDat[order(plotDat$trap_total,plotDat$doy ),]
	par(mfrow=c(3,1),oma=c(3,3,1,1),mar=c(2,4,0,0))
	
	# TOTAL TIME
	plot(p_hat~doy, pdat, subset=(trap_total==0 & 
		fish_per_vol==0),type='n',las=1,ylab="",xlab="",xaxt='n',
		ylim=c(0,0.004))
	axis(1,at=axTicks(1),labels=FALSE)
	for(i in c(-1,0,1))
		{
		points(p_hat~doy,plotDat[plotDat$tot_time==i & plotDat$fish_per_vol==0 & plotDat$trap_total==0,], lty=i+2,type='l')	
		}
		
	# DENSITY
	plot(p_hat~doy, pdat, subset=(trap_total==0 & tot_time==0),
		type='n',las=1,ylab="",xlab="",xaxt='n',
		ylim=c(0,0.004))
	axis(1,at=axTicks(1),labels=FALSE)
	for(i in c(-1,0,1))
		{
		points(p_hat~doy,plotDat[plotDat$trap_total==0 & 
			plotDat$tot_time==0 & plotDat$fish_per_vol==i,], lty=i+2,type='l')

		}	
		
	# TRAP TOTAL
	plot(p_hat~doy, pdat, 
		subset=(tot_time==0 & fish_per_vol==0),
		type='n',las=1,ylab="",xlab="",
		ylim=c(0,0.004))
	for(i in c(-1,0,1))
		{
		points(p_hat~doy,plotDat[plotDat$trap_total==i &  plotDat$tot_time==0 & plotDat$fish_per_vol==0,], lty=i+2,type='l')
		}	
	}
if(n==3)
	{
	plotDat<- tables('a')
	plotDat<- subset(plotDat, volume <2 )
	plotDat<- plotDat[order(plotDat$trap_total,plotDat$doy ),]
	par(mfrow=c(3,3),oma=c(3,3,1,1),mar=c(2,2,0,0))
	indx<- c(-1,0,1)
	# TOTAL TIME
	for(i in 1:3)
		{
		polyDat<- subset(plotDat, tot_time==indx[i] & fish_per_vol==0 & trap_total==0)	
		xx<-ifelse(i>1, "n","s")
		plot(p_hat~doy, polyDat,type='n',las=1,ylab="",xlab="",xaxt='n',
			ylim=c(0,0.35),yaxt=xx)
		axis(1,at=axTicks(1),labels=FALSE)
		if(i>1){axis(2, at=axTicks(2),labels=FALSE)}
		polygon(c(polyDat$doy, rev(polyDat$doy)),c(polyDat$p_hat_lo,rev(polyDat$p_hat_hi)),col="grey90")
		#points(p_hat~doy,polyDat, lty=i+2,type='l',lwd=2)
		}	

	# DENSITY
	for(i in 1:3)
		{
		polyDat<- subset(plotDat, tot_time==0 & fish_per_vol==indx[i] & trap_total==0)	
		xx<-ifelse(i>1, "n","s")
		plot(p_hat~doy, polyDat,type='n',las=1,ylab="",xlab="",xaxt='n',
			ylim=c(0,0.35),yaxt=xx)
		axis(1,at=axTicks(1),labels=FALSE)
		if(i>1){axis(2, at=axTicks(2),labels=FALSE)}
		polygon(c(polyDat$doy, rev(polyDat$doy)),c(polyDat$p_hat_lo,rev(polyDat$p_hat_hi)),col="grey90")
		#points(p_hat~doy,polyDat, lty=i+2,type='l',lwd=2)
		}	
		
	# TRAP TOTAL
	for(i in 1:3)
		{
		polyDat<- subset(plotDat, tot_time==0 & fish_per_vol==0 & trap_total==indx[i])	
		xx<-ifelse(i>1, "n","s")
		plot(p_hat~doy, polyDat,type='n',las=1,ylab="",xlab="",xaxt='s',
			ylim=c(0,0.35),yaxt=xx)
		if(i>1){axis(2, at=axTicks(2),labels=FALSE)}
		polygon(c(polyDat$doy, rev(polyDat$doy)),c(polyDat$p_hat_lo,rev(polyDat$p_hat_hi)),col="grey90")
		#points(p_hat~doy,polyDat, lty=i+2,type='l',lwd=2)
		}	
	mtext(side=1, "Day of year",outer=TRUE,line=1,cex=1.3)
	mtext(side=2, "Mortality", outer=TRUE,line=1.3,cex=1.3)

	}
if(n==5)
	{
	polyDat<- subset(plotDat, tot_time%in%c(-1,0,1) & fish_per_vol==0 & trap_total==0)		
	plot(y_hat~doy,polyDat,type='n',ylim=c(-20,0))
		points(y_hat~doy,polyDat,subset=tot_time==-1)
		points(y_hat_lo~doy,polyDat,subset=tot_time==-1,col="red")
		points(y_hat_hi~doy,polyDat,subset=tot_time==-1,col="green")
		points(y_hat~doy,polyDat,subset=tot_time==0)
		points(y_hat_lo~doy,polyDat,subset=tot_time==0,col="red")
		points(y_hat_hi~doy,polyDat,subset=tot_time==0,col="green")		
		points(y_hat~doy,polyDat,subset=tot_time==1)
		points(y_hat_lo~doy,polyDat,subset=tot_time==1,col="red")
		points(y_hat_hi~doy,polyDat,subset=tot_time==1,col="green")
		
		
	polyDat<- subset(plotDat, tot_time%in%c(-1,0,1) & fish_per_vol==0 & trap_total==0)		
	plot(risk~doy,polyDat,type='n',ylim=c(0,0.1))
		points(risk~doy,polyDat,subset=tot_time==-1)		
		
		
		
	}

}