figures<- function(n){

if(n==1)
	{# study area
	par(mar=c(4,6,1,1.5))
	plot(will_bndry,col="grey90",axes=TRUE,las=1,xlab="Longitude", ylab="",cex.lab=1.5)
		mtext(side=2, "Lattitude", line=4,cex=1.5)
	plot(full,add=TRUE,lwd=4)
	plot(alll,add=TRUE,col="black",lwd=4)
	plot(red, add=TRUE,col="black",lwd=4)
	plot(full[full$NAME=="Willamette River",],add=TRUE,col="black",lwd=4)
	plot(oreg_dams_ll[oreg_dams_ll$damname%in%plotDams,],add=TRUE,cex=1.5, col="black",pch=22,bg="white")
	plot(oreg_dams_ll[oreg_dams_ll$damname%in%"Willamette Falls",],add=TRUE,cex=1.5, col="black",pch=24,bg="white")
	text(-124, 43.4,label="N")	
	arrows(-124, 43.46,-124,43.75)	
	# INSET OREGON IN THE UPPER RIGHT
	par(new=TRUE,oma=c(24,18,0,0))
	plot(oreg_bndry_ll,axes=FALSE);box()
	plot(will_bndry,add=TRUE,col="grey90")
	}
if(n==2)
	{
	# foster and dexter
	
	par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(1,1,1,1))
	# SOUTH SANTIAM
	plot(allriv[allriv$NAME %in%c("South Santiam River","Middle Santiam River","Quartzville Creek"),], 
		col='black',axes=FALSE,xlim=c(-122.8,-122.3))
	rect(-123.1,44.4, -122.8, 44.85,col="white",border=NA)
	plot(UWR_chinook_reservoirs[UWR_chinook_reservoirs$WB_GNIS_NM %in% c("Foster Lake",
		"South Santiam River","Middle Santiam River","Green Peter Lake"),],col="black",add=TRUE)
	plot(oreg_dams_ll[oreg_dams_ll$damname%in%"Foster Reservoir",],add=TRUE,cex=1.5, col="black",pch=22,bg="white")
	# Dexter
	plot(allriv[allriv$NAME %in%c("N Fk Middle Fk Willamette R","Middle Fork Willamette River"),],
		add=FALSE, col="black")
	# add in hills creek (more than 1)
	plot(allriv[allriv$STREAMS_ID== 22475,],add=TRUE)
	plot(UWR_chinook_reservoirs[UWR_chinook_reservoirs$WB_GNIS_NM %in% c("Hills Creek Reservoir",
		"Dexter Reservoir","Lookout Point Reservoir"),],col="black",add=TRUE)	
	plot(oreg_dams_ll[oreg_dams_ll$damname%in%"Dexter",],add=TRUE,cex=1.5, col="black",pch=22,bg="white")

	
	}

}