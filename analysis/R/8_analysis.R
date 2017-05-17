

	
	x<-aggregate(nFish~year+location,dat_unstd,sum)# what is used for analysis	
	aggregate(nFish~location,x,min)
	aggregate(nFish~location,x,max)
	
	
	
	dat_unstd$mn<- dat_unstd$loadingTime/dat_unstd$nFish
	x<-aggregate(mn~location,dat_unstd,median)# what is used for analysis
		
	


	# RANDOM EFFECTS MODEL SELECTION
	## FOSTER
	tmp<-as.data.frame(t(sapply(1:length(out_re_fos), function(x) summary(out_re_fos[[x]])$AICtab)))
	tmp$model<- sapply(1:length(out_re_fos), function(x){
		tmp<- paste(as.character(summary(out_re_fos[[x]])$call$formula),collapse="~")
		return(substr(tmp,2,nchar(tmp)))})
	tmp$model_indx<- c(1:length(out_re_fos))
	tmp$k<- nrow(dat)-tmp$df.resid
	tmp$AICc<- tmp$AIC+((2*tmp$k*(tmp$k+1))/(nrow(dat)-tmp$k-1))
	tmp$dAICc<- tmp$AICc-min(tmp$AICc)
	tmp<- tmp[order(tmp$dAICc, decreasing=FALSE),]
	tmp$lik<- exp(-0.5*tmp$dAICc)
	tmp$w<- tmp$lik/sum(tmp$lik)
	tmp$cum_w<- cumsum(tmp$w)
	tmp$location<-"Foster Dam"
	out<- tmp

	## DEXTER
	tmp<-as.data.frame(t(sapply(1:length(out_re_dex), function(x) summary(out_re_dex[[x]])$AICtab)))
	tmp$model<- sapply(1:length(out_re_dex), function(x){
		tmp<- paste(as.character(summary(out_re_dex[[x]])$call$formula),collapse="~")
		return(substr(tmp,2,nchar(tmp)))})
	tmp$model_indx<- c(1:length(out_re_dex))
	tmp$k<- nrow(dat)-tmp$df.resid
	tmp$AICc<- tmp$AIC+((2*tmp$k*(tmp$k+1))/(nrow(dat)-tmp$k-1))
	tmp$dAICc<- tmp$AICc-min(tmp$AICc)
	tmp<- tmp[order(tmp$dAICc, decreasing=FALSE),]
	tmp$lik<- exp(-0.5*tmp$dAICc)
	tmp$w<- tmp$lik/sum(tmp$lik)
	tmp$cum_w<- cumsum(tmp$w)
	tmp$location<-"Dexter Dam"
	out<- rbind(out,tmp)
