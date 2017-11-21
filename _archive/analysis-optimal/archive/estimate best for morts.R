
## inverse logit function
inv.logit<-function(eta){1/(1+exp(-eta))}

find.me<-function(n.fish,haul.time,drivers)
	{
	## constraint maximum number to haul per trip
	max.per.trip<-350

	## vector of number of trips
	trips<- c(ceiling(n.fish/max.per.trip):20)

	## means and sd to standardize data
	trip.X<-1.7; trip.sd<-1.11
	time.X<-140; time.sd<-57.83

	## average minutes per fish
	min.per.fish<- 0.918

	### ave number fish hauled per trip
	n.haul<- n.fish/trips

	## standardize trips
	trips.std<-(trips-trip.X)/trip.sd

	## handling time per load
	hand.time<-n.haul*min.per.fish

	# calculate total time
	tot.time<-((hand.time+haul.time)-time.X)/time.sd

	## predict mortality, assume average day of year and degree days
	pred.mort<-inv.logit(-5.339 + 0.461*trips.std + 0.71*tot.time)

	## haul time each way assume 0.5 hr to dump fish, allow multiple drivers
	all.time<- (hand.time*trips) + (haul.time*2 + 30)*(trips/drivers)

	## dump combos that take longer than assumed 14 hr work day
	pred.mort<-ifelse(all.time> 14*60, 1000,pred.mort)

	## if everything is not possible, then return NA
	if(sum(pred.mort) == length(pred.mort)*1000) return(c(NA,NA))

	else return(c(trips[pred.mort == min(pred.mort)]*drivers,min(pred.mort)))
	}

hhh<-NULL
for(nf in seq(20,1000,20)){
  for(haul in seq(30,120,30))
	{
	for(driv in 1:5)
		{
		hhh<-rbind(hhh,c(nf,haul,driv,find.me(n.fish=nf, haul.time=haul, drivers= driv)))
		}
	  }
	}

colnames(hhh) = c("Number.of.Fish","Ave.time.to.drive","No.drivers","Total.trips","Est.mort.rate")

na.omit(hhh)

write.csv(hhh,"Optimal hauling operations.csv")
