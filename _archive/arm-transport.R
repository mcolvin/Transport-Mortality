

# FUNCTIONS TO PREDICT MORTALITY
# DENSITY (FISH PER M^3)
dens<- function(x=14,wght=1/3)
	{
	mn_x<- 14.17077
	std_x<- 8.420106
	x<- (x-mn_x)/std_x
	y<- -7.914902+1.126514*x
	p<- plogis(y)
	return(p)
	}
# TOTAL TIME
ttime<- function(x=144,wght=1/3)
	{
	mn_x<- 144.6322
	std_x<- 60.28575
	x<- (x-mn_x)/std_x
	y<- -7.5793987+ 0.8253778*x
	p<- plogis(y)
	return(p)
	}
# DOY
doy<- function(x=200,wght=1/3)
	{
	mn_x<- 201.9885
	std_x<- 31.0018
	x<- (x-mn_x)/std_x
	y<- -6.7301431+0.7586218*x+  -1.3356942*x^2
	p<- plogis(y)
	return(p)
	}

# ORIGINAL MODEL WEIGHTS 
wghts<- c(0.4739270,0.363562634,0.105552005)
# NORMALIZE TO 1
wghts<- wghts/sum(wghts)	

# WORK DAY UTILITY
x<- c(0,8,200)
y<- c(1,0,0)
work<- approxfun(x,y)

doys<- seq(100, 280,7)
n_fish<- c(10,25,50,75,100,125,150,175,200,225,250,275,300)
states<- expand.grid(doy=doys,n_fish=n_fish)


min_per_fish<- 0.9999
truck_volume<- 2600*0.00378541 # cubic meters
out<- data.frame()
# FIND OPTIMAL DENSITY AND TOTAL TIME
for(i in 1:nrow(states))
	{
	# HOW LONG TO PROCESS FISH INTO TRUCK
	## POSSIBLE OUTCOMES FOR VARYING DENSITIES LOADING TIMES
	tmp<-expand.grid(doy=states$doy[i], n_fish=states$n_fish[i],
		time_cutoff=c(15,30,45,60,75,90,105,120),
		travel_time=c(15,30,45,60,75,90,105,120))
	tmp$n_trips  <- round(tmp$n_fish/ tmp$time_cutoff/min_per_fish,0) # assumes if there is leftover it gets in truck or in a new truck if a alot
	# ALLOCATE FISH EVENLY OVER EACH TRIP
	tmp$truck_dens<- tmp$n_fish/tmp$n_trips/truck_volume
	tmp$tot_time<- tmp$time_cutoff+tmp$travel_time
	tmp$p_mort<- dens(tmp$truck_dens,wghts[1])+ttime(x=tmp$tot_time,wghts[2])+doy(tmp$doy,wghts[3])
	tmp$work_time<- (tmp$n_trips*tmp$tot_time/60)

	tmp$work_score<- work(tmp$work_time)
	tmp$n_morts<- tmp$n_fish*tmp$p_mort # expected mortalities
	tmp$n_morts_score<- (max(tmp$n_morts)-tmp$n_morts)/(max(tmp$n_morts)-min(tmp$n_morts)) # expected mortalities
	tmp$U<- tmp$n_morts_score*0.5 + tmp$work_score*0.5
	optimal<- tmp[which.max(tmp$U),]
	out<- rbind(out,optimal)
	print(i/nrow(states))
	}





warnin


