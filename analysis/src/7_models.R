
# WHAT RANDOM EFFECTS?
out<-list()
vars<- c("(1|site_yr)+(1|samp)+(1|waterbody)",
"(1|site_yr)+(1|waterbody)",
"(1|samp)+(1|waterbody)",
"(1|site_yr)+(1|samp)",
"(1|site_yr)",
"(1|samp)",
"(1|waterbody)")

# SET UP MODELS TO BE EVALUATED
mods<- paste("mort~tot_time+doy+I(doy^2)+trip_no+dd_50+fish_per_vol+waterTempCollSite+cloudcover+maxT_C+dd_01+Q_50+doy50+run_size+delta_temp+Q_01+day_bet+",vars,sep="")#
# FIT MODELS FOR MODEL SELECTION
for(i in 1:length(mods))
		{
		out[i]<-glmer(as.formula(mods[i]), 
		data = dat,
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
		print(i)
		print(mods[i])
		}
out_re<- out 

# EVALUATE MODELS
out<-list()
vars<- c("tot_time","doy+I(doy^2)","trip_no","dd_50",
	"fish_per_vol","1","waterTempCollSite","cloudcover",
	"maxT_C","dd_01","Q_50","doy50","run_size","delta_temp","Q_01",
	"day_bet","trap_total")
	
	#"tot_time*location","doy*location+I(doy^2)*location","trip_no*location","dd_50*location",
	#"fish_per_vol*location","waterTempCollSite*location","cloudcover*location",
	#"maxT_C*location","dd_01*location","Q_50*location","doy50*location","run_size*location","delta_temp*location","Q_01*location",
	#"day_bet*location")

	#"tot_time+location","doy+location+I(doy^2)+location","trip_no+location","dd_50+location",
	#"fish_per_vol+location","waterTempCollSite+location","cloudcover+location",
	#"maxT_C+location","dd_01+location","Q_50+location","doy50+location","run_size+location","delta_temp+location","Q_01+location",
	#"day_bet+location")
# SET UP MODELS TO BE EVALUATED
mods<- paste("mort~",vars,"+(1|samp)",sep="")#
# FIT MODELS FOR MODEL SELECTION
for(i in 1:length(mods))
		{
		out[i]<-glmer(as.formula(mods[i]), 
		data = dat,
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
		print(i)
		print(mods[i])
		}
out_all<- out # analysis for dexter and foster

## FOSTER ONLY ANALYSIS
dat_foster<-subset(dat, location=="Foster Dam")
out<-list()
vars<- c("tot_time","doy+I(doy^2)","trip_no","dd_50",
	"fish_per_vol","waterTempCollSite","cloudcover",
	"maxT_C","Q_50","doy50","run_size","delta_temp","Q_01",
	"day_bet")
# SET UP MODELS TO BE EVALUATED
mods<- paste("mort~",vars,"+(1|samp)",sep="")#+location
# FIT MODELS FOR MODEL SELECTION
for(i in 1:length(mods))
		{
		out[i]<-glmer(as.formula(mods[i]), 
		data = dat_foster,
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
		}
out_foster<- out
# DEXTER ONLY
dat_dexter<-subset(dat, location=="Dexter Dam")
out<-list()
vars<- c("tot_time","doy+I(doy^2)","trip_no","dd_50",
	"fish_per_vol","waterTempCollSite","cloudcover",
	"maxT_C","Q_50","doy50","run_size","delta_temp","Q_01",
	"day_bet")
# SET UP MODELS TO BE EVALUATED
mods<- paste("mort~",vars,"+(1|samp)",sep="")#+location
# FIT MODELS FOR MODEL SELECTION
for(i in 1:length(mods))
		{
		out[i]<-glmer(as.formula(mods[i]), 
		data = dat_dexter,
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
		}
out_dexter<- out


