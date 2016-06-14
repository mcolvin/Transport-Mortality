
dat<- tables("analysis data")

# WHAT RANDOM EFFECTS?

vars<- c("(1|year)+(1|samp)+(1|waterbody)",
"(1|year)+(1|waterbody)",
"(1|samp)+(1|waterbody)",
"(1|year)+(1|samp)",
"(1|year)",
"(1|samp)",
"(1|waterbody)")

# SET UP MODELS TO BE EVALUATED
mods<- paste("mort~1+",vars,sep="")#
# FIT MODELS FOR MODEL SELECTION
out<-list()
for(i in 1:length(mods))
		{
		out[i]<-glmer(as.formula(mods[i]), 
		data = dat,subset=location=="Foster Dam",
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
		print(i)
		print(mods[i])
		}
out_re_fos<- out 

out<- list()
for(i in 1:length(mods))
		{
		out[i]<-glmer(as.formula(mods[i]), 
		data = dat,subset=location=="Dexter Dam",
		family=binomial,
		control=glmerControl(optimizer="bobyqa"))
		print(i)
		print(mods[i])
		}
out_re_dex<- out

## FOSTER ONLY ANALYSIS
out<-list()
vars<- c("loadingTime","haulingTime","doy+I(doy^2)","trip_no","dd_50",
	"fish_per_vol","waterTempCollSite","cloudcover",
	"maxT_C","Q_50","doy50","run_size","delta_temp","Q_01",
	"day_bet")
# SET UP MODELS TO BE EVALUATED
mods<- paste("mort~",vars,"+(1|samp)",sep="")#+location
# FIT MODELS FOR MODEL SELECTION
for(i in 1:length(mods))
		{
		out[i]<-glmer(as.formula(mods[i]), 
		data = dat,
		family=binomial,
		subset=location=="Foster Dam",
		control=glmerControl(optimizer="bobyqa"))
		}
out_foster<- out
# DEXTER ONLY
out<-list()
# SET UP MODELS TO BE EVALUATED
mods<- paste("mort~",vars,"+(1|samp)",sep="")#+location
# FIT MODELS FOR MODEL SELECTION
for(i in 1:length(mods))
		{
		out[i]<-glmer(as.formula(mods[i]), 
		data = dat,
		family=binomial,
		subset=location=="Dexter Dam",
		control=glmerControl(optimizer="bobyqa"))
		}
out_dexter<- out


