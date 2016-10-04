
maxDensity<- 1/(25*0.00378541)# from will fish oparations plan <= 25 gallons per fish


# HYPOTHESES TABLE
tbl1<- read.csv("./tables/hypotheses.csv")

trap_cnt<-read.csv("./dat/trap_counts.csv")
WF_count<-read.csv("./dat/will_falls_counts.csv")
temp_data<-read.csv("./dat/temp_data.csv")
flow<- read.csv("./dat/discharge_data.csv")
weath<-read.csv("./dat/weather_dat.csv")
#trans<-read.csv("./dat/transport_data_jp.csv")
trans<-read.csv("./dat/transport_data_new.csv",skip=3) # dexter 2013
# trans<-read.csv("./dat/transport_data.csv",skip=3) #no dexter 2013, this jives more with jp's analysis

prds<-matrix(c( 
# Trip and truck specific variables
	"loadingTime","Loading time (min)*", 
	"haulingTime","Hauling Time (min)",
	"tot_time","Total time handling (min)",
	"trip_no","Trip number for the day",
	"truckVolume","Truck volume (m3)",
	"nFish" ,"Number of fish transported",
	"fish_per_vol","Number of fish per truck volume (no./m3)",
	"delta_temp","Difference in temperature between collection facility and tank (C)",
# Trap and loading conditions
	"doy","Day of the year transported",
	"trap_total","Number of fish in trap",# number in trap
	"waterTempCollSite","Water temperature at collection facility (C)",
	"maxT_C",	"Maximum daily air temperature (C)",	
	"day_bet","Average number of days since last trap tending",
	"cloudcover","Cloud cover index",
	#"waterTempRelease", "Water temperature at liberation site",
# Run size, timing, and tributary conditions
	"run_size","Run size",	
	"doy50","Day of the year 50% of run passed Willamette Falls",
	"dd_01","Degree days from first fish (C)",
	"dd_50","Degree days 50% fish (C)",
	"Q_01", "Mean daily discharge first (m3/s)",
	"Q_50","Mean daily discharge 50% fish (m3/s)"),ncol=2, byrow=TRUE)

prds<- as.data.frame(prds,stringsAsFactors = FALSE)
names(prds)<- c("pred","predictor")
prds$standardize<- 1
prds[prds$pred %in% c("trip_no"),]$standardize<-0



spfiles<- function()
	{
	#will_rivrs<<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "willamette_rivers")
	will_bndry<<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "willamette_boundary", verbose=FALSE)
	oreg_bndry<<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "or_state_boundary", verbose=FALSE)
	oreg_dams<<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "willamette_dams", verbose=FALSE)
	#UWR_chinook_reservoirs<<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "UWR_chinook_reservoirs")
	#oreg_wbody<<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "willamette_wb")
	red<<-readOGR("C:/Users/mcolvin/Documents/projects/gis coverages" ,"MigratoryCorridorDamToDam_ll_nad27", verbose=FALSE)
	full<<-readOGR("C:/Users/mcolvin/Documents/projects/gis coverages" ,"MigratoryCorridorFull_ll_nad27", verbose=FALSE)
	alll<<-readOGR("C:/Users/mcolvin/Documents/projects/gis coverages" ,"all", verbose=FALSE)
	#allriv<<-readOGR("C:/Users/mcolvin/Documents/projects/gis coverages" ,"All_Willamette_Rivers")
	#allriv<<-spTransform(allriv, CRS("+proj=longlat +datum=NAD27"))
	#
	full<<- readShapeLines("C:/Users/mcolvin/Documents/projects/gis coverages/MigratoryCorridorFull_ll_nad27.shp",
			proj4string=CRS("+proj=longlat +datum=NAD27"))
	red<<-readShapeLines("C:/Users/mcolvin/Documents/projects/gis coverages/MigratoryCorridorDamToDam_ll_nad27.shp",
		proj4string=CRS("+proj=utm + zone=10N +datum=NAD83"))
	#SP<<-readShapePoly("area_infl_ind.shp", IDvar=NULL,
	#	proj4string=CRS("+proj=utm +zone=22 +units=m +south"))
	will_bndry<<-spTransform(will_bndry, CRS("+proj=longlat +datum=NAD27"))
	#UWR_chinook_reservoirs<<-spTransform(UWR_chinook_reservoirs, CRS("+proj=longlat +datum=NAD27"))
	oreg_bndry_ll<<-spTransform(oreg_bndry, CRS("+proj=longlat +datum=NAD27"))
	oreg_dams_ll<<-spTransform(oreg_dams, CRS("+proj=longlat +datum=NAD27"))
	#oreg_wbody<<-spTransform(oreg_wbody, CRS("+proj=longlat +datum=NAD27"))
	alll<<-spTransform(alll, CRS("+proj=longlat +datum=NAD27"))

	}

	
	
	