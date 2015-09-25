

trap.cnt<-read.csv("./dat/trap_counts.csv")
WF.count<-read.csv("./dat/will_falls_counts.csv")
temp.data<-read.csv("./dat/temp_data.csv")
flow<- read.csv("./dat/discharge_data.csv")
weath<-read.csv("./dat/weather_dat.csv")
# trans<-read.csv("./dat/transport_data_jp.csv")
# trans<-read.csv("./dat/transport_data_new.csv",skip=3) # dexter 2013
trans<-read.csv("./dat/transport_data.csv",skip=3) #no dexter 2013, this jives more with jp's analysis



#	will_rivrs<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "willamette_rivers")
#	will_bndry<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "willamette_boundary")
#	oreg_bndry<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "or_state_boundary")
#	oreg_dams<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "willamette_dams")
#	UWR_chinook_reservoirs<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "UWR_chinook_reservoirs")
#	oreg_wbody<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "willamette_wb")
#	red<-readOGR("C:/Users/mcolvin/Documents/projects/gis coverages" ,"MigratoryCorridorDamToDam_ll_nad27")
#	full<-readOGR("C:/Users/mcolvin/Documents/projects/gis coverages" ,"MigratoryCorridorFull_ll_nad27")
#	alll<-readOGR("C:/Users/mcolvin/Documents/projects/gis coverages" ,"all")
#	allriv<-readOGR("C:/Users/mcolvin/Documents/projects/gis coverages" ,"All_Willamette_Rivers")
#	allriv<-spTransform(allriv, CRS("+proj=longlat +datum=NAD27"))
#
#	#	full<- readShapeLines("C:/Users/mcolvin/Documents/projects/gis coverages/MigratoryCorridorFull_ll_nad27.shp",
#	#		proj4string=CRS("+proj=longlat +datum=NAD27"))
#	#	red<-readShapeLines("C:/Users/mcolvin/Documents/projects/gis coverages/MigratoryCorridorDamToDam_ll_nad27.shp",
#	#		proj4string=CRS("+proj=utm + zone=10N +datum=NAD83"))
#	#	SP<-readShapePoly("area_infl_ind.shp", IDvar=NULL,
#	#		proj4string=CRS("+proj=utm +zone=22 +units=m +south"))
#	will_bndry<-spTransform(will_bndry, CRS("+proj=longlat +datum=NAD27"))
#	UWR_chinook_reservoirs<-spTransform(UWR_chinook_reservoirs, CRS("+proj=longlat +datum=NAD27"))
#	oreg_bndry_ll<-spTransform(oreg_bndry, CRS("+proj=longlat +datum=NAD27"))
#	oreg_dams_ll<-spTransform(oreg_dams, CRS("+proj=longlat +datum=NAD27"))
#	oreg_wbody<-spTransform(oreg_wbody, CRS("+proj=longlat +datum=NAD27"))
#	alll<-spTransform(alll, CRS("+proj=longlat +datum=NAD27"))
#	plotDams<- c("Dexter","Foster Reservoir","Cougar Reservoir","Big Cliff Dam","Fall Creek Res",
#		"Trail Bridge Reg. Res.")
#