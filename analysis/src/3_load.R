

trap.cnt<-read.csv("./dat/trap_counts.csv")
WF.count<-read.csv("./dat/will_falls_counts.csv")
temp.data<-read.csv("./dat/temp_data.csv")
flow<- read.csv("./dat/discharge_data.csv")
weath<-read.csv("./dat/weather_dat.csv")
# trans<-read.csv("./dat/transport_data_jp.csv")
trans<-read.csv("./dat/TransMorts_29Sept2014_v2.csv")


# truck volumes
truck_volumes<- as.data.frame(matrix(c('120',	-99,
	'138',	1200,
	'145',	2700,
	'23',	1500,
	'27',	2000,
	'28',	2500,
	'41',	1500,
	'58',	1500,
	'-99',	-99,
	'Portable',	300),ncol=2, byrow=TRUE))
names(truck_volumes)<- c('trucknum','truckVolume')


