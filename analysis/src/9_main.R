# TO DO
 

setwd("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/analysis/")

source("./src/1_global.R") 	
source("./src/2_functions.R")         
source("./src/3_load.R")     
source("./src/4_clean.R" )
source("./src/5_tables.R") 
source("./src/6_figures.R")  	
source("./src/7_models.R")
	
source("./src/8_analysis.R")
	
	
	
	figures(1)
	savePlot("./figures/01_study_area.wmf",type="wmf")
	
	# a 2x1 plot of the prediction interval of important factors
	# there are 3 for foster and 1 for dexter representing variables
	# that are within 0.95 weight.  
	figures(2)
	
	figures(3)
	figures(4)
	# want a plot of 'predicted' effect to show how small it is..
	
	## TABLES
	tbl2<- tables(2)# means
	write.csv(tbl2, "./output/tbl2.csv",row.names = FALSE)
	tbl3<- tables(3)# model selection
	write.csv(tbl3, "./output/tbl3.csv",row.names = FALSE)
	tbl4<- tables(4)# parameter estimates
	write.csv(tbl4, "./output/tbl4.csv",row.names = FALSE)
	
	# OPIMAL DECISION FOR FOSTER
	tbl5<- tables(5)
	# SUBSET TO AVERAGE CONDITIONS
	tbl5<-subset(tbl5,Q_01==0 &  dd_50==0)
	write.csv(tbl5, "./output/tbl5.csv",row.names = FALSE)
	# OPIMAL DECISION FOR DEXTER
	tbl6<- tables(6)
	write.csv(tbl6, "./output/tbl6.csv",row.names = FALSE)
	