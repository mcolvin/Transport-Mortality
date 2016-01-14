

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
	
	
	## TABLES
	tbl2<- tables(2)
	write.csv(tbl2, "./output/tbl2.csv",row.names = FALSE)
	tbl3<- tables(3,model_fits=out_all)
	write.csv(tbl3, "./output/tbl3.csv",row.names = FALSE)
	tbl4<- tables(4)
	write.csv(tbl4, "./output/tbl4.csv",row.names = FALSE)
	