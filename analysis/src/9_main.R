

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
	