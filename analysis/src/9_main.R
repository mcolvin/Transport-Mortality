
setwd("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/analysis/")

source("./src/1_global.R") 	
source("./src/2_functions.R")         
source("./src/3_load.R")     
source("./src/4_clean.R" )
source("./src/5_tables.R") 
source("./src/6_figures.R")  	
source("./src/7_models.R")
	
source("./src/8_analysis.R")
	
    plot(trans$waterTempStart-trans$waterTempEnd,        
(trans$nLoss+trans$nLikelyLoss)/trans$nFish   )
    
    
    
figures(1)
savePlot("./figures/study-area.wmf",type="wmf")
savePlot("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/submission/figure-1.pdf",type='pdf')   
savePlot("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/submission/figure-1.tiff",type='tiff')  	

## PREDICTED RESPONSES	
figures(2.1)# foster
savePlot("./figures/foster_predicted.wmf",type='wmf')
savePlot("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/submission/figure-2.pdf",type='pdf')   
savePlot("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/submission/figure-2.tiff",type='tiff')   
figures(2.2)# dexter
savePlot("./figures/dexter_predicted.wmf",type='wmf')
savePlot("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/submission/figure-3.pdf",type='pdf')   
savePlot("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/submission/figure-3.tiff",type='tiff')  	
	
figures(3)# OPTIMAL POLICIES FOR FOSTER AN DEXTER
savePlot("./figures/figure-4-opt-haul-plot.wmf",type="wmf")
savePlot("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/submission/figure-4.pdf",type='pdf')   
savePlot("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/submission/figure-4.tiff",type='tiff')  	

write.csv(tables(3), "./tables/tbl3-model-selection.csv")
write.csv(tables(4),"./tables/tbl4-model-estimates.csv")


setwd("C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality/analysis/")
topdir<- "C:/Users/mcolvin/Documents/projects/PSM/analyses/Transport-Mortality"
# COMPILE FIGURES TO DOCX
knitr::knit("./src/2_figures.Rmd")	
knitr::pandoc('2_figures.md', format='docx')
file.copy("2_figures.docx", paste0(topdir,"/2_figures.docx"),overwrite=TRUE)
file.remove("2_figures.docx");file.remove("./2_figures.md")

# COMPILE TABLES TO DOCX
knitr::knit("./src/3_tables.Rmd")	
knitr::pandoc('3_tables.md', format='docx')
file.copy("./3_tables.docx",paste0(topdir,"/3_tables.docx"),overwrite=TRUE)
file.remove("./3_tables.docx");file.remove("./3_tables.md")	


 
	
	
	figures(1)
	savePlot("./figures/01_study_area.wmf",type="wmf")
	
	# a 2x1 plot of the prediction interval of important factors
	# there are 3 for foster and 1 for dexter representing variables
	# that are within 0.95 weight.  
	figures(2)
	
	figures(3)
	figures(4)
	# want a plot of 'predicted' effect to show how small it is..
	
	figures(7)# policy plot for foster
	figures(8)# polica plot for dexter
	
	
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
	