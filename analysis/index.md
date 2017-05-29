---
title: "Transport ARM"
date: "29 May, 2017"
---

<!--
rmarkdown::render("index.Rmd")
knit("index.Rmd")# returns md file
rmarkdown::render("index.Rmd",
    clean=FALSE,run_pandoc=FALSE)# and leave md file clean =false
##
* add location to dataset and then sim time to 
outplant from that as well as time to load then
use that info to get at rewards
    * bin density
    * by outplant location
    * 
https://stackoverflow.com/questions/31914161/how-to-convert-rmd-into-md-in-r-studio
-->




```r
head(dat_unstd)
```

```
##     location year doy       date trip.no driver trucknum truckVolume
## 1 Dexter Dam 2008 220 2008-08-07       1   MATT      145    10.22061
## 2 Dexter Dam 2008 240 2008-08-27       1   BRAD      145    10.22061
## 3 Dexter Dam 2008 241 2008-08-28       1   BRAD      145    10.22061
## 4 Dexter Dam 2008 255 2008-09-11       1   BRAD      145    10.22061
## 5 Dexter Dam 2009 195 2009-07-14       1  JASON      145    10.22061
## 6 Dexter Dam 2009 196 2009-07-15       1   BRAD      145    10.22061
##    waterbody waterTempCollSite waterTempStart waterTempEnd
## 1       N FK             11.67          11.11        11.11
## 2       N FK             10.56          10.56        11.11
## 3       N FK             10.56          10.56        11.11
## 4       N FK             10.56           9.44        10.00
## 5 M FK PATTY             11.11          11.11        12.22
## 6 M FK PATTY             11.11          11.11        11.11
##   waterTempRelease loadingStart loadingStop loadingTime haulingStart
## 1            11.11         8:30     9:30:00          60      9:40:00
## 2            12.22         8:30     9:30:00          60      9:35:00
## 3            12.22         8:30     9:30:00          60      9:35:00
## 4            11.67         8:00     9:45:00         105      9:45:00
## 5             8.89         8:15    10:15:00         120     10:20:00
## 6            10.56         8:00     9:20:00          80      9:25:00
##   haulingStop haulingTime nFish nJacks nLoss nLikelyLoss Comments   maxT_C
## 1    10:45:00          65   135    166     0           0          28.33333
## 2    11:15:00         100   100     11     0           0          24.44444
## 3    11:15:00         100    69      9     0           0          29.44444
## 4    11:00:00          75   147     11     0           0          32.22222
## 5    12:15:00         115   171    153     7           0          26.66667
## 6    11:30:00         125   125    107     0           0          31.11111
##   cloudcover trip_no tot_time delta_temp delta_trip_temp fish_per_vol
## 1      0.625       1      125      -0.56            0.00    13.208609
## 2      0.500       1      160       0.55            0.55     9.784155
## 3      0.125       1      160       0.55            0.55     6.751067
## 4      0.000       1      180      -0.56            0.56    14.382707
## 5      0.375       1      235       1.11            1.11    16.730905
## 6      0.250       1      205       0.00            0.00    12.230193
##   mort.1 mort.2 trap01 trap50 doy01 doy50 run_size    dd_alb dd  dd_50
## 1      0    135    170    190    87   164    14149  9422.888 NA 3134.4
## 2      0    100    170    190    87   164    14149  9422.888 NA 3134.4
## 3      0     69    170    190    87   164    14149  9422.888 NA 3134.4
## 4      0    147    170    190    87   164    14149  9422.888 NA 3134.4
## 5      7    164    168    190    66   145    25795 10753.888 NA 5002.7
## 6      0    125    168    190    66   145    25795 10753.888 NA 5002.7
##       Q_01     Q_50   dd_01 trap_total day_bet     p_mort
## 1 3603.247 2667.078 7387.70        220       7 0.00000000
## 2 3603.247 2667.078 7387.70        118      20 0.00000000
## 3 3603.247 2667.078 7387.70        163       1 0.00000000
## 4 3603.247 2667.078 7387.70        173      14 0.00000000
## 5 3297.557 3395.411 8573.05        423       5 0.04268293
## 6 3297.557 3395.411 8573.05        313       1 0.00000000
```

```r
dat_unstd$totalTrans<- dat_unstd$nFish + dat_unstd$nJacks

run_comp<- aggregate(cbind(trap_total,totalTrans)~year+doy+location,dat_unstd,max)
run_comp$flag<- ifelse(run_comp$trap_total<run_comp$totalTrans,1,0)
run_comp<- subset(run_comp,flag==0)

fit<- glm(cbind(totalTrans,(trap_total-totalTrans))~location*doy*year,
    data=run_comp,
    family="binomial")
fit<- glm(cbind(totalTrans,(trap_total-totalTrans))~location*doy,
    data=run_comp,
    family="binomial")   
    
run_comp$p<- run_comp$totalTrans/run_comp$trap_total
library(lattice)
xyplot(p~doy|year,run_comp, subset=location=="Foster Dam")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
xyplot(p~doy|year,run_comp, subset=location=="Dexter Dam")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png)

```r
run_comp$phat<- predict(fit, run_comp, type="response")
plot(p~phat,run_comp)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-3.png)

```r
vcov(fit)
```

```
##                          (Intercept) locationFoster Dam           doy
## (Intercept)             0.0244829331      -0.0244829331 -1.172503e-04
## locationFoster Dam     -0.0244829331       0.0394058516  1.172503e-04
## doy                    -0.0001172503       0.0001172503  5.665459e-07
## locationFoster Dam:doy  0.0001172503      -0.0001877961 -5.665459e-07
##                        locationFoster Dam:doy
## (Intercept)                      1.172503e-04
## locationFoster Dam              -1.877961e-04
## doy                             -5.665459e-07
## locationFoster Dam:doy           9.075264e-07
```


## Decision space

### Outplant densities



### Outplant locations

#### Foster

There are 3 current locations where spring chinook are outplanted
in the South Santiam River above Foster Dam. These locations are

1. S.S. - Caukins 2.970195 0.52626145
2. S.S. - River Bend 3.390801 0.06450340
3. S.S. -  Gordon Rd. LN( 3.898760 0.19941085

The maximum transport density for fish from Foster was 37.8 fish per m3

Did decisions of densities 1,5,10,15,20,25, 30,35, 40.

There are 3 truck volumes used at Foster Dam

1. 7.57082  
2. 9.463525  
3. 10.220607 (most common)

It takes ~4.6 minutes to process a fish at Foster Dam

The most number of fish ever transported at Foster was 88 in a day
#### Dexter

The maximum transport density for fish from Dexter was 57.2 fish per m3

There are 3 truck volumes used at Dexter Dam

1. 1.135623 
2. 4.542492 
3. 5.67811

It takes ~0.25 minutes to process a fish at Foster Dam
The most number of fish ever transported at Dexter Dam was 414 in a day
## Predicting survival




First the predictors need to be 



Now we take that dataset and generate predictions. But to get 
the full variance we need to do this in chunks. 


```r
outcomes<- lapply(1:10,function(x)
    {
    ## MATRIX OF PROBABILITIES TO SIMULATE OUTCOMES
    nn<-nrow(yyy[[x]])
    y_hat<-matrix(NA,nrow=nn,ncol=nrow(confModSet))
    for(i in 1:length(confModSet$model_indx))
        {
        mm<- paste("~",paste(confModSet$pred[i],collapse="+"))
        mm<- model.matrix(as.formula(mm),yyy[[x]])   
        
        ## INDEX FOR MODEL OUTPUT
        xx<- confModSet$model_indx[i]
        ### VARIANCE COMPONENTS
        re<- VarCorr(out_foster[[xx]])$samp[1] 
        vcv<-suppressWarnings(as.matrix(vcov(out_foster[[xx]])))
        pvar1 <- diag(mm %*% tcrossprod(vcv,mm))
        tvar1 <- pvar1+re ## TOTAL VARIANCE

        ## y_hat pr survival in order of weights
        y_hat[,i]<-plogis(rnorm(nn,mm%*%fixef(out_foster[[xx]]),tvar1))
        ## NUMBER OF SURVIVORS
        y_hat[,i]<-rbinom(nn,
            yyy[[x]]$n_per_trip,
            y_hat[,i])        
        }
        
    tmp<- data.frame(
        id=max(yyy[[x]]$id),
        outplantLocation= max(yyy[[x]]$out_location),
        density=max(yyy[[x]]$density_u),
        truckVolume=max(yyy[[x]]$truckVolume_u),
        ntranslocated=max(yyy[[x]]$ntrap), 
        nTrips= max(yyy[[xx]]$n_trips),
        totalLoading=sum(yyy[[x]]$loadingTime_u),
        totalHauling=sum(yyy[[x]]$haulingTime_u)*2,
        totalMortalities=sum(y_hat %*% confModSet$w),
        totalTransported=sum(yyy[[x]]$n_per_trip))
    return(tmp)
    })

outcomes<- do.call("rbind",outcomes)
```

Now we link the many predicted survivals to the number of fish to 
translocate (i.e., number of fish in trap. 


```r
n_hat<- y_hat
## EACH COLUMN IN Y_HAT IS A PREDICTION FROM THE MODEL. 
for(i in 1:ncol(y_hat))
    {
    
    }
## SURVIVORS ACCOUNTING FOR MODEL UNCERTAINTY
```

## Rewards matrix

fn(effort and survivors) trying to minimize effort 
and maximize number of fish to outplant.
Time to locations...

The NOAA constraint is 10.56689 fish per m3


```r
## UTILITY
### MIN(EFFORT) + MAX(SURVIVORS)
dens<- seq(1, 25, by =1)    
loc<- c(1,2,3)

R<- matrix(NA,nrow=length(dens)*length(loc), ncol=nstates)  
```


    

  


## PREDICTED RESPONSES	






	



# Figures





Figure 1.  Study area and location of tributary populations of interest.
Open squares denote the first impassible major tributary dams.
The open triangle denotes the location of Willamette Falls.
Arrow denotes north and direction of river flow.  





Figure 2.  Predictions (solid black line) and 95% prediction interval (gray area) for models
representing hypotheses predicting the mortality rate of transported Chinook Salmon.
Panels represent prediction for models retained in the confidence model set for Foster
(panel a) and Dexter Dam (panel b).  





Figure 3.  Optimal transportation policies for spring-run Chinook Salmon trapped at Foster Dam.
The white lines denote commonly used tank volumes (300, 1200, 1500, 2000, 2700 gallons) in Upper 
Willamette River spring-run Chinook Salmon translocation. 
 
NOTE: What is going on here is there is a benefit of more trips because it reduces the loading time,
The 'flickering' is in part due to ties and how density is handled.  So this is a policy of load up 
a few fish and go rather than waiting a long time even if fish are at low densities in the truck.
Loading times are very high for Foster, much higher than Dexter but loaded densities are much lower.
Figure 3 and Figure 4 are still preliminary as I am working out some of the flickering.
Although Figure 4 behaves as one would expect and I do not anticipate it changing much.
 



Figure 4.  Optimal transportation policies for spring-run Chinook Salmon trapped at Dexter Dam.
The white lines denote commonly used tank volumes (300, 1200, 1500, 2000, 2700 gallons) in Upper
Willamette River spring-run Chinook Salmon translocation.      
    
# Tables

Table 1.  Candidate explanatory variables, descriptions, and hypotheses. 



Table 2. Summary of candidate variable used to evaluate the factors related to Chinook Salmon transport mortality in the Upper Willamette Basin.



Table 3. Candidate models and model selection criteria for the set of candidate models for 
adult spring Chinook Salmon transport mortality. All models contained an intercept and 
a random effect of sample in addition to the variable being evaluated.  Models retained for 
full model; cumulative model weight = 0.95.



 
  
  
  
  
Table 4. Parameter estimates and 95% confidence limits (CL) of fixed and random effects 
for best approximating model of adult Chinook Salmon transport mortality. 
Random effects are variance components. 



	

# Tables




Table 1.  Candidate explanatory variables, descriptions, and hypotheses. 


|Predictor type                              |Predictor                                                          |Description and hypothesis                                                                                                                                                                                                                                                                                                      |
|:-------------------------------------------|:------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Truck and translocation occasion conditions |Loading time (min)                                                 |How long it took to load fish.  Hypothesis:  longer loading durations increase the probability of mortality events.                                                                                                                                                                                                             |
|NA                                          |Hauling Time (min)                                                 |How long it took to transport fish to outplant location.  Hypothesis:  longer travel durations increase the probability of mortality events.                                                                                                                                                                                    |
|NA                                          |Total time handling (min)                                          |How long it took to load and transport fish.  Hypothesis:  longer trip durations increase the probability of mortality events.                                                                                                                                                                                                  |
|NA                                          |Trip number for the day                                            |Number of trips conducted in a day.  Hypothesis:  this metric integrates the number of fish trapped and a rate of trapping (sort of).  Probability of mortality events increase with the number of daily hauls, later hauls may have a higher probability of mortality                                                          |
|NA                                          |Truck volume (m~3~)                                                |The volume of the transport tank.  Hypothesis:  Trucks with larger volumetric capacity decreases probability of mortality events.                                                                                                                                                                                               |
|NA                                          |Number of fish transported                                         |The number of fish transported in a single trip.  Hypothesis:  higher numbers of fish transported increases the probability of mortality events.                                                                                                                                                                                |
|NA                                          |Number of fish per truck volume (no./m~3~)                         |The density of fish in the truck (fish/truck volume).  Hypothesis:  higher densities increase the probability of mortality events                                                                                                                                                                                               |
|NA                                          |Difference in temperature between collection facility and tank (C) |Difference between collection facility water temperature and transport water temperature.  Hypothesis:  fish are more likely to experience mortality events if stressful water temperature differences are experienced.                                                                                                         |
|NA                                          |Day of the year transported                                        |Day of year of hauling event.  Hypothesis:  Fish from the middle of the run will experience higher mortality due to environmental conditions (e.g., temperatures below dam, water quality) conditions that may peak mid-run.                                                                                                    |
|Daily weather and trap conditions           |Number of fish in trap                                             |Cumulative number of fish captured in the trap.  Hypothesis:  high numbers of fish in the trap may increase stress, which may increase the probability of mortality events.                                                                                                                                                     |
|NA                                          |Water temperature at collection facility (C)                       |Temperature at collection facility at time of hauling.  Hypothesis: fish collected from higher water temperature are more likely to experience mortality events                                                                                                                                                                 |
|NA                                          |Maximum daily air temperature (C)                                  |Maximum air temperature when loading and transporting.  Hypothesis:  Exposure to elevated air temperatures increase the probability of mortality.                                                                                                                                                                               |
|NA                                          |Average number of days since last trap tending                     |Average number of days between trap tending occasions (cumulative).  Hypothesis:  increased time in the trap increases stress, which may increase the probability of mortality events.                                                                                                                                          |
|NA                                          |Cloud cover index                                                  |Daily cloud cover when loading and transport.  Hypothesis:  Exposure to sunlight increases the probability of mortality                                                                                                                                                                                                         |
|Annual conditions                           |Run size                                                           |Number of fish escaping Willamette Falls.  Hypothesis: density dependent factors (e.g., horizontal disease transmission) increase probability and magnitude of a mortality event                                                                                                                                                |
|NA                                          |Day of the year 50% of run passed Willamette Falls                 |Day of 50% passage of immigrating spring Chinook at Willamette Falls.  This variable is intended to characterize the potential effects of run timing.  Hypothesis:  as transport mortalities increase in years where the run timing is earlier and therefore fish are likely to have higher exposure in the migratory corridor. |
|NA                                          |Degree days from first fish (C)                                    |Number of accumulated degree days from the first fish ascending Willamette Falls to the first fish captured at Dexter dam.  Hypothesis: the probability of mortality increases with the accumulated degree days                                                                                                                 |
|NA                                          |Degree days 50% fish (C)                                           |Number of accumulated degree days from 50% passage of immigrating spring Chinook at Willamette Falls to the 50% passage of fish at Dexter Dam.  Hypothesis: the probability of mortality events increases with increasing degree days.                                                                                          |
|NA                                          |Mean daily discharge first (m~3~/s)                                |Average discharge at dam from the first fish ascending Willamette Falls plus travel time to the first fish captured at each dam.  Hypothesis: the probability of mortality decreases with increased flows                                                                                                                       |
|NA                                          |Mean daily discharge 50% fish (m~3~/s)                             |Average discharge at dam from 50% passage of immigrating spring Chinook at Willamette Falls plus travel time to the 50% passage of fish at each dam.   Hypothesis: the probability of mortality decreases with increased flows.                                                                                                 |

Table 2. Summary of candidate variable used to evaluate the factors related to Chinook Salmon transport mortality in the Upper Willamette Basin.


|Predictor                                                          |Mean (SD)           |  Minimum|  Maximum|Mean (SD)           |  Minimum|  Maximum|
|:------------------------------------------------------------------|:-------------------|--------:|--------:|:-------------------|--------:|--------:|
|Loading time (min)*                                                |122.78 (68.39)      |    20.00|   300.00|43.53 (27.77)       |    10.00|   135.00|
|Hauling Time (min)                                                 |37.37 (17.98)       |     0.00|   120.00|77.28 (20.86)       |    45.00|   130.00|
|Total time handling (min)                                          |160.16 (66.99)      |    65.00|   345.00|120.82 (37.55)      |    65.00|   235.00|
|Trip number for the day                                            |1.35 (0.62)         |     1.00|     4.00|2.13 (1.46)         |     1.00|     8.00|
|Truck volume (m3)                                                  |2.8 (2.1)           |     1.14|     5.68|9.27 (1.72)         |     5.68|    10.22|
|Number of fish transported                                         |33.4 (24.22)        |     1.00|   144.00|149.81 (72.34)      |    18.00|   387.00|
|Number of fish per truck volume (no./m3)                           |12.31 (8.59)        |     0.18|    57.24|17.01 (7.34)        |     1.76|    37.86|
|Difference in temperature between collection facility and tank (C) |0.84 (1.19)         |    -1.11|     3.89|-0.18 (1.18)        |    -3.89|     2.22|
|Day of the year transported                                        |199.2 (36.28)       |   128.00|   276.00|206.27 (19.83)      |   177.00|   255.00|
|Number of fish in trap                                             |250.39 (191.64)     |     0.00|   715.00|569.91 (287.43)     |    26.00|  1108.00|
|Water temperature at collection facility (C)                       |10.55 (1.06)        |     6.67|    12.22|14.07 (1.71)        |    10.56|    16.11|
|Maximum daily air temperature (C)                                  |25.37 (4.77)        |    12.78|    37.78|27.32 (3.73)        |    19.44|    33.89|
|Average number of days since last trap tending                     |4.59 (2.99)         |     1.00|    14.00|4 (3.85)            |     0.00|    20.00|
|Cloud cover index                                                  |0.34 (0.37)         |     0.00|     1.00|0.37 (0.29)         |     0.00|     1.00|
|Run size                                                           |40995.92 (15660.29) | 14149.00| 65293.00|40262.18 (14851.96) | 14149.00| 65293.00|
|Day of the year 50% of run passed Willamette Falls                 |144.61 (10.02)      |   134.00|   164.00|143.41 (9.9)        |   132.00|   164.00|
|Degree days from first fish (C)                                    |5163.48 (1395.19)   |  2836.70|  7381.40|7874.73 (624.45)    |  6946.65|  8573.05|
|Degree days 50% fish (C)                                           |4810.05 (656.22)    |  3839.14|  5724.34|4937.75 (1189.98)   |  3134.40|  6830.88|
|Mean daily discharge first (m3/s)                                  |3580.31 (135.07)    |  3413.34|  3956.36|3041.61 (186.12)    |  2791.83|  3603.25|
|Mean daily discharge 50% fish (m3/s)                               |1884.99 (259.96)    |  1281.62|  2193.30|3465.55 (425.01)    |  2667.08|  3966.73|

Table 3. Candidate models and model selection criteria for the set of candidate models for 
adult spring Chinook Salmon transport mortality. All models contained an intercept and 
a random effect of sample in addition to the variable being evaluated.  Models retained for 
full model; cumulative model weight = 0.95.


| model_indx|predictor                                                          |location   |   k|   AICc| dAICc|  lik|    w| cum_w|
|----------:|:------------------------------------------------------------------|:----------|---:|------:|-----:|----:|----:|-----:|
|          6|Number of fish per truck volume (no./m3)                           |Dexter Dam | 161| 801.86|  0.00| 1.00| 0.94|  0.94|
|          2|Hauling Time (min)                                                 |Dexter Dam | 161| 808.49|  6.63| 0.04| 0.03|  0.98|
|         14|Mean daily discharge first (m3/s)                                  |Dexter Dam | 161| 811.74|  9.89| 0.01| 0.01|  0.98|
|         11|Day of the year 50% of run passed Willamette Falls                 |Dexter Dam | 161| 812.07| 10.21| 0.01| 0.01|  0.99|
|          9|Maximum daily air temperature (C)                                  |Dexter Dam | 161| 813.35| 11.49| 0.00| 0.00|  0.99|
|          5|Degree days 50% fish (C)                                           |Dexter Dam | 161| 814.33| 12.47| 0.00| 0.00|  0.99|
|          8|Cloud cover index                                                  |Dexter Dam | 161| 815.10| 13.24| 0.00| 0.00|  0.99|
|          7|Water temperature at collection facility (C)                       |Dexter Dam | 161| 815.30| 13.44| 0.00| 0.00|  1.00|
|         10|Mean daily discharge 50% fish (m3/s)                               |Dexter Dam | 161| 815.44| 13.58| 0.00| 0.00|  1.00|
|          1|Loading time (min)*                                                |Dexter Dam | 161| 816.03| 14.18| 0.00| 0.00|  1.00|
|         12|Run size                                                           |Dexter Dam | 161| 816.15| 14.29| 0.00| 0.00|  1.00|
|         13|Difference in temperature between collection facility and tank (C) |Dexter Dam | 161| 816.18| 14.32| 0.00| 0.00|  1.00|
|          4|Trip number for the day                                            |Dexter Dam | 161| 816.24| 14.38| 0.00| 0.00|  1.00|
|         15|Average number of days since last trap tending                     |Dexter Dam | 161| 816.39| 14.53| 0.00| 0.00|  1.00|
|          3|Day of the year transported                                        |Dexter Dam | 162| 827.90| 26.04| 0.00| 0.00|  1.00|
|          1|Loading time (min)*                                                |Foster Dam | 106| 312.29|  0.00| 1.00| 0.84|  0.84|
|         14|Mean daily discharge first (m3/s)                                  |Foster Dam | 106| 316.28|  4.00| 0.14| 0.11|  0.96|
|          5|Degree days 50% fish (C)                                           |Foster Dam | 106| 320.37|  8.08| 0.02| 0.01|  0.97|
|          3|Day of the year transported                                        |Foster Dam | 107| 321.65|  9.36| 0.01| 0.01|  0.98|
|         11|Day of the year 50% of run passed Willamette Falls                 |Foster Dam | 106| 322.57| 10.28| 0.01| 0.00|  0.98|
|          2|Hauling Time (min)                                                 |Foster Dam | 106| 323.70| 11.41| 0.00| 0.00|  0.99|
|          7|Water temperature at collection facility (C)                       |Foster Dam | 106| 323.73| 11.44| 0.00| 0.00|  0.99|
|          4|Trip number for the day                                            |Foster Dam | 106| 324.34| 12.05| 0.00| 0.00|  0.99|
|         10|Mean daily discharge 50% fish (m3/s)                               |Foster Dam | 106| 324.40| 12.12| 0.00| 0.00|  0.99|
|         12|Run size                                                           |Foster Dam | 106| 325.18| 12.90| 0.00| 0.00|  0.99|
|          6|Number of fish per truck volume (no./m3)                           |Foster Dam | 106| 325.28| 12.99| 0.00| 0.00|  1.00|
|         15|Average number of days since last trap tending                     |Foster Dam | 106| 325.28| 13.00| 0.00| 0.00|  1.00|
|         13|Difference in temperature between collection facility and tank (C) |Foster Dam | 106| 325.34| 13.05| 0.00| 0.00|  1.00|
|          8|Cloud cover index                                                  |Foster Dam | 106| 325.41| 13.13| 0.00| 0.00|  1.00|
|          9|Maximum daily air temperature (C)                                  |Foster Dam | 106| 325.44| 13.16| 0.00| 0.00|  1.00|

 
  
  
  
  
Table 4. Parameter estimates and 95% confidence limits (CL) of fixed and random effects 
for best approximating model of adult Chinook Salmon transport mortality. 
Random effects are variance components. 



|Parameter    | model|pred                                     | Estimate| 2.5 %| 97.5 %|location   |
|:------------|-----:|:----------------------------------------|--------:|-----:|------:|:----------|
|(Intercept)  |     1|Loading time (min)*                      |    -7.00| -8.96|  -5.86|Foster Dam |
|.sig01       |     1|Loading time (min)*                      |     2.16|  1.29|   3.96|Foster Dam |
|loadingTime  |     1|Loading time (min)*                      |     1.17|  0.59|   1.95|Foster Dam |
|(Intercept)  |     6|Number of fish per truck volume (no./m3) |    -7.70| -9.22|  -6.65|Dexter Dam |
|.sig01       |     6|Number of fish per truck volume (no./m3) |     3.11|  2.26|   4.57|Dexter Dam |
|fish_per_vol |     6|Number of fish per truck volume (no./m3) |     1.71|  0.82|   2.84|Dexter Dam |

