
# FIXED AND RANDOM EFFECTS FOR CONFIDENCE MODEL SETS
ms<- tables(3) # GET MODEL SELECTION RESULTS
# FOSTER DAM
confModSet<- subset(ms[ms$location=="Foster Dam",])
# prds MOST ARE STANDARDIZED THEREFORE -2,-1,0,1,2
# data.frame TO GENERATE STOCHASTIC REALIZATIONS OF INPUTS
confModSet<- merge(confModSet,prds,by="predictor",all.x=TRUE)
confModSet<- confModSet[order(confModSet$w,decreasing=TRUE),]

## NUMBER OF STOCHASTIC REPLICATES
n<- 500000

pdat<- lapply(1:nrow(confModSet),function(x)
    {
    if(confModSet$standardize[x]==1){xx<-runif(n,-2,2)}
    if(confModSet$standardize[x]==0 & confModSet$pred[x]!=1){xx<-sample(c(1:4),
        n,replace=TRUE)}
    if(confModSet$standardize[x]==0 & confModSet$pred[x]==1){xx<-rep(1,n)}  
    return(xx)
    })

    
    
    
## COMBINE UP AND CREATE A DATA.FRAME
## TO PREDICT SURVIVALS FOR
yy<- as.data.frame(do.call("cbind",pdat))


## NAMES FOR MODEL.MATRIX
names(yy)<- confModSet$pred 
yy$trip_no<- as.factor(yy$trip_no)
# NUMBER OF FISH TO TRANSLOCATE
yy$ntrap<- floor(runif(n,1,89))## max translocated at foster
    
## ASSIGN LOCATION TO OUTPLANT TO
yy$out_location<- sample(c(1:3),n,replace=TRUE)

yy$haulingTime_u<- 0
indx<- which(yy$out_location==1)
yy[indx,]$haulingTime_u<- rlnorm(length(indx),2.970195,0.52626145)
indx<- which(yy$out_location==2)
yy[indx,]$haulingTime_u<- rlnorm(length(indx),3.390801, 0.06450340)
indx<- which(yy$out_location==3)
yy[indx,]$haulingTime_u<- rlnorm(length(indx),3.898760,0.19941085)
    
## TOTAL TRIP TIME
yy$tripTime<-yy$haulingTime_u* 2


## TRUCK VOLUMES
yy$truckVolume_u<- sample(c(7.57082,9.463525,10.220607),
    n,replace=TRUE)
### STANDARDIZE FOR ESTIMATING MORTALITY
yy$fish_per_vol<- scale(yy$truckVolume_u,
		center= fos[which(fos[,1]==8),2],
		scale = fos[which(fos[,1]==8),3])

        
## DENSITY
yy$density_u<- sample(c(1,5,10,15,20,25,30,35,40),
    n,replace=TRUE)
### STANDARDIZE DENSITY FOR ESIMATING MORTALITY
yy$fish_per_vol<- scale(yy$density_u,
		center= fos[which(fos[,1]==31),2],
		scale = fos[which(fos[,1]==31),3])
        
## NUMBER OF TRIPS
### HOW MANY CAN FIT IN THE TRUCK GIVEN DENSITY
yy$n_per_trip<- floor(yy$density_u*yy$truckVolume_u)
yy$n_trips<- ceiling(yy$ntrap/yy$n_per_trip)
yy$fish_left_over<- ifelse(yy$n_trips>1,
    yy$ntrap-((yy$n_trips-1)*yy$n_per_trip),0)
yy$n_per_trip<- ifelse(yy$n_trips==1,yy$ntrap,yy$n_per_trip)
   
## NEED TO BREAK UP TRIPS GIVEN SCENARIO (MULTIPLE TRIPS)... 
### ADD A SCENARIO ID   
yy$id<- 1:n

### EXAPND DATASET FOR MULTIPLE TRIPS
#### AND UPDATE TRIP NUMBER... IT IS WRONG
yy$trip_no<-1


## update trip number
trip_no<-as.vector(unlist(lapply(yy$n_trips,function(x) c(1:x))))
trip_no<- ifelse(trip_no>4,4,trip_no)

## expand dataset
## a row for each trip
yy<-as.data.frame(lapply(yy,function(x) rep(x,yy$n_trips)))
yy$trip_no<-factor(trip_no,levels=c(1,2,3,4))

## UPDATE LAST TRIP TO TRANSPORT THE LAST FISH
yy$n_per_trip<- ifelse(yy$trip_no==yy$n_trips & yy$n_trips>1, yy$fish_left_over,yy$n_per_trip)


## LOADING TIME GIVEN VOLUME AND DENSITY  
yy$loadingTime_u<- yy$n_per_trip*4.6
yy$loadingTime<- scale(yy$loadingTime_u,
    fos[1,2],fos[1,3])
  
yy$samp<-1  
yy$actual_density_u<- yy$n_per_trip/yy$truckVolume_u   
## update scaled densities for each trip
yy$fish_per_vol<- scale(yy$actual_density_u,
    fos[6,2],fos[6,3])

yhat<-matrix(0,nrow=nrow(yy),ncol=nrow(confModSet))
     for(i in 1:length(confModSet$model_indx))
        {
        xx<- confModSet$model_indx[i]
        yhat[,i]<-predict(out_foster[[xx]],yy, type="link",re.form = NA,allow.new.levels=TRUE)       
        }
ssss<-yhat %*% confModSet$w
        
        
## NUMBER OF SURVIVORS
yy$morts<-rbinom(nrow(yy),
    yy$n_per_trip,
    plogis(yhat %*% confModSet$w))       

   


   
tmp<- ddply(yy,.(id),summarize,
    id=max(id),
    outplantLocation= max(out_location),
    density=max(density_u),
    truckVolume=max(truckVolume_u),
    ntranslocated=max(ntrap), 
    nTrips= max(n_trips),
    totalLoading=sum(loadingTime_u),
    totalHauling=sum(haulingTime_u)*2,
    totalMortalities=sum(morts),
    totalTransported=sum(n_per_trip))


write.csv(tmp, "./output/outcomes.csv")
