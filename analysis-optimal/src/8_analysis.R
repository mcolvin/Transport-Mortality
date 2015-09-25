

hhh<-NULL
for(nf in seq(20,1000,20)){
  for(haul in seq(30,120,30))
	{
	for(driv in 1:5)
		{
		hhh<-rbind(hhh,c(nf,haul,driv,find.me(n.fish=nf, haul.time=haul, drivers= driv)))
		}
	  }
	}

colnames(hhh) = c("Number.of.Fish","Ave.time.to.drive","No.drivers","Total.trips","Est.mort.rate")

out<- na.omit(hhh)

write.csv(hhh,"Optimal hauling operations.csv")
