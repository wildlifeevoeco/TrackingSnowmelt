############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# February 22nd, 2020 ##########################################################################

############# Script 3: Making random points ###############################################################

############################################################################################################
############################################################################################################

## Packages
library(raster)
library(rgdal)
library(maptools)
library(spatstat)
library(adehabitatHR)

## Load in the data ##

data<-readRDS("Output/Locs/CleanDataSeasonal.RDS")

#### Migration - Brownian Bridge

Migr<-subset(data,season=="Mig")

## Add in point ID
Migr$PtID<-c(1:nrow(Migr))

Migr<-droplevels(Migr)

### Make a field that matches the file name associated with the BB produced in migration mapper
Migr$file<-paste(Migr$ID,"_sp",substr(Migr$year,3,4),"_ASCII.asc",sep="")

bursts<-unique(Migr$file)

t<-1
for(i in bursts){
  
  ## subset by file
  dataSub<-subset(Migr,file==i)
  
  ## Duplicate each row 10 times
  dataRand<-dataSub[rep(seq_len(nrow(dataSub)), each=10),]
  
  
  ### Reading in the BB raster and making a polygon window
  # Read in the file
  #bb <- raster("Input/UDs/466_sp17_ASCII.asc")
  bb<-raster(paste("Output/MigrationMapper/UDs/",i,sep=""))
  # Set the projection
  projection(bb) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # Get the sorted values of the bb
  cutoff <- sort(values(bb), decreasing=TRUE)
  # Get the cumulative sum
  vlscsum <- cumsum(cutoff)
  # Set the cutoff percentage
  cutoff <- cutoff[vlscsum > 0.9999][1]
  # Reclassify the raster, 1 for part of contour, 0 for outside
  bb_rc <- reclassify(bb, rcl=matrix(c(-1,cutoff,NA, cutoff, 1, 1),2,3, byrow=T))
  # Turn it into a polygon
  bb_poly <- rasterToPolygons(bb_rc, dissolve = TRUE)
  # Transform it to UTM (CRS of the data)
  bb_UTM<-spTransform(bb_poly, CRSobj="+proj=utm +zone=21 ellps=WGS84")
  
  ## For some stupid reason "runifpoint" won't accept input from previous step,
  ## but writing it as a shapefile and reading it back in with readOGR works...
  ## Even though they are the same class of objects...grrr...
  shapefile(bb_UTM,"Output/Temp/shape.shp",overwrite=T)
  bb_new<-rgdal::readOGR("Output/Temp/shape.shp")
  
  ## Generate the random points
  RandomPts<-runifpoint(n=nrow(dataRand),win=bb_new)
  
  ##Turn the points into a SpatialPoints class and extract the coordinates
  
  RandomPtsSp<-as.SpatialPoints.ppp(RandomPts)
  RandCoords<-coordinates(RandomPtsSp)
  
  names<-c("ID","Herd","year","CalfStart","CalfEnd","Time","x","y","burst","JDateTime","JDay","SpringMigDst",
           "StartMig","EndMig","season","PtID","file","Pres")
  
  RandData<-data.frame(dataRand$ID,dataRand$Herd,dataRand$year,dataRand$CalfDate,dataRand$EndCalf,
                       dataRand$Time,RandCoords,dataRand$burst,dataRand$JDateTime,dataRand$JDay,dataRand$springMigDst,
                       dataRand$StartMig,dataRand$EndMig,dataRand$season,dataRand$PtID,dataRand$file)
  RandData$Pres<-0
  
  colnames(RandData)<-names
  
  PresData<-dataSub
  PresData$Pres<-1
  
  colnames(PresData)<-names
  
  NewData<-rbind(PresData,RandData)
  
  if (t==1){
    AllData<-NewData
    
  } else {
    AllData<-rbind(AllData,NewData)
    
  }
  print(t/length(bursts))
  t<-t+1
  
}

MigrationData<-AllData

MigrationData$file<-NULL

##### Calving - Kernels and random points

Calf<-subset(data,season=="Calving")
Calf<-droplevels(Calf)
Calf$PtID<-c(1:nrow(Calf))

bursts<-unique(Calf$burst)

t<-1
for(i in bursts){
  
  ## subset by burst
  dataSub<-subset(Calf,burst==i)
  dataSub<-droplevels(dataSub)
  ## Duplicate each row 10 times. 
  dataRand<-dataSub[rep(seq_len(nrow(dataSub)), each=10),]
  
  ## Set up spatial data for kernelUD function
  id<-data.frame(dataSub$burst)
  xy<-cbind(dataSub$x,dataSub$y)
  coordinates(id)<-xy
  
  ## Make the kernels
  kern<-kernelUD(id, grid=800, extent=2, h=400)
  
  ## Get the 99% kernel HR
  hr99<-getverticeshr(kern, percent = 99)
  
  
  ## For some stupid reason "runifpoint" won't accept input from previous step,
  ## but writing it as a shapefile and reading it back in with readOGR works...
  ## Even though they are the same class of objects...grrr...
  shapefile(hr99,"Output/Temp/shape.shp",overwrite=T)
  hr99_new<-rgdal::readOGR("Output/Temp/shape.shp")
  
  ## Generate the random points
  RandomPts<-runifpoint(n=nrow(dataRand),win=hr99_new)
  
  ##Turn the points into a SpatialPoints class and extract the coordinates
  
  RandomPtsSp<-as.SpatialPoints.ppp(RandomPts)
  RandCoords<-coordinates(RandomPtsSp)
  
  names<-c("ID","Herd","year","CalfStart","CalfEnd","Time","x","y","burst","JDateTime","JDay","SpringMigDst",
           "StartMig","EndMig","season","PtID","Pres")
  
  RandData<-data.frame(dataRand$ID,dataRand$Herd,dataRand$year,dataRand$CalfDate,dataRand$EndCalf,
                       dataRand$Time,RandCoords,dataRand$burst,dataRand$JDateTime,dataRand$JDay,dataRand$springMigDst,
                       dataRand$StartMig,dataRand$EndMig,dataRand$season,dataRand$PtID)
  RandData$Pres<-0
  
  colnames(RandData)<-names
  
  PresData<-dataSub
  PresData$Pres<-1
  
  colnames(PresData)<-names
  
  NewData<-rbind(PresData,RandData)
  
  if (t==1){
    AllData<-NewData
    
  } else {
    AllData<-rbind(AllData,NewData)
    
  }
  print(t/length(bursts))
  t<-t+1
  
}

CalvingData<-AllData

AllDataRand<-rbind(MigrationData,CalvingData)

### Quick plots to make sure everything lines up and looks good!

bursts<-unique(AllDataRand$burst)
sub<-subset(AllDataRand,burst==bursts[15])
subPres<-subset(sub,Pres==1)
subRand<-subset(sub,Pres==0)
plot(subRand$x,subRand$y,pch=ifelse(subRand$season=="Mig",1,2))
points(subPres$x,subPres$y,col="red",pch=ifelse(subPres$season=="Mig",1,2))

### Save data and random points

saveRDS(AllDataRand,"Output/Locs/CleanAndRand.RDS")


################# Extract NDSI and NDVI data to points ##################

library(raster)

data<-readRDS("Output/Locs/CleanAndRand.RDS")

years<-unique(data$year)

## The correct bands to use are: 4,7,10,13,16,19,22. Start at 4 and add three for each subsequent year.
t<-4
j<-1
for(i in years){
  ## Read in NDSI data
  NDSI<-raster("Inputs/Landcover/NDSI-snow-off.tif",band=t)
  ## Reproject to UTM
  NDSIrp<-projectRaster(NDSI, crs = '+proj=utm +zone=21 ellps=WGS84')
  ## Read in peak IRG data
  PeakIRG<-raster(paste("Inputs/Landcover/PeakIRG/PeakIRG",i,".tif",sep=""))
  
  ## Subset the data
  subData<-subset(data,year==i)
  
  ## Extract data and add to dataframe
  subData$SnowOff<-extract(NDSIrp, data.frame(subData$x,subData$y))
  subData$IRGDay<-extract(PeakIRG, data.frame(subData$x,subData$y))
  
  if(j==1){
    NewData<-subData
  }else{
    NewData<-rbind(NewData,subData)
  }
  t<-t+3
  j<-j+1
  print(i)
}

data<-NewData

data$DaysToSnow<-data$JDateTime-data$SnowOff
data$DaysToIRG<-data$JDateTime-data$IRGDay

saveRDS(data,"Output/Locs/LocsWSpatial.RDS")

####################  END #############################

