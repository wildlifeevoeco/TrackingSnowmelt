############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# May 17th, 2020                    ############################################################

############# Script S3: Table and Figure for Appendix S3 ##################################################

############################################################################################################
############################################################################################################


######  Step 1 - Prepping random locations to import to GEE  #########

### Read in the data and packages

data<-readRDS("Output/Locs/CleanData.RDS")
library(adehabitatHR)
library(spatstat)
library(raster)
library(sp)

#i<-"BUCHANS"
herds<-unique(data$Herd)

t<-1
for(i in herds){
  sub<-subset(data, Herd==i)
  sp<-SpatialPoints(coords=data.frame(sub$x,sub$y))
  kernel<-kernelUD(sp)
  hr<-getverticeshr(kernel,percent=99)
  Rands<-spsample(n=1000,x=hr,type="random")
  plot(hr)
  points(Rands)
  Coords<-coordinates(Rands)
  randPts<-data.frame(Coords,i)
  colnames(randPts)<-c("x","y","Herd")
  if(t==1){
    AllRandom<-randPts
  }else{
    AllRandom<-rbind(AllRandom,randPts)
  }
  t<-t+1
}

AllRandom$PtID<-c(1:nrow(AllRandom))

summary(AllRandom)

write.csv(AllRandom,"Output/RandLocsForGEE.csv")

##############  Take this output and run NDSI extract script in Google Earth Engine:
# 
# // === NDSI Previous to Snow Off ===
#   // Alec Robitaille
# // May 19 2020
# 
# 
# // --- Images
# var ndsi = ee.ImageCollection('MODIS/006/MOD10A1');
# var pts = ee.FeatureCollection('users/mppl55/NDSIsampling');
# 
# 
# // --- Mask cloud function
# function maskClouds(img) {
#   return img.updateMask(img.select(['NDSI_Snow_Cover_Basic_QA']).lt(3));
# }
# 
# // --- Add dates (julian day and year) function
# function addDates(img) {
#   var date = img.date();
#   return img.addBands(ee.Image([date.getRelative('day', 'year'),
#                                 date.get('year')]).rename(['doy', 'year'])).float();
# }
# 
# // --- Filter and prep image collection
# var sub = ndsi
# .filter(ee.Filter.dayOfYear(45, 200))
# .filter(ee.Filter.date('2007-01-01', '2013-12-31'));
# 
# sub = sub
# .map(maskClouds)
# .map(addDates)
# .select(['NDSI_Snow_Cover', 'NDSI_Snow_Cover_Class', 'NDSI', 'doy', 'year']);
# 
# 
# // --- Reduce
# var reduce = sub.map(function(img) {
#   return(img.reduceRegions({
#     collection: pts,
#     reducer: ee.Reducer.mean(),
#     scale: 30,
#     crs: 'EPSG:32621'
#   }));
# });
# 
# 
# 
# // --- Export
# var out = reduce.flatten().filter(ee.Filter.neq('NDSI_Snow_Cover', null))
# 
# Export.table.toDrive({
#   collection: out,
#   folder: 'Output',
#   description: 'GEE_NDSI_data',
#   fileFormat:'.csv'
# });
# 


############  Step 2: Extract the NDSI data  ###########

library(raster)
NDSI<-read.csv("Output/GEE/GEE_NDSI.csv")

head(NDSI)

summary(NDSI$NDSI)
summary(NDSI$NDSI_Snow_Cover)

## Need to re-attach coordinates using RandPoints
RandPts<-read.csv("Output/RandLocsForGEE.csv")

NDSI<-merge(NDSI,RandPts, by="PtID")
NDSI<-NDSI[order(NDSI$PtID, NDSI$system.index),]

# Add date and year column
NDSI$date<-substr(NDSI$system.index,1,10)
NDSI$year<-substr(NDSI$system.index,1,4)

## Quick plot
OnePoint<-subset(NDSI,PtID==410)

plot(OnePoint$doy,OnePoint$NDSI)


# Subset random locations by year - loop

# Extract the NDSI = 0 date for each point in each year

## The correct bands to use are: 4,7,10,13,16,19,22. Start at 4 and add three for each subsequent year.
Years<-unique(NDSI$year)
t<-4
j<-1
for(i in Years){
  ## Read in NDSI data
  NDSIrast<-raster("Inputs/Landcover/NDSI-snow-off.tif",band=t)
  ## Reproject to UTM
  NDSIrp<-projectRaster(NDSIrast, crs = '+proj=utm +zone=21 ellps=WGS84')
  
  ## Subset the data
  subData<-subset(NDSI,year==i)
  
  ## Extract data and add to dataframe
  subData$SnowOff<-extract(NDSIrp, data.frame(subData$x,subData$y))
  
  if(j==1){
    NewData<-subData
  }else{
    NewData<-rbind(NewData,subData)
  }
  t<-t+3
  j<-j+1
  print(i)
}


# Substract day peak NDSI from doy to get "days from peak"
NewData$DeltaSnowOff<-NewData$doy-NewData$SnowOff

## Second loop

## First do for all herds

t<-1
#SnowOff<-data.frame()
for (i in c(-35:25)){
  sub<-subset(NewData,DeltaSnowOff==i)
  NDSIval<-mean(sub$NDSI,na.rm=T)/10000
  NDSIsd<-sd(sub$NDSI,na.rm=T)/10000
  NewD<-data.frame(i,NDSIval,NDSIsd)
  if (t==1){
    SnowOff<-NewD
  }else{
    SnowOff<-rbind(SnowOff,NewD)
  }
  t<-t+1
  print(i)
}

colnames(SnowOff)<-c("DaysToSnowOff","NDSI","NDSIsd")

SnowOff$FRA<-0.06+1.21*(SnowOff$NDSI)
SnowOff$FRAsd<-0.06+1.21*(SnowOff$NDSIsd)
SnowOff$FRAup<-SnowOff$FRA+SnowOff$FRAsd
SnowOff$FRAlow<-SnowOff$FRA-SnowOff$FRAsd

SnowOff$FRAup[SnowOff$FRAup>1] <- 1
SnowOff$FRAup[SnowOff$FRAup<0] <- 0
SnowOff$FRAlow[SnowOff$FRAlow<0] <- 0
SnowOff$FRA[SnowOff$FRA<0]<-0


## Redo with an outer loop to subset by herds
herds<-unique(NewData$Herd.x)
t<-1
for (j in herds){
  sub.h<-subset(NewData,Herd.x==j)
  print(j)
for (i in c(-35:25)){
  sub<-subset(sub.h,DeltaSnowOff==i)
  NDSIval<-mean(sub$NDSI,na.rm=T)/10000
  NDSIsd<-sd(sub$NDSI,na.rm=T)/10000
  NewD<-data.frame(j,i,NDSIval,NDSIsd)
  if (t==1){
    SnowOff_herd<-NewD
  }else{
    SnowOff_herd<-rbind(SnowOff_herd,NewD)
  }
  t<-t+1
  print(i)
}
}

colnames(SnowOff_herd)<-c("Herd","DaysToSnowOff","NDSI","NDSIsd")

SnowOff_herd$FRA<-0.06+1.21*(SnowOff_herd$NDSI)
SnowOff_herd$FRAsd<-0.06+1.21*(SnowOff_herd$NDSIsd)
SnowOff_herd$FRAup<-SnowOff_herd$FRA+SnowOff_herd$FRAsd
SnowOff_herd$FRAlow<-SnowOff_herd$FRA-SnowOff_herd$FRAsd


SnowOff_herd$FRAlow[SnowOff_herd$FRAlow<0] <- 0
SnowOff_herd$FRA[SnowOff_herd$FRA<0]<-0


#### Figure S3-1: Fractional snow off by herd through time

png("graphics/NDSIsupFig2.png", width=7,height=7, units="in",res=300)
plot(SnowOff$DaysToSnowOff,SnowOff$FRA,type="l",lwd=3,xlab="Days to snow off (first recorded negative NDSI value)",
     ylab="Fractional snow cover",ylim=c(0,1))
lines(SnowOff$DaysToSnowOff,SnowOff$FRAup,type="l",lty=2)
lines(SnowOff$DaysToSnowOff,SnowOff$FRAlow,type="l",lty=2)
herd.lwd<-1
abline(v=-7.04, lwd=3)
abline(v=-6.3, lwd=herd.lwd, col="blue")
abline(v=-8.39, lwd=herd.lwd, col="red")
abline(v=-17.23, lwd=herd.lwd, col="green")
abline(v=1.41, lwd=herd.lwd, col="orange")
abline(v=-3.08, lwd=herd.lwd, col="cyan")
lines(subset(SnowOff_herd,Herd=="BUCHANS")$DaysToSnowOff,
      subset(SnowOff_herd,Herd=="BUCHANS")$FRA,col="blue")
lines(subset(SnowOff_herd,Herd=="GREY")$DaysToSnowOff,
      subset(SnowOff_herd,Herd=="GREY")$FRA,col="red")
lines(subset(SnowOff_herd,Herd=="LAPOILE")$DaysToSnowOff,
      subset(SnowOff_herd,Herd=="LAPOILE")$FRA,col="green")
lines(subset(SnowOff_herd,Herd=="MIDRIDGE")$DaysToSnowOff,
      subset(SnowOff_herd,Herd=="MIDRIDGE")$FRA,col="orange")
lines(subset(SnowOff_herd,Herd=="TOPSAILS")$DaysToSnowOff,
      subset(SnowOff_herd,Herd=="TOPSAILS")$FRA,col="cyan")
legend(x=5,y=1,lwd=c(3,rep(herd.lwd,5)),col=c("black","blue","red","green","orange","cyan"),
       legend=c("All herds","Buchans","Grey River","Lapoile","Middle Ridge","Topsails"))
dev.off()


#### Table of fractional snow-off dates

densitiesAll<-read.csv("Output/Results/Densities.csv")

herd<-densitiesAll$X
SurfInd<-densitiesAll$densitiesMigSO

All<-subset(SnowOff, DaysToSnowOff==round(SurfInd[1],0))
Buch<-subset(SnowOff_herd,Herd=="BUCHANS" & DaysToSnowOff==round(SurfInd[2],0))
Grey<-subset(SnowOff_herd,Herd=="GREY" & DaysToSnowOff==round(SurfInd[3],0))
Lapo<-subset(SnowOff_herd,Herd=="LAPOILE" & DaysToSnowOff==round(SurfInd[4],0))
MidR<-subset(SnowOff_herd,Herd=="MIDRIDGE" & DaysToSnowOff==round(SurfInd[5],0))
Tops<-subset(SnowOff_herd,Herd=="TOPSAILS" & DaysToSnowOff==round(SurfInd[6],0))

NDSIdata<-c(All$NDSI,Buch$NDSI,Grey$NDSI,Lapo$NDSI,MidR$NDSI,Tops$NDSI)
NDSIdatasd<-c(All$NDSIsd,Buch$NDSIsd,Grey$NDSIsd,Lapo$NDSIsd,MidR$NDSI,Tops$NDSIsd)

FRAdata<-c(All$FRA,Buch$FRA,Grey$FRA,Lapo$FRA,MidR$FRA,Tops$FRA)
FRAdatasd<-c(All$FRAsd,Buch$FRAsd,Grey$FRAsd,Lapo$FRAsd,MidR$FRA,Tops$FRAsd)

NDSIsum<-paste(round(NDSIdata,2)," (",round(NDSIdatasd,2),")",sep="")
FRAsum<-paste(round(FRAdata,2)," (",round(FRAdatasd,2),")",sep="")

NDSIsupTable<-data.frame(herd,SurfInd,NDSIsum,FRAsum)

write.csv(NDSIsupTable, "Output/SnowOffS3Table.csv")




##################################### END #######################################

