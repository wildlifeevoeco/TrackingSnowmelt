############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# Collated August 26th, 2020 ###################################################################

############# Script S01: Appendix 1 code ##################################################################

############################################################################################################
############################################################################################################

##### Figure S01-2: Phenology of example pixel

################  Appendix - IRG and snow-off profile for a representative pixel

library(raster)
library(nlme)
library(lme4)
library(zoo)
library(mosaic)
library(minpack.lm)

NDSI<-read.csv("Output/GEE/GEE_NDSI.csv")

Pt_id<-1745

##1745, 2011
sub<-subset(NDSI,PtID==Pt_id & year==2011)

plot(sub$NDSI_Snow_Cover~sub$doy)

## Fit the curve to the NDVI

th1<-1
th2<-coef(lm(sub$NDSI_Snow_Cover~sub$doy))[1]
th3<-coef(lm(sub$NDSI_Snow_Cover~sub$doy))[2]

model<-nlsLM(NDSI_Snow_Cover ~ theta1/(1 + exp(-(theta2 + theta3*doy))),
             start=list(theta1 = 1, theta2 = 130, 
                        theta3 = -0.78),
             control = list(maxiter = 40000000),data=sub)

Preds<-predict(model,newdata = data.frame(doy = c(40:200)))

coords<-coordinates(sub$.geo)[1]

Rand<-read.csv("Output/RandLocsForGEE.csv")
head(Rand)

RandSub<-subset(Rand,PtID==Pt_id)

### Extract the NDVI

setwd('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI/NDVI/2011')
Files2011<-list.files('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI/NDVI/2011')
NDVIras2011<-sapply(Files2011, raster)

t<-1
for (i in 40:275){
  if(i<100){
    NDVI<-extract(x=eval(parse(text=paste("NDVIras2011$Pred20110",i,".tif",sep=""))),
                  y=data.frame(RandSub$x,RandSub$y))
  }else{
    NDVI<-extract(x=eval(parse(text=paste("NDVIras2011$Pred2011",i,".tif",sep=""))),
                  y=data.frame(RandSub$x,RandSub$y))
    
  }
  if(t==1){
    NDVIAll<-NDVI
  }else{
    NDVIAll<-c(NDVIAll,NDVI)
  }
  t<-t+1
  print(i)
}


### Extract the IRG

setwd('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI/IRG/2011')
Files2011<-list.files('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI/IRG/2011')
IRG2011<-sapply(Files2011, raster)

t<-1
for (i in 40:275){
  if(i<100){
    IRG<-extract(x=eval(parse(text=paste("IRG2011$IRG20110",i,".tif",sep=""))),
                 y=data.frame(RandSub$x,RandSub$y))
  }else{
    IRG<-extract(x=eval(parse(text=paste("IRG2011$IRG2011",i,".tif",sep=""))),
                 y=data.frame(RandSub$x,RandSub$y))
    
  }
  if(t==1){
    IRGAll<-IRG
  }else{
    IRGAll<-c(IRGAll,IRG)
  }
  t<-t+1
  print(i)
}


#### Extract NDVI with no flooring

Days<-seq(41,273,by=8)

setwd('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfit/2011')
Files2011<-list.files('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfit/2011')
NDVI2011uf<-sapply(Files2011, raster)

t<-1
for (i in Days){
  if(i<100){
    NDVI_o<-extract(x=eval(parse(text=paste("NDVI2011uf$FinalM20110",i,"UTM.tif",sep=""))),
                    y=data.frame(RandSub$x,RandSub$y))
  }else{
    NDVI_o<-extract(x=eval(parse(text=paste("NDVI2011uf$FinalM2011",i,"UTM.tif",sep=""))),
                    y=data.frame(RandSub$x,RandSub$y))
    
  }
  if(t==1){
    NDVI_uf<-NDVI_o
  }else{
    NDVI_uf<-c(NDVI_uf,NDVI_o)
  }
  t<-t+1
  print(i)
}


### Pull out the values for flooring the data and date of peak IRG

NDVI_min<-raster("C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/LowerNDVI.tif")
NDVI_max<-raster("C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/UpperNDVI.tif")

PeakIRG<-raster("C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/PeakIRG/PeakIRG2011.tif")  

LowVal<-extract(x=NDVI_min,y=data.frame(RandSub$x,RandSub$y))
UppVal<-extract(x=NDVI_max,y=data.frame(RandSub$x,RandSub$y))
SnowOffVal<-120
PeakIRGVal<-extract(x=PeakIRG,y=data.frame(RandSub$x,RandSub$y))


NDVIFL<-ifelse(NDVI_uf<LowVal,LowVal,NDVI_uf)

## Make the figure

png("C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Figures/FigS1-2.png",width=5,height=5,units="in",res=300)

par(mar=c(3.2,3.2,1,2))
plot(sub$NDSI_Snow_Cover/100~sub$doy, xlab=NA,ylab=NA,xaxt='n',xlim=c(40,220),col="blue",pch=20,ylim=c(0,1))
mtext(side=1, "Date",padj=2)
mtext(side=2, "Proportion snow cover - scaled NDVI and IRG",padj=-2.8)#,col="blue")
axis(1,at=c(0,32,61,92,122,153,183,214,245,275,306,336,365),labels=NA)
months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1)
lines(Preds/100~c(40:200),col="blue",lwd=3)
arrows(x0=SnowOffVal,x1=SnowOffVal,y0=0,y1=0.05,angle=0,col="blue",lwd=3)
arrows(x0=PeakIRGVal,x1=PeakIRGVal,y0=0,y1=0.05,angle=0,col="forestgreen",lwd=3)
par(new=T)
plot(NDVIAll~c(40:275),type="l",xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(40,220),
     col="forestgreen",lwd=3)
par(new=T)
plot(IRGAll~c(40:275),type="l",xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(40,220),
     col="forestgreen",lwd=3,lty=2)
#mtext(side=4,"Relative NDVI (solid) and IRG (dashed)",col="forestgreen")
par(new=T)
plot(NDVIFL~Days,xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(40,220),col="forestgreen",ylim=c(LowVal,UppVal),pch=20)
legend(legend=c("Snow cover","NDVI","IRG"),lty=c(1,1,2),col=c("blue","forestgreen","forestgreen"),
       lwd=3,x=40,y=5500,bty="n",cex=0.8)

dev.off()


### Version 2: IRG fit to unfloored data (for response to R2)


Pt_id<-1745

##1745, 2011
sub<-subset(NDSI,PtID==Pt_id & year==2011)

plot(sub$NDSI_Snow_Cover~sub$doy)

th1<-1
th2<-coef(lm(sub$NDSI_Snow_Cover~sub$doy))[1]
th3<-coef(lm(sub$NDSI_Snow_Cover~sub$doy))[2]

lm(sub$NDSI_Snow_Cover~sub$doy)

model<-nlsLM(NDSI_Snow_Cover ~ theta1/(1 + exp(-(theta2 + theta3*doy))),
             start=list(theta1 = 1, theta2 = 130, 
                        theta3 = -0.78),
             control = list(maxiter = 40000000),data=sub)

Preds<-predict(model,newdata = data.frame(doy = c(40:200)))

coords<-coordinates(sub$.geo)[1]

str(sub)

Rand<-read.csv("Output/RandLocsForGEE.csv")
head(Rand)

RandSub<-subset(Rand,PtID==Pt_id)


setwd('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfloored/2011/NDVI')
Files2011<-list.files('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfloored/2011/NDVI')
NDVIras2011<-sapply(Files2011, raster)

t<-1
for (i in 40:275){
  if(i<100){
    NDVI<-extract(x=eval(parse(text=paste("NDVIras2011$Pred20110",i,".tif",sep=""))),
                  y=data.frame(RandSub$x,RandSub$y))
  }else{
    NDVI<-extract(x=eval(parse(text=paste("NDVIras2011$Pred2011",i,".tif",sep=""))),
                  y=data.frame(RandSub$x,RandSub$y))
    
  }
  if(t==1){
    NDVIAll<-NDVI
  }else{
    NDVIAll<-c(NDVIAll,NDVI)
  }
  t<-t+1
  print(i)
}



setwd('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfloored/2011/IRG')
Files2011<-list.files('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover_unfloored/OldNDVI/2011/IRG')
IRG2011<-sapply(Files2011, raster)

t<-1
for (i in 40:275){
  if(i<100){
    IRG<-extract(x=eval(parse(text=paste("IRG2011$IRG20110",i,".tif",sep=""))),
                 y=data.frame(RandSub$x,RandSub$y))
  }else{
    IRG<-extract(x=eval(parse(text=paste("IRG2011$IRG2011",i,".tif",sep=""))),
                 y=data.frame(RandSub$x,RandSub$y))
    
  }
  if(t==1){
    IRGAll<-IRG
  }else{
    IRGAll<-c(IRGAll,IRG)
  }
  t<-t+1
  print(i)
}


#### Extract unfloored NDVI

Days<-seq(41,273,by=8)

setwd('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfit/2011')
Files2011<-list.files('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfit/2011')
NDVI2011uf<-sapply(Files2011, raster)

t<-1
for (i in Days){
  if(i<100){
    NDVI_o<-extract(x=eval(parse(text=paste("NDVI2011uf$FinalM20110",i,"UTM.tif",sep=""))),
                    y=data.frame(RandSub$x,RandSub$y))
  }else{
    NDVI_o<-extract(x=eval(parse(text=paste("NDVI2011uf$FinalM2011",i,"UTM.tif",sep=""))),
                    y=data.frame(RandSub$x,RandSub$y))
    
  }
  if(t==1){
    NDVI_uf<-NDVI_o
  }else{
    NDVI_uf<-c(NDVI_uf,NDVI_o)
  }
  t<-t+1
  print(i)
}


PeakIRG<-raster("C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfloored/2011/PeakIRG2011.tif")  

SnowOffVal<-120
PeakIRGVal<-extract(x=PeakIRG,y=data.frame(RandSub$x,RandSub$y))


## Make a "no snow removed" version to show the difference

png("C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Figures/FigS4_V2.png",width=5,height=5,units="in",res=300)

par(mar=c(3.2,3.2,1,2))
plot(sub$NDSI_Snow_Cover/100~sub$doy, xlab=NA,ylab=NA,xaxt='n',xlim=c(40,220),col="blue",pch=20,ylim=c(0,1))
mtext(side=1, "Date",padj=2)
mtext(side=2, "Proportion snow cover - scaled NDVI and IRG",padj=-2.8)#,col="blue")
axis(1,at=c(0,32,61,92,122,153,183,214,245,275,306,336,365),labels=NA)
months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1)
lines(Preds/100~c(40:200),col="blue",lwd=3)
arrows(x0=SnowOffVal,x1=SnowOffVal,y0=0,y1=0.05,angle=0,col="blue",lwd=3)
arrows(x0=PeakIRGVal,x1=PeakIRGVal,y0=0,y1=0.05,angle=0,col="forestgreen",lwd=3)
par(new=T)
plot(NDVIAll~c(40:275),type="l",xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(40,220),
     col="forestgreen",lwd=3)
par(new=T)
plot(IRGAll~c(40:275),type="l",xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(40,220),
     col="forestgreen",lwd=3,lty=2)
#mtext(side=4,"Relative NDVI (solid) and IRG (dashed)",col="forestgreen")
par(new=T)
plot(NDVI_uf~Days,xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(40,220),col="forestgreen",pch=20)
legend(legend=c("Snow cover","NDVI","IRG"),lty=c(1,1,2),col=c("blue","forestgreen","forestgreen"),
       lwd=3,x=150,y=3500,bty="n",cex=0.8)

dev.off()



######### END Fig S1-2  ##############



######## Figure S1-3 #############

library(raster)
library(nlme)
library(lme4)
library(zoo)
library(mosaic)
library(minpack.lm)
library(drc)

NDSI<-read.csv("Output/GEE/GEE_NDSI_AllYears.csv")

NDSI$NDSIsc<-NDSI$NDSI/10000
NDSI$FRA<-0.06+1.21*(NDSI$NDSIsc)

herds<-unique(NDSI$Herd)
herds<-sort(herds)
herdnames<-c("A) Buchans","B) Grey River","C) Lapoile","D) Middle Ridge","E) Topsails")

png("Figures/FigS1-3-SnowCurvesbyHerd.png",height = 7, width = 5, units = "in", res=300)

par(mfrow=c(3,2),mar=c(1,1,1,1),oma=c(3,3,1,1))
hn<-1
for(h in herds){
  for(j in 2001:2020){
    NDSI_sub<-subset(NDSI,year==j & Herd==h)
    t<-1
    for(i in 45:200){
      mean<-mean(subset(NDSI_sub,doy==i)$FRA,na.rm=T)
      meandata<-data.frame(i,mean)
      if(t==1){
        data<-meandata
      }else{
        data<-rbind(data,meandata)
      }
      t<-t+1
      print(i)
    }
    colnames(data)<-c("doy","FRA")
    
    M1<-drm(FRA~doy,fct=L.3(),data=data)
    
    Preds<-predict(M1,newdata = data.frame(doy = c(40:200)))
    
    if(j==2001){
      
      plot(c(40:200),Preds,type="l", xlab=NA,ylab=NA,xaxt='n',xlim=c(40,220),ylim=c(0,1))
      axis(1,at=c(0,32,61,92,122,153,183,214,245,275,306,336,365),labels=NA)
      months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1)
      text(herdnames[hn],x=175,y=0.92)
      
    }else{
      if(h=="BUCHANS" & j>=2007 & j<=2012 |
         h=="MIDRIDGE" & j>=2010 & j<=2013 |
         h=="GREY" & j>=2007 & j<=2012 |
         h=="LAPOILE" & j>=2007 & j<=2012 |
         h=="TOPSAILS" & j>=2007 & j<=2011){
        lines(c(40:200),Preds,lwd=3,col="red")
      }else{
        lines(c(40:200),Preds)
      }
      
    }
  }
  hn<-hn+1
}

mtext(side=2, "Proportion snow cover",padj=-1.5,outer=T)
mtext(side=1, "Date",padj=2,outer=T)

dev.off()


##### code to export annual brownian bridge data for each individual for making Fig S1-4
##### (actual figure/map generated in arcMap)

############# Making BB HRs to export for making maps ##################

## Takes the random points and generates an MCP and exports the MCP as a shapefile
## for plotting consistency of migration across years.

library(adehabitatHR)
library(maptools)
library(raster)

## Read in the data
data<-readRDS("Output/Locs/LocsWSpatial.RDS")

## Take just the available points - drawn within 99.99% BB corridor in step 3
Avail<-subset(data,Pres==0 & season=="Mig")


### Loop to make shapefiles for each herd-year
Avail$HerdYear<-paste(Avail$Herd,Avail$year,sep="")
Herdyears<-unique(Avail$HerdYear)


CRSproj<-"+proj=utm +zone=21 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
CSize=500

for(i in Herdyears){
  
  ### Subset to year and herd
  Sub<-subset(Avail,HerdYear==i)
  
  ### Set up the data for kernelUD
  id<-data.frame(rep(1,nrow(Sub)))
  xy<-cbind(Sub$x,Sub$y)
  coordinates(id)<-xy
  
  proj4string(id)<-CRS("+proj=utm +zone=21 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  ### Set up grid for kernel
  pts<-SpatialPoints(data.frame(Sub$x,Sub$y),proj4string = CRS(CRSproj))
  Ext<-extent(pts)
  
  xVec<-seq(Ext[1]-50000,Ext[2]+50000, by=CSize)
  yVec<-seq(Ext[3]-50000,Ext[4]+50000, by=CSize)
  
  xVec2<-rep(xVec,times=length(yVec))
  yVec2<-rep(yVec,each=length(xVec))
  
  pts<-SpatialPoints(data.frame(xVec2,yVec2),proj4string = CRS(CRSproj))
  sp<-SpatialPixels(points=pts)
  
  ## Make the kernel and the 99.9% HR
  kern<-kernelUD(id, grid=sp, extent=3, h=400)
  hr<-getverticeshr(kern, percent = 99.9)
  
  ## Set the projection
  proj4string(hr)<-CRS("+proj=utm +zone=21 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  ## Write out the shapefile
  shapefile(hr,paste("Output/SeasonalMigCors/",i,".shp",sep=""), overwrite=T)
  print(i)
}


###### Correlation between pixels and neighboring pixels (last section appendix 1)


library(raster)

### Read in random locations for sampling
NDSI<-read.csv("Output/RandLocsForGEE.csv")

### Vector of days for NDVI data
Days<-seq(41,273,by=8)

### Setwd and read in files
setwd('/Users/Mike/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfit/2011')
#setwd('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfit/2011')
Files2011<-list.files('/Users/Mike/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfit/2011')
#Files2011<-list.files('C:/Users/RBG_local/Dropbox/From RGB/SnowOffGreenUpdated/Inputs/Landcover/NDVI_unfit/2011')
NDVI2011uf<-sapply(Files2011, raster)

### Set up the focal matrix to sample neighbourhood cells
foc1<-rep(1/24,5)
foc2<-c(1/24,1/24,0,1/24,1/24)
focal<-data.frame(foc1,foc1,foc2,foc1,foc1)
focal2<-as.matrix(focal)

### Generate focal NDVI layer
focNDVI<-lapply(NDVI2011uf,function(x) focal(x,focal2,na.rm=T))

### Extract the values from the 5000 random locations to the NDVI in 2011, as
### well as the regional (surrounding 5 X 5 matrix excluding the focal cell) mean
### value of surrounding cells.

t<-1
for (i in Days){
  if(i<100){
    NDVIpt<-extract(x=eval(parse(text=paste("NDVI2011uf$FinalM20110",i,"UTM.tif",sep=""))),
                    y=data.frame(NDSI$x,NDSI$y))
    NDVIreg<-extract(x=eval(parse(text=paste("focNDVI$FinalM20110",i,"UTM.tif",sep=""))),
                     y=data.frame(NDSI$x,NDSI$y))
  }else{
    NDVIpt<-extract(x=eval(parse(text=paste("NDVI2011uf$FinalM2011",i,"UTM.tif",sep=""))),
                    y=data.frame(NDSI$x,NDSI$y))
    NDVIreg<-extract(x=eval(parse(text=paste("focNDVI$FinalM2011",i,"UTM.tif",sep=""))),
                     y=data.frame(NDSI$x,NDSI$y))
  }
  if(t==1){
    NDVI_uf<-NDVIpt
    NDVI_reg<-NDVIreg
  }else{
    NDVI_uf<-c(NDVI_uf,NDVIpt)
    NDVI_reg<-c(NDVI_reg,NDVIreg)
  }
  t<-t+1
  print(i)
}

plot(NDVI_uf~NDVI_reg)
cor(NDVI_uf,NDVI_reg,use="complete.obs")

### Pearson correlation coefficient = 0.9903
summary(lm(NDVI_uf~NDVI_reg))

### Adjusted R2 = 0.9808


##### For Methods: Quick script to quantify the number of pixels that are contaminated by clouds #####

NDVI_ter<-read.csv("Output/GEE/GEE_NDVI_dataTer.csv")
NDVI_aqu<-read.csv("Output/GEE/GEE_NDVI_dataAqua.csv")

NDVI_all<-rbind(NDVI_ter,NDVI_aqu)

nrow(subset(NDVI_all,SummaryQA==3))/nrow(subset(NDVI_all,SummaryQA!="NA"))

### 10.0% of pixels contaminated by cloud




############## END #########################3
