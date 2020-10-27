############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# February 21st, 2020 ##########################################################################

############# Script 1: cleaning the raw data and exporting for MM #########################################

############################################################################################################
############################################################################################################

## Read in the data

data<-read.csv("Inputs/Locs/AllCaribouDataRaw.csv")

## Packages

library(adehabitatLT)
library(data.table)
library(rgdal)
library(lubridate)

## Keep only needed herds, only GPS data, and only females:

data<-subset(data,HERD=="MIDRIDGE"|HERD=="TOPSAILS"|HERD=="GREY"|HERD=="LAPOILE"|HERD=="BUCHANS")
data<-subset(data,COLLAR_TYPE_CL=="GPS")
data<-subset(data,SEX=="F")

data<-droplevels(data)

## Remove unnecessary columns

data$X<-NULL
data$SPECIES<-NULL
data$SEX<-NULL
data$FIX_ID<-NULL
data$EPSG_CODE<-NULL
data$Map_Quality<-NULL
data$Fix_Time_Delta<-NULL
data$COLLAR_FILE_ID<-NULL
data$COLLAR_ID<-NULL
data$COLLAR_TYPE_CL<-NULL
data$EXCLUDE<-NULL
data$VENDOR_CL<-NULL
data$AGE<-NULL
data$DOP<-NULL
data$NAV<-NULL
data$VALIDATED<-NULL
data$LOCQUAL<-NULL

utm21N <- '+proj=utm +zone=21 ellps=WGS84'
locs<-project(cbind(data$X_COORD, data$Y_COORD),utm21N)
coords<-coordinates(locs)

data<-data.frame(data,locs)
data$date<-as.POSIXct(paste(data$FIX_DATE, data$FIX_TIME, sep = ' '), format="%Y-%m-%d %H:%M:%S", tz="UTC")

data$FIX_DATE<-NULL
data$FIX_TIME<-NULL
data$X_COORD<-NULL
data$Y_COORD<-NULL

colnames(data)<-c("ID","Herd","Easting","Northing","Date")

data<-subset(data,Easting>0 & Easting<800000)
data<-subset(data,Northing>5200000 & Northing<6000000)

## Add in date and year column
data$year<-as.factor(year(data$Date))
data$day<-yday(data$Date)

data<-droplevels(data)

## Add in ID year field
data$IDyear<-paste(data$ID,data$year, sep="_")

length(unique(data$IDyear))
length(unique(data$ID))

## Remove data that doesn't include spring

NewData<-data.table(data) ## Make it a data table
NewData[, MinDay := min(day), by = .(IDyear)] ## Minumum day
NewData[, MaxDay := max(day), by = .(IDyear)] ## Minumum day

head(NewData)

## Remove ID years that have no data from start of April to mid-May. Remove individuals that 
## that have no data in the migration/calving season.
NewData2<-subset(NewData,MinDay<91 & MaxDay>135)

length(unique(NewData2$IDyear)) ### 326
length(unique(NewData2$ID))     ### 112

data<-NewData2


## Add in the parturition data
Part<-read.csv("Inputs/Parturition/AllHerdsSurvival2hrMR.csv")
nrow(Part)
Part$Lost<-NULL
Part$LossDate<-NULL
Part$CalfDate<-yday(Part$CalvingDate)
Part$EndCalf<-Part$CalfDate+21

## New ID-year field for merging
data$IDyear<-paste(data$Herd,data$IDyear,sep="_")

## Merge them
data<-merge(data,Part,by="IDyear")

length(unique(data$IDyear))
length(unique(data$ID))

## Subset to between Feb 1st and end of calving
data<-subset(data,day>31 & day<=EndCalf)
data$SLength<-data$EndCalf-31

#### Removing ID-years with insufficient data

NewD<-data.table(data) ## Make it a data table

NewD[, Counts := .N, by = .(IDyear)] ## Column representing the number of locations per ID*Year

NewD$PtsPerDay<-NewD$Counts/NewD$SLength ## Mean number of Locs per day for each ID*Year

## Some summary stats
median(NewD$PtsPerDay)
range(NewD$PtsPerDay)


NewD[, dayCount := length(Easting), by = .(IDyear, day)]## Number of fixes per day (necessary??)

length(unique(NewD$IDyear))### 350 ID-years
length(unique(NewD$ID))

i<-4 ## Threshold value for minimum number of mean fixes per day to retain 
j<-0.9 ### Threshold value for proportion of days that must have at least one fix to retain


NewD2<-NewD[PtsPerDay > i]# Subset to only include individuals with a mean of at least i points per day
length(unique(NewD2$IDyear))  ## Down to 318 ID*Year combinations
NewD2$IDYearDay<-paste(NewD2$IDyear,NewD2$day, sep="") ## Make a new Year*ID*Julian Day column
NewD2[, datDay := length(unique(IDYearDay)), by = .(IDyear)] ## Calculate the number of days in which there is data for each Year*ID
NewD2$PropDataDays<-NewD2$datDay/(trunc(NewD2$SLength)) ## Calculate the proportion of days in which there is data for each Year*ID
DataEdited<-NewD2[PropDataDays>=j] ## Remove all ID*Year combos for which the proportion of days with at least one fix is < j
(1-(length(unique(NewD$IDyear))-length(unique(DataEdited$IDyear)))/length(unique(NewD$IDyear)))*100 ##84 Percent data retained 

## reset data to edited version
data<-DataEdited

length(unique(data$IDyear))### 294 ID-years
length(unique(data$ID))### 103

## Add POSIXct time field
data$Time<-as.POSIXct(data$Date)

##Dealing with error - non-unique dates for given burst (duplicate rows?)
##Add in a "burst date" field
data$IDyear_time<-paste(data$IDyear,data$Time)

## Remove duplicated ID*year*Time
data<-data[!duplicated(data$IDyear_time),]

data<-droplevels(data)

## Make an LTRAJ object to get net squared displacement (NSD)
Traj<-as.ltraj(xy=data.frame(data$Easting,data$Northing), date=data$Time, 
               id=data$ID, burst=data$IDyear, 
               proj4string = CRS(utm21N))

## Turn the ltraj back to df and add back the year and julian day fields

data2<-ld(Traj)
data2$IDyear_time<-paste(data2$burst,data2$date)

## Re-maerge LTRAJ data with original data
data3<-merge(data,data2,by="IDyear_time")

data<-data3

data$displace<-sqrt(data$R2n)/1000

### A bit more cleaning...get rid of spatial outliers
### First make a displacement/time column (km/h)

data$movRate<-(data$dist/1000)/(data$dt/3600)

data<-subset(data, movRate<12)


## How far did each animal migrate in each year?
data<-setDT(data)
Totaldisp<-data[,max(displace), by = burst]

mean(Totaldisp$V1)## Mean migration distance = 71.9 km

#### Proportion of IDyears that are retained when dropping IDyears where migration distance is less
#### than "minDisp" threshold set at the start of the script
nrow(subset(Totaldisp, V1>20))/nrow(Totaldisp) ## At 20, = 97% retained

#### Subset out those non-migrants
data[, MaxDisp := max(displace), by = burst]
data<-subset(data,MaxDisp>20) ### Down to 286 ID-years

## Add in decimal day field to be a bit more precise in subsetting:
## Is there no native function in lubridate for this?!

data$JDateTime<-data$day+(hour(data$date)+minute(data$date)/60+second(data$date)/3600)/24

## Remove unnecessary columns

data$IDyear_time<-NULL
data$Date<-NULL
data$JDay<-data$day
data$day<-NULL
data$NumYear<-NULL
data$Easting<-NULL
data$Northing<-NULL
data$date<-NULL
data$dx<-NULL
data$dy<-NULL
data$dist<-NULL
data$dt<-NULL
data$R2n<-NULL
data$abs.angle<-NULL
data$rel.angle<-NULL
data$id<-NULL
data$IDyear<-NULL
data$pkey<-NULL
data$displace<-NULL
data$movRate<-NULL
data$MaxDisp<-NULL
data$X<-NULL
data$CalvingDate<-NULL
data$Counts<-NULL
data$PtsPerDay<-NULL
data$dayCount<-NULL
data$IDYearDay<-NULL
data$datDay<-NULL
data$PropDataDays<-NULL
data$Calved<-NULL
data$SLength<-NULL

data$Month<-month(data$Time)
data$Day<-day(data$Time)
data$Hour<-hour(data$Time)
data$Minute<-minute(data$Time)
data$Second<-second(data$Time)

head(data)

saveRDS(data,"Output/Locs/CleanData.RDS")

### Keep only up to parturition for MM:

dataMM<-subset(data, JDay<=CalfDate)

write.csv(dataMM, "Output/Locs/CleanDataMM.csv")

################### END ########################

