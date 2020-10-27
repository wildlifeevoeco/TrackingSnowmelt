############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# February 22nd, 2020 ##########################################################################

############# Script 2: Appending migration data from MM ###################################################

############################################################################################################
############################################################################################################

library(data.table)
library(lubridate)

### Read in parturition data and migration start/end times

Mig<-read.csv("Output/MigrationMapper/tabSixOutputs/migtime.csv")

### Need to append herd data to MM output for join. Join using main data:

data<-readRDS("Output/Locs/CleanData.RDS")
## Make field for joining
data$id_yr<-paste(data$ID,data$year,sep="_")

## Remove individuals migrate of where data was insufficient
Mig<-subset(Mig,springMig==1 & notes!="nm")

### Clean it up a bit:
Mig$nsdYear<-NULL
Mig$startFall<-NULL
Mig$endFall<-NULL
Mig$springMig<-NULL
Mig$fallMig<-NULL
Mig$notes<-NULL
Mig$moveType<-NULL
Mig$fallMigDst<-NULL
Mig$classifiedBy<-NULL
Mig$X<-NULL


### Adding in Julian date versions of all dates
Mig$StartMig<-yday(Mig$startSpring)
Mig$EndMig<-yday(Mig$endSpring)


### Remove original date columns
Mig$startSpring<-NULL
Mig$endSpring<-NULL
Mig$newUid<-NULL



data<-merge(data,Mig,by="id_yr")

data$Month<-NULL
data$Day<-NULL
data$Hour<-NULL
data$Minute<-NULL
data$Second<-NULL
data$burst<-data$id_yr
data$id_yr<-NULL

## Add in the season

data$season<-as.factor(ifelse(data$JDay<data$StartMig,"Pre-Mig",
                    ifelse(data$JDay<data$EndMig,"Mig",
                           ifelse(data$JDay<data$CalfDate,"Between",
                                  ifelse(data$JDay<=data$EndCalf,"Calving","Post-calving")))))

## Order the data
data<-data[order(data$Herd,data$ID,data$Time),]

## Save the data
saveRDS(data,"Output/Locs/CleanDataSeasonal.RDS")


################### END ########################