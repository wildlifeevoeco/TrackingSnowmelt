############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# February 24th, 2020 ##########################################################################

############# Script 8: Summary stats ######################################################################

############################################################################################################
############################################################################################################

library(data.table)

data<-readRDS("Output/Locs/LocsWSpatial_sub.RDS")

#unique(data$burst)   ### 216 ID-years
#unique(data$ID)      ### 94 IDs
#summary(data)
data$MigDur<-data$EndMig-data$StartMig

##### Making summary data table
Migr<-data.table(subset(data,season=="Mig"))
Calv<-data.table(subset(data,season=="Calving"))

BuchMigr<-subset(Migr, Herd=="BUCHANS")
GreyMigr<-subset(Migr, Herd=="GREY")
LapoMigr<-subset(Migr, Herd=="LAPOILE")
MidRMigr<-subset(Migr, Herd=="MIDRIDGE")
TopsMigr<-subset(Migr, Herd=="TOPSAILS")

BuchCalv<-subset(Calv, Herd=="BUCHANS")
GreyCalv<-subset(Calv, Herd=="GREY")
LapoCalv<-subset(Calv, Herd=="LAPOILE")
MidRCalv<-subset(Calv, Herd=="MIDRIDGE")
TopsCalv<-subset(Calv, Herd=="TOPSAILS")


NumIDs<-c(length(unique(Migr$ID)),
          length(unique(BuchMigr$ID)),
          length(unique(GreyMigr$ID)),
          length(unique(LapoMigr$ID)),
          length(unique(MidRMigr$ID)),
          length(unique(TopsMigr$ID)))

NumIDYears<-c(length(unique(Migr$burst)),
              length(unique(BuchMigr$burst)),
              length(unique(GreyMigr$burst)),
              length(unique(LapoMigr$burst)),
              length(unique(MidRMigr$burst)),
              length(unique(TopsMigr$burst)))

StartMigM<-c(median(Migr[,median(StartMig),by=burst]$V1),
             median(BuchMigr[,median(StartMig),by=burst]$V1),
             median(GreyMigr[,median(StartMig),by=burst]$V1),
             median(LapoMigr[,median(StartMig),by=burst]$V1),
             median(MidRMigr[,median(StartMig),by=burst]$V1),
             median(TopsMigr[,median(StartMig),by=burst]$V1))

StartMigMmin<-c(min(Migr[,median(StartMig),by=burst]$V1),
                min(BuchMigr[,median(StartMig),by=burst]$V1),
                min(GreyMigr[,median(StartMig),by=burst]$V1),
                min(LapoMigr[,median(StartMig),by=burst]$V1),
                min(MidRMigr[,median(StartMig),by=burst]$V1),
                min(TopsMigr[,median(StartMig),by=burst]$V1))

StartMigMmax<-c(max(Migr[,median(StartMig),by=burst]$V1),
                max(BuchMigr[,median(StartMig),by=burst]$V1),
                max(GreyMigr[,median(StartMig),by=burst]$V1),
                max(LapoMigr[,median(StartMig),by=burst]$V1),
                max(MidRMigr[,median(StartMig),by=burst]$V1),
                max(TopsMigr[,median(StartMig),by=burst]$V1))

StartMigMform<-format(as.Date(StartMigM,origin = "1899-12-31"), "%b %d")
StartMigMminform<-format(as.Date(StartMigMmin,origin = "1899-12-31"), "%b %d")
StartMigMmaxform<-format(as.Date(StartMigMmax,origin = "1899-12-31"), "%b %d")

MigrationStart<-paste(StartMigMform, " [",StartMigMminform, ", ",StartMigMmaxform,"]", sep="")


EndMigM<-c(median(Migr[,median(EndMig),by=burst]$V1),
           median(BuchMigr[,median(EndMig),by=burst]$V1),
           median(GreyMigr[,median(EndMig),by=burst]$V1),
           median(LapoMigr[,median(EndMig),by=burst]$V1),
           median(MidRMigr[,median(EndMig),by=burst]$V1),
           median(TopsMigr[,median(EndMig),by=burst]$V1))

EndMigMmin<-c(min(Migr[,median(EndMig),by=burst]$V1),
              min(BuchMigr[,median(EndMig),by=burst]$V1),
              min(GreyMigr[,median(EndMig),by=burst]$V1),
              min(LapoMigr[,median(EndMig),by=burst]$V1),
              min(MidRMigr[,median(EndMig),by=burst]$V1),
              min(TopsMigr[,median(EndMig),by=burst]$V1))

EndMigMmax<-c(max(Migr[,median(EndMig),by=burst]$V1),
              max(BuchMigr[,median(EndMig),by=burst]$V1),
              max(GreyMigr[,median(EndMig),by=burst]$V1),
              max(LapoMigr[,median(EndMig),by=burst]$V1),
              max(MidRMigr[,median(EndMig),by=burst]$V1),
              max(TopsMigr[,median(EndMig),by=burst]$V1))

EndMigMform<-format(as.Date(EndMigM,origin = "1899-12-31"), "%b %d")
EndMigMminform<-format(as.Date(EndMigMmin,origin = "1899-12-31"), "%b %d")
EndMigMmaxform<-format(as.Date(EndMigMmax,origin = "1899-12-31"), "%b %d")

MigrationEnd<-paste(EndMigMform, " [",EndMigMminform, ", ",EndMigMmaxform,"]", sep="")


MigDuration<-c(median(Migr[,median(MigDur),by=burst]$V1),
               median(BuchMigr[,median(MigDur),by=burst]$V1),
               median(GreyMigr[,median(MigDur),by=burst]$V1),
               median(LapoMigr[,median(MigDur),by=burst]$V1),
               median(MidRMigr[,median(MigDur),by=burst]$V1),
               median(TopsMigr[,median(MigDur),by=burst]$V1))

MigDurationMin<-c(min(Migr[,median(MigDur),by=burst]$V1),
                  min(BuchMigr[,median(MigDur),by=burst]$V1),
                  min(GreyMigr[,median(MigDur),by=burst]$V1),
                  min(LapoMigr[,median(MigDur),by=burst]$V1),
                  min(MidRMigr[,median(MigDur),by=burst]$V1),
                  min(TopsMigr[,median(MigDur),by=burst]$V1))

MigDurationMax<-c(max(Migr[,median(MigDur),by=burst]$V1),
                  max(BuchMigr[,median(MigDur),by=burst]$V1),
                  max(GreyMigr[,median(MigDur),by=burst]$V1),
                  max(LapoMigr[,median(MigDur),by=burst]$V1),
                  max(MidRMigr[,median(MigDur),by=burst]$V1),
                  max(TopsMigr[,median(MigDur),by=burst]$V1))

MigrationDuration<-paste(round(MigDuration,1), " [",round(MigDurationMin,1), ", ",round(MigDurationMax,1),"]", sep="")

MigDistance<-c(median(Migr[,median(SpringMigDst),by=burst]$V1),
               median(BuchMigr[,median(SpringMigDst),by=burst]$V1),
               median(GreyMigr[,median(SpringMigDst),by=burst]$V1),
               median(LapoMigr[,median(SpringMigDst),by=burst]$V1),
               median(MidRMigr[,median(SpringMigDst),by=burst]$V1),
               median(TopsMigr[,median(SpringMigDst),by=burst]$V1))/1000

MigDistanceMin<-c(min(Migr[,median(SpringMigDst),by=burst]$V1),
                  min(BuchMigr[,median(SpringMigDst),by=burst]$V1),
                  min(GreyMigr[,median(SpringMigDst),by=burst]$V1),
                  min(LapoMigr[,median(SpringMigDst),by=burst]$V1),
                  min(MidRMigr[,median(SpringMigDst),by=burst]$V1),
                  min(TopsMigr[,median(SpringMigDst),by=burst]$V1))/1000

MigDistanceMax<-c(max(Migr[,median(SpringMigDst),by=burst]$V1),
                  max(BuchMigr[,median(SpringMigDst),by=burst]$V1),
                  max(GreyMigr[,median(SpringMigDst),by=burst]$V1),
                  max(LapoMigr[,median(SpringMigDst),by=burst]$V1),
                  max(MidRMigr[,median(SpringMigDst),by=burst]$V1),
                  max(TopsMigr[,median(SpringMigDst),by=burst]$V1))/1000

MigrationDistance<-paste(round(MigDistance,1), " [",round(MigDistanceMin,1), ", ",round(MigDistanceMax,1),"]", sep="")


SnowOffmedian<-c(median(subset(Migr,Pres=="0")$SnowOff),
               median(subset(Migr,Herd=="BUCHANS" & Pres=="0")$SnowOff),
               median(subset(Migr,Herd=="GREY" & Pres=="0")$SnowOff),
               median(subset(Migr,Herd=="LAPOILE" & Pres=="0")$SnowOff),
               median(subset(Migr,Herd=="MIDRIDGE" & Pres=="0")$SnowOff),
               median(subset(Migr,Herd=="TOPSAILS" & Pres=="0")$SnowOff))

SnowOffSD<-c(sd(subset(Migr,Pres=="0")$SnowOff),
             sd(subset(Migr,Herd=="BUCHANS" & Pres=="0")$SnowOff),
             sd(subset(Migr,Herd=="GREY" & Pres=="0")$SnowOff),
             sd(subset(Migr,Herd=="LAPOILE" & Pres=="0")$SnowOff),
             sd(subset(Migr,Herd=="MIDRIDGE" & Pres=="0")$SnowOff),
             sd(subset(Migr,Herd=="TOPSAILS" & Pres=="0")$SnowOff))


SnowOffmedianform<-format(as.Date(SnowOffmedian,origin = "1899-12-31"), "%b %d")

SnowOffRange<-paste(SnowOffmedianform, " [",round(SnowOffSD,1), " days]", sep="")


CalvingM<-c(median(Migr[,median(CalfStart),by=burst]$V1),
            median(BuchMigr[,median(CalfStart),by=burst]$V1),
            median(GreyMigr[,median(CalfStart),by=burst]$V1),
            median(LapoMigr[,median(CalfStart),by=burst]$V1),
            median(MidRMigr[,median(CalfStart),by=burst]$V1),
            median(TopsMigr[,median(CalfStart),by=burst]$V1))

Calvingmin<-c(min(Migr[,median(CalfStart),by=burst]$V1),
              min(BuchMigr[,median(CalfStart),by=burst]$V1),
              min(GreyMigr[,median(CalfStart),by=burst]$V1),
              min(LapoMigr[,median(CalfStart),by=burst]$V1),
              min(MidRMigr[,median(CalfStart),by=burst]$V1),
              min(TopsMigr[,median(CalfStart),by=burst]$V1))

Calvingmax<-c(max(Migr[,median(CalfStart),by=burst]$V1),
              max(BuchMigr[,median(CalfStart),by=burst]$V1),
              max(GreyMigr[,median(CalfStart),by=burst]$V1),
              max(LapoMigr[,median(CalfStart),by=burst]$V1),
              max(MidRMigr[,median(CalfStart),by=burst]$V1),
              max(TopsMigr[,median(CalfStart),by=burst]$V1))

CalvingMform<-format(as.Date(CalvingM,origin = "1899-12-31"), "%b %d")
Calvingminform<-format(as.Date(Calvingmin,origin = "1899-12-31"), "%b %d")
Calvingmaxform<-format(as.Date(Calvingmax,origin = "1899-12-31"), "%b %d")

CalvingRange<-paste(CalvingMform, " [",Calvingminform, ", ",Calvingmaxform,"]", sep="")


PeakIRGmedian<-c(median(subset(Calv,Pres=="0")$IRGDay),
               median(subset(Calv,Herd=="BUCHANS" & Pres=="0")$IRGDay),
               median(subset(Calv,Herd=="GREY" & Pres=="0")$IRGDay),
               median(subset(Calv,Herd=="LAPOILE" & Pres=="0")$IRGDay),
               median(subset(Calv,Herd=="MIDRIDGE" & Pres=="0")$IRGDay),
               median(subset(Calv,Herd=="TOPSAILS" & Pres=="0")$IRGDay))

PeakIRGSD<-c(sd(subset(Calv,Pres=="0")$IRGDay),
             sd(subset(Calv,Herd=="BUCHANS" & Pres=="0")$IRGDay),
             sd(subset(Calv,Herd=="GREY" & Pres=="0")$IRGDay),
             sd(subset(Calv,Herd=="LAPOILE" & Pres=="0")$IRGDay),
             sd(subset(Calv,Herd=="MIDRIDGE" & Pres=="0")$IRGDay),
             sd(subset(Calv,Herd=="TOPSAILS" & Pres=="0")$IRGDay))


PeakIRGmedianform<-format(as.Date(PeakIRGmedian,origin = "1899-12-31"), "%b %d")

PeakIRGRange<-paste(PeakIRGmedianform, " [",round(PeakIRGSD,1), " days]", sep="")

SumTable<-t(data.frame(NumIDs,NumIDYears,MigrationStart,MigrationEnd,
                       MigrationDuration,MigrationDistance,SnowOffRange,CalvingRange,PeakIRGRange))

colnames(SumTable)<-c("All Herds","Buchans","Grey River","Lapoile","Middle Ridge","Topsails")
rownames(SumTable)<-c("Number of individuals","Number of ID years","median + [range], start date of migration",
                      "median + [range], end date of migration","median + [range], duration of migration (days)",
                      "median + [range], distance of migration (km)",
                      "median + [sd], date of snow melt over migratory route",
                      "median + [range], date of calving","median + [sd], date of peak IRG during calving")


SumTable

write.csv(SumTable, "Output/Results/SummaryTable.R2.csv")



##################################### END #######################################
