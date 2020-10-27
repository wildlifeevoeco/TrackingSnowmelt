############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# February 24th, 2020 ##########################################################################

############# Script 4: Surfing indices ####################################################################

############################################################################################################
############################################################################################################

library(data.table)
library(piecewiseSEM)

data<-readRDS("Output/Locs/LocsWSpatial.RDS")

## Drop NAs (~8000)
data<-data[complete.cases(data),]

## Drop short-distance migrators
hist(data$SpringMigDst)
data<-subset(data,SpringMigDst>30000)
data<-droplevels(data)

#### Final sample size after dropping individuals that migrated less than 30 km

length(unique(data$ID))  
length(unique(data$burst))

saveRDS(data, "Output/Locs/LocsWSpatial_sub.RDS")

### Re-read the data

data<-readRDS("Output/Locs/LocsWSpatial_sub.RDS")  #### Archived on Zenodo as "locs" with actual location removed

## Make delta Snow-IRG
data$dif<-data$IRGDay-data$SnowOff

hist(data$SpringMigDst)

### Datasets with Pres only for each season (random points not used here)

MigrPres<-data.table(subset(data, Pres==1 & season=="Mig"))
CalvPres<-data.table(subset(data, Pres==1 & season=="Calving"))

##### Getting mean surfing index score for each individual for snow melt and green up

MigrSOff<-MigrPres[,mean(DaysToSnow,na.rm = TRUE),by=burst]
MigrIRG<-MigrPres[,mean(DaysToIRG,na.rm = TRUE),by=burst]
CalvIRG<-CalvPres[,mean(DaysToIRG,na.rm = TRUE),by=burst]

### Migration distance versus surfing indices

CalvIRGDist<-CalvPres[,mean(SpringMigDst,na.rm = TRUE),by=burst]

DistData<-merge(CalvIRGDist,CalvIRG,by="burst")
DistData<-merge(DistData,MigrSOff,by="burst")

colnames(DistData)<-c("burst","Distance","CalvIRGInd","MigrSOInd")
DistData$Distance<-DistData$Distance/1000

summary(lm(abs(DistData$CalvIRGInd)~DistData$Distance))
summary(lm(abs(DistData$MigrSOInd)~DistData$Distance))

plot(abs(DistData$CalvIRGInd)~DistData$Distance,xlab="Distance migrated",ylab="Mean days from peak IRG during calving (absolute value)")


### By Herd:

MigrSOff_MidR<-MigrPres[Herd=="MIDRIDGE",mean(DaysToSnow,na.rm = TRUE),by=burst]
MigrSOff_Lapo<-MigrPres[Herd=="LAPOILE",mean(DaysToSnow,na.rm = TRUE),by=burst]
MigrSOff_Buch<-MigrPres[Herd=="BUCHANS",mean(DaysToSnow,na.rm = TRUE),by=burst]
MigrSOff_Grey<-MigrPres[Herd=="GREY",mean(DaysToSnow,na.rm = TRUE),by=burst]
MigrSOff_Tops<-MigrPres[Herd=="TOPSAILS",mean(DaysToSnow,na.rm = TRUE),by=burst]

MigrIRG_MidR<-MigrPres[Herd=="MIDRIDGE",mean(DaysToIRG,na.rm = TRUE),by=burst]
MigrIRG_Lapo<-MigrPres[Herd=="LAPOILE",mean(DaysToIRG,na.rm = TRUE),by=burst]
MigrIRG_Buch<-MigrPres[Herd=="BUCHANS",mean(DaysToIRG,na.rm = TRUE),by=burst]
MigrIRG_Grey<-MigrPres[Herd=="GREY",mean(DaysToIRG,na.rm = TRUE),by=burst]
MigrIRG_Tops<-MigrPres[Herd=="TOPSAILS",mean(DaysToIRG,na.rm = TRUE),by=burst]

CalvIRG_MidR<-CalvPres[Herd=="MIDRIDGE",mean(DaysToIRG,na.rm = TRUE),by=burst]
CalvIRG_Lapo<-CalvPres[Herd=="LAPOILE",mean(DaysToIRG,na.rm = TRUE),by=burst]
CalvIRG_Buch<-CalvPres[Herd=="BUCHANS",mean(DaysToIRG,na.rm = TRUE),by=burst]
CalvIRG_Grey<-CalvPres[Herd=="GREY",mean(DaysToIRG,na.rm = TRUE),by=burst]
CalvIRG_Tops<-CalvPres[Herd=="TOPSAILS",mean(DaysToIRG,na.rm = TRUE),by=burst]


densitiesMigSO<-round(c(density(MigrSOff$V1)$x[which.max(density(MigrSOff$V1)$y)],
             density(MigrSOff_Buch$V1)$x[which.max(density(MigrSOff_Buch$V1)$y)],
             density(MigrSOff_Grey$V1)$x[which.max(density(MigrSOff_Grey$V1)$y)],
             density(MigrSOff_Lapo$V1)$x[which.max(density(MigrSOff_Lapo$V1)$y)],
             density(MigrSOff_MidR$V1)$x[which.max(density(MigrSOff_MidR$V1)$y)],
             density(MigrSOff_Tops$V1)$x[which.max(density(MigrSOff_Tops$V1)$y)]),2)

densitiesCalvIRG<-round(c(density(CalvIRG$V1)$x[which.max(density(CalvIRG$V1)$y)],
                  density(CalvIRG_Buch$V1)$x[which.max(density(CalvIRG_Buch$V1)$y)],
                  density(CalvIRG_Grey$V1)$x[which.max(density(CalvIRG_Grey$V1)$y)],
                  density(CalvIRG_Lapo$V1)$x[which.max(density(CalvIRG_Lapo$V1)$y)],
                  density(CalvIRG_MidR$V1)$x[which.max(density(CalvIRG_MidR$V1)$y)],
                  density(CalvIRG_Tops$V1)$x[which.max(density(CalvIRG_Tops$V1)$y)]),2)

densitiesMigIRG<-round(c(density(MigrIRG$V1)$x[which.max(density(MigrIRG$V1)$y)],
                  density(MigrIRG_Buch$V1)$x[which.max(density(MigrIRG_Buch$V1)$y)],
                  density(MigrIRG_Grey$V1)$x[which.max(density(MigrIRG_Grey$V1)$y)],
                  density(MigrIRG_Lapo$V1)$x[which.max(density(MigrIRG_Lapo$V1)$y)],
                  density(MigrIRG_MidR$V1)$x[which.max(density(MigrIRG_MidR$V1)$y)],
                  density(MigrIRG_Tops$V1)$x[which.max(density(MigrIRG_Tops$V1)$y)]),2)

densitiesAll<-data.frame(densitiesMigSO,densitiesCalvIRG,densitiesMigIRG)

rownames(densitiesAll)<-c("All","Buchans","Grey River","Lapoile","Middle Ridge","Topsails")

densitiesAll
write.csv(densitiesAll,"Output/Results/Densities.csv")

### Making delta mean IRG at birth data

data<-readRDS("Output/Locs/LocsWSpatial_sub.RDS")

head(data)
dataAvail<-subset(data,Pres==0)
dt<-data.table(dataAvail)
MeanPeak<-dt[,mean(IRGDay,na.rm = TRUE),by=burst]

dataAvailmerge<-data.frame(dataAvail$burst,dataAvail$Herd,dataAvail$CalfStart)
colnames(dataAvailmerge)<-c("burst","Herd","Calving")
MeanPeak2<-merge(MeanPeak,dataAvailmerge,by="burst",all = F)
BirthIRG<-MeanPeak2[!duplicated(MeanPeak2$burst),]
colnames(BirthIRG)<-c("burst","PeakIRG","Herd","CalfDate")
BirthIRG$Delta<-BirthIRG$CalfDate-BirthIRG$PeakIRG

nrow(subset(BirthIRG, Delta<0))/nrow(BirthIRG)

#### Figure 1: Density distributions

m <- rbind(c(1, 1, 2), c(1, 1, 3), c(4,5,6))

#png("Figures/Fig1-SurfIndDensPlots30km_lab.R2.png",width=170,height=150,units="mm",res=600)
pdf("Figures/Fig1-SurfIndDensPlots30km_lab.Final.pdf",width=170/25.4,height=150/25.4)
par(mar=c(2,1,0.5,0.5),oma=c(2,2.5,1,1))
usr <- par("usr")  	# get user coordinates 
layout(m)

plot(density(CalvIRG$V1),xlim=c(-120,60),col="forestgreen",lwd=4,ylab=NA,
     yaxt='n',xlab=NA, main=NA,
     ylim=c(0,max(c(max(density(CalvIRG$V1)$y),max(density(MigrIRG$V1)$y),
                    max(density(MigrSOff$V1)$y)))))
lines(density(MigrIRG$V1),col="forestgreen",lwd=4,lty=2)
lines(density(MigrSOff$V1),col="blue",lwd=4)
arrows(x0=BirthIRG$Delta,x1=BirthIRG$Delta,y0=0,y1=0.001,angle=0,lwd=0.3,col="forestgreen")
abline(v=0,lty=2)
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
legend(x=0.05,y=0.98,col=c("blue","forestgreen","forestgreen"),lty=c(1,1,2),lwd=c(3,3),
       legend=c("Snowmelt, migration","Green-up, calving","Green-up, migration"),cex=1.4,bty='n',seg.len=3)
text("A) All herds",x=0.85,y=0.9,cex=1.5)


plot(density(CalvIRG_Buch$V1),xlim=c(-120,60),col="forestgreen",lwd=2,ylab=NA,
     yaxt='n',xlab=NA, main=NA,
     ylim=c(0,max(c(max(density(CalvIRG_Buch$V1)$y),max(density(MigrIRG_Buch$V1)$y),
                    max(density(MigrSOff_Buch$V1)$y)))))
lines(density(MigrIRG_Buch$V1),col="forestgreen",lwd=2,lty=2)
lines(density(MigrSOff_Buch$V1),col="blue",lwd=2)
abline(v=0,lty=2)
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("B) Buchans",x=0.3,y=0.9,cex=1.2)


plot(density(CalvIRG_Grey$V1),xlim=c(-120,60),col="forestgreen",lwd=2,ylab=NA,
     yaxt='n',xlab=NA, main=NA, 
     ylim=c(0,max(c(max(density(CalvIRG_Grey$V1)$y),max(density(MigrIRG_Grey$V1)$y),
                    max(density(MigrSOff_Grey$V1)$y)))))
lines(density(MigrIRG_Grey$V1),col="forestgreen",lwd=2,lty=2)
lines(density(MigrSOff_Grey$V1),col="blue",lwd=2)
abline(v=0,lty=2)
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("C) Grey River",x=0.3,y=0.9,cex=1.2)

plot(density(CalvIRG_Lapo$V1),xlim=c(-120,60),col="forestgreen",lwd=2,ylab=NA,
     yaxt='n',xlab=NA, main=NA, 
     ylim=c(0,max(c(max(density(CalvIRG_Lapo$V1)$y),max(density(MigrIRG_Lapo$V1)$y),
                    max(density(MigrSOff_Lapo$V1)$y)))))
lines(density(MigrIRG_Lapo$V1),col="forestgreen",lwd=2,lty=2)
lines(density(MigrSOff_Lapo$V1),col="blue",lwd=2)
abline(v=0,lty=2)
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("D) Lapoile",x=0.3,y=0.9,cex=1.2)


plot(density(CalvIRG_MidR$V1),xlim=c(-120,60),col="forestgreen",lwd=2,ylab=NA,
     yaxt='n',xlab=NA, main=NA, 
     ylim=c(0,max(c(max(density(CalvIRG_MidR$V1)$y),max(density(MigrIRG_MidR$V1)$y),
                    max(density(MigrSOff_MidR$V1)$y)))))
lines(density(MigrIRG_MidR$V1),col="forestgreen",lwd=2,lty=2)
lines(density(MigrSOff_MidR$V1),col="blue",lwd=2)
abline(v=0,lty=2)
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("E) Middle Ridge",x=0.3,y=0.9,cex=1.2)


plot(density(CalvIRG_Tops$V1),xlim=c(-120,60),col="forestgreen",lwd=2,ylab=NA,
     yaxt='n',xlab=NA, main=NA, 
     ylim=c(0,max(c(max(density(CalvIRG_Tops$V1)$y),max(density(MigrIRG_Tops$V1)$y),
                    max(density(MigrSOff_Tops$V1)$y)))))
lines(density(MigrIRG_Tops$V1),col="forestgreen",lwd=2,lty=2)
lines(density(MigrSOff_Tops$V1),col="blue",lwd=2)
abline(v=0,lty=2)
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("F) Topsails",x=0.3,y=0.9,cex=1.2)

mtext("Density", side=2, outer = T)
mtext("Days from peak", side=1,padj=1, outer = T)

par(usr = usr)	# restore original user coordinates 

dev.off()


#### Correlations between indices

### Prep data for correlations

MigrDT<-MigrPres
CalvDT<-CalvPres

MigrDT$Burst2<-paste(MigrDT$Herd,MigrDT$burst,sep="")
CalvDT$Burst2<-paste(CalvDT$Herd,CalvDT$burst,sep="")

MigrSOff2<-MigrDT[,mean(DaysToSnow,na.rm = TRUE),by=Burst2]
MigrIRG2<-MigrDT[,mean(DaysToIRG,na.rm = TRUE),by=Burst2]
#MigrSOffH<-MigrDT[,Herd,by=Burst]

MigrInd<-data.frame(MigrSOff2$Burst2,MigrSOff2$V1,MigrIRG2$V1)
colnames(MigrInd)<-c("Burst","SnowOffMig","IRGMig")
MigrInd$Herd<-substr(MigrInd$Burst,1,4)

CalvIRG2<-CalvDT[,mean(DaysToIRG,na.rm = TRUE),by=Burst2]

colnames(CalvIRG2)<-c("Burst","CalvIRG")
Indices<-merge(MigrInd,CalvIRG2,by="Burst")

nrow(Indices)




png("Figures/FigS2-1-SurfIndCors.R2.png",width=85,height=200,units="mm",res=600)
#pdf("Figures/FigS2-1-SurfIndCors.R2.pdf",width=85/25.4,height=200/25.4)

par(mfrow=c(3,1),mar=c(4.2,4,1,1))
usr <- par("usr")  	# get user coordinates 

plot(MigrInd$IRGMig~MigrInd$SnowOffMig,
     xlab="Snowmelt surfing index",
     ylab="Green-wave surfing index, migration",
     pch = c(1,2,3,4,5)[as.factor(MigrInd$Herd)])
abline(glm(MigrInd$IRGMig~MigrInd$SnowOffMig))
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text(0.4, 0.9, bquote(R^2 == .(rs), list(rs=round(rsquared(lm(MigrInd$IRGMig~MigrInd$SnowOffMig))$R.squared,2))))
text(0.15, 0.9, "A)")
legend(legend=c("Buchans","Grey River","Lapoile","Middle Ridge","Topsails"),
       pch=c(1,2,3,4,5),x=0.01,y=0.88,bty='n')

plot(Indices$CalvIRG~Indices$SnowOffMig,
     xlab="Snowmelt surfing index",
     ylab="Green-wave surfing index, calving",
     pch = c(1,2,3,4,5)[as.factor(Indices$Herd)])
abline(glm(Indices$CalvIRG~Indices$SnowOffMig))
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text(0.4, 0.9, bquote(R^2 == .(rs), list(rs=round(rsquared(lm(Indices$CalvIRG~Indices$SnowOffMig))$R.squared,2))))
text(0.15, 0.9, "B)")

plot(Indices$CalvIRG~Indices$IRGMig,
     xlab="Green-wave surfing index, migration",
     ylab="Green-wave surfing index, calving",
     pch = c(1,2,3,4,5)[as.factor(Indices$Herd)])
abline(glm(Indices$CalvIRG~Indices$IRGMig))
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text(0.4, 0.9, bquote(R^2 == .(rs), list(rs=round(rsquared(lm(Indices$CalvIRG~Indices$IRGMig))$R.squared,2))))
text(0.15, 0.9, "C)")

par(usr = usr)	# restore original user coordinates 

dev.off()

### Regressions

M1<-lm(Indices$IRGMig~Indices$SnowOffMig)
M2<-lm(Indices$CalvIRG~Indices$SnowOffMig)
M3<-lm(Indices$CalvIRG~Indices$IRGMig)

summary(M1)
summary(M2)
summary(M3)

M1coef<-round(M1$coef[2],3)
M1SE<-c(sqrt(diag(vcov(M1))))[2]
M1LCI<-round(M1coef-(M1SE*1.96),3)
M1UCI<-round(M1coef+(M1SE*1.96),3)
M1CI<-paste(M1coef, " (",M1LCI,", ",M1UCI,")", sep="")

M2coef<-round(M2$coef[2],3)
M2SE<-c(sqrt(diag(vcov(M2))))[2]
M2LCI<-round(M2coef-(M2SE*1.96),3)
M2UCI<-round(M2coef+(M2SE*1.96),3)
M2CI<-paste(M2coef, " (",M2LCI,", ",M2UCI,")", sep="")

M3coef<-round(M3$coef[2],3)
M3SE<-c(sqrt(diag(vcov(M3))))[2]
M3LCI<-round(M3coef-(M3SE*1.96),3)
M3UCI<-round(M3coef+(M3SE*1.96),3)
M3CI<-paste(M3coef, " (",M3LCI,", ",M3UCI,")", sep="")

rs<-c(rsquared(M1)$R.squared,rsquared(M2)$R.squared,rsquared(M3)$R.squared)

#rsquared(M1)$R.squared

mods<-rbind(M1CI,M2CI,M3CI)
mods<-data.frame(mods,rs)

colnames(mods)<-c("Estimate+95% CI","R-squared")
rownames(mods)<-c("SnowOffMig vs IRGMig","SnowOffMig vs IRGcalving","IRGmig vs IRGcalving")

mods

write.csv(mods,"Output/Results/SurfIndices.csv")


####################  END #############################
