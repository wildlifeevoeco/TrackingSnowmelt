############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# February 24th, 2020 ##########################################################################

############# Script 6: Plotting RSFs ######################################################################

############################################################################################################
############################################################################################################

####### Updated version March 31st - Only one model - with absolute value (red model in previous iteration)
#### New update April 16th - Transforming to delta LL instead of delta AIC
#### Update July 21st - using glmmTMB instead of mclogit to fit models to include random terms


## read in the data

AICs<-readRDS("Output/RSFs/AICs_glmmTMB.RDS")

AICs[[1]]

m <- rbind(c(1, 1, 2), c(1, 1, 3), c(4,5,6))

###### Figure 2 - Delta log-likelihoods

#png("Figures/Fig2-LLs_TMB.R2.png",width=170,height=150,units="mm",res=600, pointsize = 10)
pdf("Figures/Fig2-LLs_TMB.Final.pdf",width=170/25.4,height=150/25.4,pointsize = 9.5)
par(mar=c(1.5,2,1,2),oma=c(3,3,1,1))
layout(m)

xloc<-0.2
yloc<-0.8
lthick<-2

### A) All herds

### Multiply deltaAIC by -0.5 to get delta LL (k doesn't change)

plot(AICs$MigSnow_all[3:71,2]*-0.5~AICs$MigSnow_all[3:71,1],type="l",xlab=NA,ylab=NA,lwd=lthick+2,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_all[,1],AICs$CalvIRG_all[,1])),max(AICs$MigSnow_all[,1],AICs$CalvIRG_all[,1])),
     col="Blue")
axis(2,col.axis="blue",cex.axis=1.5)
abline(v=0,lty=2,lwd=0.5)
par(new=TRUE)
plot(AICs$CalvIRG_all[,2]*-0.5~AICs$CalvIRG_all[,1],type="l",xlab=NA,ylab=NA,lwd=lthick+2,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_all[,1],AICs$CalvIRG_all[,1])),max(AICs$MigSnow_all[,1],AICs$CalvIRG_all[,1])),
     col="forestgreen")
axis(4,col.axis="forestgreen",cex.axis=1.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("A) All herds",y=yloc+0.1,x=xloc+0.65, cex=1.5)
legend(legend=c("Snowmelt, migration","green-up, calving"),col=c("Blue","forestgreen"),lty=1,lwd=lthick,x=0.01,y=0.98,cex=1.5,bty='n')



### Each herd

## B) Buchans

plot(AICs$MigSnow_Buchans[,2]*-0.5~AICs$MigSnow_Buchans[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_Buchans[,1],AICs$CalvIRG_Buchans[,1])),max(AICs$MigSnow_Buchans[,1],AICs$CalvIRG_Buchans[,1])),
     col="Blue")
axis(2,col.axis="blue",cex.axis=0.9)
abline(v=0,lty=2,lwd=0.5)
par(new=TRUE)
plot(AICs$CalvIRG_Buchans[,2]*-0.5~AICs$CalvIRG_Buchans[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_Buchans[,1],AICs$CalvIRG_Buchans[,1])),max(AICs$MigSnow_Buchans[,1],AICs$CalvIRG_Buchans[,1])),
     col="forestgreen")
axis(4,col.axis="forestgreen",cex.axis=0.9)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("B) Buchans",y=yloc+0.1,x=xloc, cex=1.2)

## C) Grey River

plot(AICs$MigSnow_Grey[,2]*-0.5~AICs$MigSnow_Grey[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_Grey[,1],AICs$CalvIRG_Grey[,1])),max(AICs$MigSnow_Grey[,1],AICs$CalvIRG_Grey[,1])),
     col="Blue")
axis(2,col.axis="blue",cex.axis=0.9)
abline(v=0,lty=2,lwd=0.5)
par(new=TRUE)
plot(AICs$CalvIRG_Grey[,2]*-0.5~AICs$CalvIRG_Grey[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_Grey[,1],AICs$CalvIRG_Grey[,1])),max(AICs$MigSnow_Grey[,1],AICs$CalvIRG_Grey[,1])),
     col="forestgreen")
axis(4,col.axis="forestgreen",cex.axis=0.9)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("C) Grey River",y=yloc+0.1,x=xloc, cex=1.2)

## D) Lapoile

plot(AICs$MigSnow_Lapoile[,2]*-0.5~AICs$MigSnow_Lapoile[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_Lapoile[,1],AICs$CalvIRG_Lapoile[,1])),max(AICs$MigSnow_Lapoile[,1],AICs$CalvIRG_Lapoile[,1])),
     col="Blue")
axis(2,col.axis="blue",cex.axis=0.9)
abline(v=0,lty=2,lwd=0.5)
par(new=TRUE)
plot(AICs$CalvIRG_Lapoile[,2]*-0.5~AICs$CalvIRG_Lapoile[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_Lapoile[,1],AICs$CalvIRG_Lapoile[,1])),max(AICs$MigSnow_Lapoile[,1],AICs$CalvIRG_Lapoile[,1])),
     col="forestgreen")
axis(4,col.axis="forestgreen",cex.axis=0.9)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("D) Lapoile",y=yloc+0.1,x=xloc+0.05, cex=1.2)


## E) Middle Ridge

plot(AICs$MigSnow_MidRidge[,2]*-0.5~AICs$MigSnow_MidRidge[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_MidRidge[,1],AICs$CalvIRG_MidRidge[,1])),max(AICs$MigSnow_MidRidge[,1],AICs$CalvIRG_MidRidge[,1])),
     col="Blue")
axis(2,col.axis="blue",cex.axis=0.9)
abline(v=0,lty=2,lwd=0.5)
par(new=TRUE)
plot(AICs$CalvIRG_MidRidge[,2]*-0.5~AICs$CalvIRG_MidRidge[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_MidRidge[,1],AICs$CalvIRG_MidRidge[,1])),max(AICs$MigSnow_MidRidge[,1],AICs$CalvIRG_MidRidge[,1])),
     col="forestgreen")
axis(4,col.axis="forestgreen",cex.axis=0.9)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("E) Middle Ridge",y=yloc+0.1,x=xloc+0.55, cex=1.2)

## F) Topsails

plot(AICs$MigSnow_Topsails[,2]*-0.5~AICs$MigSnow_Topsails[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_Topsails[,1],AICs$CalvIRG_Topsails[,1])),max(AICs$MigSnow_Topsails[,1],AICs$CalvIRG_Topsails[,1])),
     col="Blue")
axis(2,col.axis="blue",cex.axis=0.9)
abline(v=0,lty=2,lwd=0.5)
par(new=TRUE)
plot(AICs$CalvIRG_Topsails[,2]*-0.5~AICs$CalvIRG_Topsails[,1],type="l",xlab=NA,ylab=NA,lwd=lthick,yaxt='n',
     xlim=c(min(c(AICs$MigSnow_Topsails[,1],AICs$CalvIRG_Topsails[,1])),max(AICs$MigSnow_Topsails[,1],AICs$CalvIRG_Topsails[,1])),
     col="forestgreen")
axis(4,col.axis="forestgreen",cex.axis=0.9)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("F) Topsails",y=yloc+0.1,x=xloc, cex=1.2)

mtext("Lag (days)",outer=T,side=1,padj=1.5)
mtext(expression(paste(Delta," log-likelihood")),outer=T,side=2,padj=-1)

dev.off()



##### Green-up, migration....

#png("Figures/FigS2-2-AICSIRGMigration_TMB.final.png",width=170,height=150,units="mm",res=600,pointsize = 11.5)
pdf("Figures/FigS2-2-AICSIRGMigration_TMB.final.pdf",width=170/25.4,height=150/25.4,pointsize = 11.5)
par(mar=c(1,1,1,1),oma=c(3,3,1,1))
layout(m)

xloc<-0.3
yloc<-0.85
lthick<-2

### A) All herds

plot(AICs$MigIRG_all[,2]*-0.5~AICs$MigIRG_all[,1],
     type="l",xlab=NA,ylab=NA,lwd=lthick+2, col="forestgreen", lty=2, cex.axis=1.5)

abline(v=0,lty=2,lwd=0.5)
usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("A) All herds",y=yloc,x=xloc+0.45, cex=1.5)

### Each herd

## B) Buchans

plot(AICs$MigIRG_Buchans[,2]*-0.5~AICs$MigIRG_Buchans[,1],
     type="l",xlab=NA,ylab=NA,lwd=lthick, col="forestgreen", lty=2)


abline(v=0,lty=2,lwd=0.5)
usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("B) Buchans",y=yloc,x=xloc+0.4)

## C) Grey River

plot(AICs$MigIRG_Grey[,2]*-0.5~AICs$MigIRG_Grey[,1],
     type="l",xlab=NA,ylab=NA,lwd=lthick, col="forestgreen", lty=2)

abline(v=0,lty=2,lwd=0.5)
usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("C) Grey River",y=yloc,x=xloc+0.2)

## D) Lapoile

plot(AICs$MigIRG_Lapoile[,2]*-0.5~AICs$MigIRG_Lapoile[,1],
     type="l",xlab=NA,ylab=NA,lwd=lthick, col="forestgreen", lty=2)

abline(v=0,lty=2,lwd=0.5)
usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("D) Lapoile",y=yloc,x=xloc-0.1)


## E) Middle Ridge

plot(AICs$MigIRG_MidRidge[,2]*-0.5~AICs$MigIRG_MidRidge[,1],
     type="l",xlab=NA,ylab=NA,lwd=lthick, col="forestgreen", lty=2)

abline(v=0,lty=2,lwd=0.5)
usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("E) Middle Ridge",y=yloc,x=xloc)

## F) Topsails

plot(AICs$MigIRG_Topsails[,2]*-0.5~AICs$MigIRG_Topsails[,1],
     type="l",xlab=NA,ylab=NA,lwd=lthick, col="forestgreen", lty=2)

abline(v=0,lty=2,lwd=0.5)
usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("F) Topsails",y=yloc,x=xloc+0.25)

mtext("Lag (days)",outer=T,side=1,padj=1.5)
mtext(expression(paste(Delta," log-likelihood")),outer=T,side=2,padj=-1)

dev.off()




######## Plotting RSFs ##############

library(glmmTMB)
library(readr)
modelOut<-readRDS("Output/RSFs/Top_models_output_glmmTMB.RDS")
models<-readRDS("Output/RSFs/Top_models_glmmTMB.RDS")

modList<-c("MigSnow_all","MigSnow_Buchans","MigSnow_Grey","MigSnow_Lapoile","MigSnow_MidRidge","MigSnow_Topsails",
           "CalvIRG_all","CalvIRG_Buchans","CalvIRG_Grey","CalvIRG_Lapoile","CalvIRG_MidRidge","CalvIRG_Topsails",
           "MigIRG_all","MigIRG_Buchans","MigIRG_Grey","MigIRG_Lapoile","MigIRG_MidRidge","MigIRG_Topsails")

startDay<--20
endDay<-20

days<-seq(startDay,endDay,1)
daysAbs<-abs(days)*-1

nd<-data.frame(DaysToAbs = daysAbs, strata=1, burst=NA)

for(i in modList){
  assign(paste(i,"Pred",sep="_"),predict(eval(parse(text=paste("models$",i,sep=""))), 
                                         newdata=nd, type="link",se.fit=T,interval="predict", re.form = ~0))
  assign(paste(i,"Lag",sep="_"),days-parse_number(substr(rownames(eval(parse(text=paste("modelOut$",i,sep=""))))[1],1,6))*-1)
  print(i)
}


#### Figure 3 ####

#png("Figures/Fig3-ResponsesByHerd_glmmTMB.FINAL.png",width=170,height=150,units="mm",res=600, pointsize=11)
pdf("Figures/Fig3-ResponsesByHerd_glmmTMB.FINAL.pdf",width=170/25.4,height=150/25.4, pointsize=11)

m <- rbind(c(1, 1, 2), c(1, 1, 3), c(4,5,6))

par(mfrow=c(2,3),mar=c(1,1,1,1),oma=c(3,3,1,1))
layout(m)

xloclet<-0.8
yloclet<-0.9

### Plotting the response
plot(MigSnow_all_Lag, MigSnow_all_Pred$fit, typ="l", 
     ylim=c(-1.3,0),
     xlim=c(-30,30),
     ylab=NA,xlab=NA,col="Blue",lwd=2,cex.axis=1.2)
lines(MigSnow_all_Lag, MigSnow_all_Pred$fit+MigSnow_all_Pred$se.fit*1.96,lty=2,col="Blue")
lines(MigSnow_all_Lag, MigSnow_all_Pred$fit-MigSnow_all_Pred$se.fit*1.96,lty=2,col="Blue")

lines(CalvIRG_all_Lag, CalvIRG_all_Pred$fit,col="forestgreen",lwd=2)
lines(CalvIRG_all_Lag, CalvIRG_all_Pred$fit+CalvIRG_all_Pred$se.fit*1.96,lty=2, col="forestgreen")
lines(CalvIRG_all_Lag, CalvIRG_all_Pred$fit-CalvIRG_all_Pred$se.fit*1.96,lty=2, col="forestgreen")
abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("A) All herds",y=yloclet,x=xloclet, cex = 1.5)
legend(legend=c("Snowmelt, migration", "Green-up, calving"), x=0,y=0.2,
       col=c("Blue","forestgreen"),lty=c(1,1),bty='n',lwd=c(2,2),cex=1.5)


plot(MigSnow_Buchans_Lag, MigSnow_Buchans_Pred$fit, typ="l", 
     ylim=c(-1.3,0),
     xlim=c(-30,30),
     ylab=NA,xlab=NA,col="Blue",lwd=2)
lines(MigSnow_Buchans_Lag, MigSnow_Buchans_Pred$fit+MigSnow_Buchans_Pred$se.fit*1.96,lty=2,col="Blue")
lines(MigSnow_Buchans_Lag, MigSnow_Buchans_Pred$fit-MigSnow_Buchans_Pred$se.fit*1.96,lty=2,col="Blue")

lines(CalvIRG_Buchans_Lag, CalvIRG_Buchans_Pred$fit,col="forestgreen",lwd=2)
lines(CalvIRG_Buchans_Lag, CalvIRG_Buchans_Pred$fit+CalvIRG_Buchans_Pred$se.fit*1.96,lty=2, col="forestgreen")
lines(CalvIRG_Buchans_Lag, CalvIRG_Buchans_Pred$fit-CalvIRG_Buchans_Pred$se.fit*1.96,lty=2, col="forestgreen")
abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("B) Buchans",y=yloclet,x=xloclet-0.65)


plot(MigSnow_Grey_Lag, MigSnow_Grey_Pred$fit, typ="l", 
     ylim=c(-1.3,0),
     xlim=c(-30,30),
     ylab=NA,xlab=NA,col="Blue",lwd=2)
lines(MigSnow_Grey_Lag, MigSnow_Grey_Pred$fit+MigSnow_Grey_Pred$se.fit*1.96,lty=2,col="Blue")
lines(MigSnow_Grey_Lag, MigSnow_Grey_Pred$fit-MigSnow_Grey_Pred$se.fit*1.96,lty=2,col="Blue")

lines(CalvIRG_Grey_Lag, CalvIRG_Grey_Pred$fit,col="forestgreen",lwd=2)
lines(CalvIRG_Grey_Lag, CalvIRG_Grey_Pred$fit+CalvIRG_Grey_Pred$se.fit*1.96,lty=2, col="forestgreen")
lines(CalvIRG_Grey_Lag, CalvIRG_Grey_Pred$fit-CalvIRG_Grey_Pred$se.fit*1.96,lty=2, col="forestgreen")
abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("C) Grey River",y=yloclet+0.03,x=xloclet+0.025)


plot(MigSnow_Lapoile_Lag, MigSnow_Lapoile_Pred$fit, typ="l", 
     ylim=c(-1.3,0),
     xlim=c(-30,30),
     ylab=NA,xlab=NA,col="Blue",lwd=2)
lines(MigSnow_Lapoile_Lag, MigSnow_Lapoile_Pred$fit+MigSnow_Lapoile_Pred$se.fit*1.96,lty=2,col="Blue")
lines(MigSnow_Lapoile_Lag, MigSnow_Lapoile_Pred$fit-MigSnow_Lapoile_Pred$se.fit*1.96,lty=2,col="Blue")

lines(CalvIRG_Lapoile_Lag, CalvIRG_Lapoile_Pred$fit,col="forestgreen",lwd=2)
lines(CalvIRG_Lapoile_Lag, CalvIRG_Lapoile_Pred$fit+CalvIRG_Lapoile_Pred$se.fit*1.96,lty=2, col="forestgreen")
lines(CalvIRG_Lapoile_Lag, CalvIRG_Lapoile_Pred$fit-CalvIRG_Lapoile_Pred$se.fit*1.96,lty=2, col="forestgreen")
abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("D) Lapoile",y=yloclet,x=xloclet)


plot(MigSnow_MidRidge_Lag, MigSnow_MidRidge_Pred$fit, typ="l", 
     ylim=c(-1.3,0),
     xlim=c(-30,30),
     ylab=NA,xlab=NA,col="Blue",lwd=2)
lines(MigSnow_MidRidge_Lag, MigSnow_MidRidge_Pred$fit+MigSnow_MidRidge_Pred$se.fit*1.96,lty=2,col="Blue")
lines(MigSnow_MidRidge_Lag, MigSnow_MidRidge_Pred$fit-MigSnow_MidRidge_Pred$se.fit*1.96,lty=2,col="Blue")

lines(CalvIRG_MidRidge_Lag, CalvIRG_MidRidge_Pred$fit,col="forestgreen",lwd=2)
lines(CalvIRG_MidRidge_Lag, CalvIRG_MidRidge_Pred$fit+CalvIRG_MidRidge_Pred$se.fit*1.96,lty=2, col="forestgreen")
lines(CalvIRG_MidRidge_Lag, CalvIRG_MidRidge_Pred$fit-CalvIRG_MidRidge_Pred$se.fit*1.96,lty=2, col="forestgreen")
abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("E) Middle Ridge",y=yloclet,x=xloclet)


plot(MigSnow_Topsails_Lag, MigSnow_Topsails_Pred$fit, typ="l", 
     ylim=c(-1.3,0),
     xlim=c(-30,30),
     ylab=NA,xlab=NA,col="Blue",lwd=2)
lines(MigSnow_Topsails_Lag, MigSnow_Topsails_Pred$fit+MigSnow_Topsails_Pred$se.fit*1.96,lty=2,col="Blue")
lines(MigSnow_Topsails_Lag, MigSnow_Topsails_Pred$fit-MigSnow_Topsails_Pred$se.fit*1.96,lty=2,col="Blue")

lines(CalvIRG_Topsails_Lag, CalvIRG_Topsails_Pred$fit,col="forestgreen",lwd=2)
lines(CalvIRG_Topsails_Lag, CalvIRG_Topsails_Pred$fit+CalvIRG_Topsails_Pred$se.fit*1.96,lty=2, col="forestgreen")
lines(CalvIRG_Topsails_Lag, CalvIRG_Topsails_Pred$fit-CalvIRG_Topsails_Pred$se.fit*1.96,lty=2, col="forestgreen")
abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("F) Topsails",y=yloclet,x=xloclet+0.05)


mtext("Days until snowmelt/green-up", side=1, outer=T, padj=1.5)
mtext("Probability of selection", side=2, outer=T,padj=-1.3)

dev.off()


##### Figure S2-3 - IRG during migration

#png("Figures/FigS2-3-ResponsesIRG_mig_glmmTMB.final.png",width=170,height=150,units="mm",res=600, pointsize=11.5)
pdf("Figures/FigS2-3-ResponsesIRG_mig_glmmTMB.Final.pdf",width=170/25.4,height=150/25.4, pointsize=11.5)

m <- rbind(c(1, 1, 2), c(1, 1, 3), c(4,5,6))

par(mfrow=c(2,3),mar=c(1,1,1,1),oma=c(3,3,1,1))
layout(m)

xloclet<-0.5
yloclet<-0.9

### Plotting the response
plot(MigIRG_all_Lag, MigIRG_all_Pred$fit, typ="l", ylim=c(-1,0),xlim=c(-85,5),
     ylab=NA,xlab=NA,col="forestgreen",lwd=2, cex.axis=1.2)
lines(MigIRG_all_Lag, MigIRG_all_Pred$fit+MigIRG_all_Pred$se.fit*1.96,lty=2,col="forestgreen")
lines(MigIRG_all_Lag, MigIRG_all_Pred$fit-MigIRG_all_Pred$se.fit*1.96,lty=2,col="forestgreen")

abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("A) All herds",y=yloclet,x=xloclet+0.2,cex=1.5)


plot(MigIRG_Buchans_Lag, MigIRG_Buchans_Pred$fit, typ="l", ylim=c(-1,0),xlim=c(-85,5),
     ylab=NA,xlab=NA,col="forestgreen",lwd=2)
lines(MigIRG_Buchans_Lag, MigIRG_Buchans_Pred$fit+MigIRG_Buchans_Pred$se.fit*1.96,lty=2,col="forestgreen")
lines(MigIRG_Buchans_Lag, MigIRG_Buchans_Pred$fit-MigIRG_Buchans_Pred$se.fit*1.96,lty=2,col="forestgreen")

abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("B) Buchans",y=yloclet,x=xloclet, cex=1.2)


plot(MigIRG_Grey_Lag, MigIRG_Grey_Pred$fit, typ="l", ylim=c(0,1),xlim=c(-85,5),
     ylab=NA,xlab=NA,col="forestgreen",lwd=2)
lines(MigIRG_Grey_Lag, MigIRG_Grey_Pred$fit+MigIRG_Grey_Pred$se.fit*1.96,lty=2,col="forestgreen")
lines(MigIRG_Grey_Lag, MigIRG_Grey_Pred$fit-MigIRG_Grey_Pred$se.fit*1.96,lty=2,col="forestgreen")

abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("C) Grey River",y=yloclet,x=xloclet, cex=1.2)


plot(MigIRG_Lapoile_Lag, MigIRG_Lapoile_Pred$fit, typ="l", ylim=c(-1,0),xlim=c(-85,5),
     ylab=NA,xlab=NA,col="forestgreen",lwd=2)
lines(MigIRG_Lapoile_Lag, MigIRG_Lapoile_Pred$fit+MigIRG_Lapoile_Pred$se.fit*1.96,lty=2,col="forestgreen")
lines(MigIRG_Lapoile_Lag, MigIRG_Lapoile_Pred$fit-MigIRG_Lapoile_Pred$se.fit*1.96,lty=2,col="forestgreen")

abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("D) Lapoile",y=yloclet,x=xloclet+0.2, cex=1.2)


plot(MigIRG_MidRidge_Lag, MigIRG_MidRidge_Pred$fit, typ="l",  ylim=c(0,1),xlim=c(-85,5),
     ylab=NA,xlab=NA,col="forestgreen",lwd=2)
lines(MigIRG_MidRidge_Lag, MigIRG_MidRidge_Pred$fit+MigIRG_MidRidge_Pred$se.fit*1.96,lty=2,col="forestgreen")
lines(MigIRG_MidRidge_Lag, MigIRG_MidRidge_Pred$fit-MigIRG_MidRidge_Pred$se.fit*1.96,lty=2,col="forestgreen")

abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("E) Middle Ridge",y=yloclet,x=xloclet, cex=1.2)


plot(MigIRG_Topsails_Lag, MigIRG_Topsails_Pred$fit, typ="l",  ylim=c(0,1),xlim=c(-85,5),
     ylab=NA,xlab=NA,col="forestgreen",lwd=2)
lines(MigIRG_Topsails_Lag, MigIRG_Topsails_Pred$fit+MigIRG_Topsails_Pred$se.fit*1.96,lty=2,col="forestgreen")
lines(MigIRG_Topsails_Lag, MigIRG_Topsails_Pred$fit-MigIRG_Topsails_Pred$se.fit*1.96,lty=2,col="forestgreen")

abline(v=0,lty=2,lwd=0.5)

usr <- par("usr")  	# get user coordinates 
par(usr = c(0, 1, 0, 1)) # new relative user coordinates 
text("F) Topsails",y=yloclet,x=xloclet, cex=1.2)


mtext("Days until green-up", side=1, outer=T, padj=1.5)
mtext("Probability of selection", side=2, outer=T,padj=-1.5)

dev.off()

##################################### END #######################################
