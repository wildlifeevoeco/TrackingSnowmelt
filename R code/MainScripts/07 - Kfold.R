############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# March 10th, 2020 #############################################################################

############# Script 7: K-fold cross-validation ############################################################

############# + Comparing top model for snow off and green wave during migration, pulling out some stats ###

############################################################################################################
############################################################################################################

## Load data and model outputs

library(glmmTMB)
library(readr)

data<-readRDS("Output/Locs/LocsWSpatial_sub.RDS")
data<-data[complete.cases(data),]

modelOut<-readRDS("Output/RSFs/Top_models_output_glmmTMB.RDS")

###### Re-make datasets for X-Validation

### Migration

MigSnowAll<-subset(data,season=="Mig")
i<-parse_number(substr(rownames(modelOut$MigSnow_all)[1],1,6))

MigSnow<-data.frame(MigSnowAll$Herd,MigSnowAll$PtID,MigSnowAll$Pres,MigSnowAll$DaysToSnow-i,MigSnowAll$ID,MigSnowAll$burst)
colnames(MigSnow)<-c("Herd","strata","Pres","DaysTo","ID","burst")

# Take the absolute value (but make it negative for easier interpretation: higher = better)
MigSnow$DaysToAbs<-abs(MigSnow$DaysTo)*-1 

MigSnow<-MigSnow[order(MigSnow$ID),]

### Calving

CalvIRGAll<-subset(data,season=="Calving")
i<-parse_number(substr(rownames(modelOut$CalvIRG_all)[1],1,6))

CalvIRG<-data.frame(CalvIRGAll$Herd,CalvIRGAll$PtID,CalvIRGAll$Pres,CalvIRGAll$DaysToIRG-i,CalvIRGAll$ID,CalvIRGAll$burst)
colnames(CalvIRG)<-c("Herd","strata","Pres","DaysTo","ID","burst")

# Take the absolute value (but make it negative for easier interpretation: higher = better)
CalvIRG$DaysToAbs<-abs(CalvIRG$DaysTo)*-1 

CalvIRG<-CalvIRG[order(CalvIRG$ID),]

#### Run the K-Fold cross-validation

## Source in k-fold cross-validation function
source("Scripts/f01 - KFold.R")

##### Perform the cross-validation

#### Snow off migration   
#rownames(modelOut$MigSnow_all)
KfoldSO<-KfoldXVal(data=MigSnow,formula="Pres ~ -1 + DaysToAbs + (1|strata) + (1 | burst)",
                 binVar = "ID", modType="glmmTMB",setSeed = T, 
                 TMBtheta= log(1e3),TMBmaparg=list(theta=factor(c(NA,1))),
                 modFam="poisson")
KfoldSO

rownames(modelOut$CalvIRG_all)
KfoldIRG<-KfoldXVal(data=CalvIRG,formula="Pres ~ -1 + DaysToAbs + (1|strata) + (1 | burst)",
                    binVar = "ID", modType="glmmTMB",setSeed = T, 
                    TMBtheta= log(1e3),TMBmaparg=list(theta=factor(c(NA,1))),
                    modFam="poisson")
KfoldIRG

### Calculate the delta AIC and evidence ratio between snow off model and IRG model during migration

models<-readRDS("Output/RSFs/Top_models_glmmTMB.RDS")

AIC(models$MigSnow_all)-AIC(models$MigIRG_all)

library(AICcmodavg)
library(mclogit)

delta<-AIC(models$MigIRG_all)-AIC(models$MigSnow_all)

ER<-(exp(0)/exp(-1/2*delta))

### Infinite!


### Pulling out the best lags

parse_number(substr(rownames(modelOut$MigSnow_all)[1],1,6))

parse_number(substr(rownames(modelOut$MigSnow_Buchans)[1],1,6))
parse_number(substr(rownames(modelOut$MigSnow_Grey)[1],1,6))
parse_number(substr(rownames(modelOut$MigSnow_Lapoile)[1],1,6))
parse_number(substr(rownames(modelOut$MigSnow_MidRidge)[1],1,6))
parse_number(substr(rownames(modelOut$MigSnow_Topsails)[1],1,6))

parse_number(substr(rownames(modelOut$CalvIRG_all)[1],1,6))

parse_number(substr(rownames(modelOut$CalvIRG_Buchans)[1],1,6))
parse_number(substr(rownames(modelOut$CalvIRG_Grey)[1],1,6))
parse_number(substr(rownames(modelOut$CalvIRG_Lapoile)[1],1,6))
parse_number(substr(rownames(modelOut$CalvIRG_MidRidge)[1],1,6))
parse_number(substr(rownames(modelOut$CalvIRG_Topsails)[1],1,6))



##################################### END #######################################
