############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# February 24th, 2020 ##########################################################################

############# Updated with glmmTMB July 20, 2020 ###########################################################

############# Script 5: RSFs ###############################################################################

############################################################################################################
############################################################################################################
time<-Sys.time()

## Load packages and data
library(mclogit)
library(glmmTMB)

data<-readRDS("Output/Locs/LocsWSpatial_sub.RDS")
data<-data[complete.cases(data),]

### Counting the number of individuals and ID-years per herd

t<-1
for(i in unique(data$Herd)){
  sub<-subset(data,Herd==i)
  ID<-length(unique(sub$ID))
  IDy<-length(unique(sub$burst))
  dat<-c(i,ID,IDy)
  if(t==1){
    counts<-dat
  }else{
    counts<-rbind(counts,dat)
  }
  t<-t+1
}

colnames(counts)<-c("Herd","IDs","ID-years")
counts

#### Making "seasonal" subsets. For snow off, make the "days to" column days to snow off,
#### for IRG make it days to snow off

MigSnow<-subset(data,season=="Mig")
MigSnow<-data.frame(MigSnow$Herd,MigSnow$PtID,MigSnow$Pres,MigSnow$DaysToSnow,MigSnow$burst,MigSnow$ID,MigSnow$year)
colnames(MigSnow)<-c("Herd","PtID","Pres","DaysTo","burst","ID","Year")

MigIRG<-subset(data,season=="Mig")
MigIRG<-data.frame(MigIRG$Herd,MigIRG$PtID,MigIRG$Pres,MigIRG$DaysToIRG,MigIRG$burst,MigIRG$ID,MigIRG$year)
colnames(MigIRG)<-c("Herd","PtID","Pres","DaysTo","burst","ID","Year")

CalvIRG<-subset(data,season=="Calving")
CalvIRG<-data.frame(CalvIRG$Herd,CalvIRG$PtID,CalvIRG$Pres,CalvIRG$DaysToIRG,CalvIRG$burst,CalvIRG$ID,CalvIRG$year)
colnames(CalvIRG)<-c("Herd","PtID","Pres","DaysTo","burst","ID","Year")

## Set the seasons
seasons<-c("MigSnow","MigIRG","CalvIRG")

## All the herds, including "all"
herds<-c("all",as.character(unique(data$Herd)))

####### Does glmmTMB give the same answer as mclogit? Changed at revision to incorporate
####### random effects

mclogTest<-mclogit(cbind(Pres,PtID)~DaysTo, data=MigSnow)

TMBTest = glmmTMB(Pres ~ -1 + DaysTo + (1|PtID), 
                    family=poisson, data=MigSnow, doFit=FALSE,
                  map=list(theta=factor(c(NA)))) 
TMBTest$parameters$theta[1] = log(1e3) 
TMBTest$mapArg = list(theta=factor(c(NA)))

glmm.TMB.test <- glmmTMB:::fitTMB(TMBTest)

summary(mclogTest)    #### 
summary(glmm.TMB.test)  ####  VERY minor difference

# Fitting the model using glmmTMB using the technique of Muff et al (2020),
# using a poisson model with strata fit as a random effect with a fixed, large variance

### - Here is an example version--
#TMBStruc = glmmTMB(Pres ~ -1 + DaysTo + (1|PtID) +
#                      (1 | burst), map=list(theta=factor(c(NA,1))),
#                    family=poisson, data=MigSnow, doFit=FALSE) 
#TMBStruc$parameters$theta[1] = log(1e3) 
#TMBStruc$mapArg = list(theta=factor(c(NA,1)))
#glmm.TMB.random <- glmmTMB:::fitTMB(TMBStruc)

Top<-10000000    ### Sets the default "best" AIC very high
t<-1             ### Start the loop

### Loop cycles through each season/variable, each herd, determines the appropriate lag for each herd for that season,
### runs the model and extracts the AIC, and finds the model with the lowest AIC. Outputs the AIC of each model,
### and the parameter estimates and SEs for the model.

for(k in seasons){
  for(j in herds){
    if(j=="all"){
      NewData<-eval(parse(text=k))# Don't subset if all data used
    }else{
      NewData<-subset(eval(parse(text=k)),Herd==j)#Subset to herd if necessary
    }
    min<-round(quantile(NewData$DaysTo, 0.10),0) # Determine the 10% and 90% quantile of the
    max<-round(quantile(NewData$DaysTo, 0.90),0) # number of days to snow off/peak IRG
    range<-seq(min,max,by=1) # Set this as the range
    for(i in range){
      subData<-data.frame(NewData$Pres,NewData$PtID,NewData$burst,NewData$Year,NewData$DaysTo-i) # New df, with days subtracted
      colnames(subData)<-c("Pres","strata","burst","Year", "DaysTo")
      subData$DaysToAbs<-abs(subData$DaysTo)*-1 # Take the absolute value 
      ##(but make it negative for easier interpretation: higher = better)
      
      ### Run the model
      
      TMBStruc = glmmTMB(Pres ~ -1 + DaysToAbs + (1|strata) +
                            (1 | burst), map=list(theta=factor(c(NA,1))),
                          family=poisson, data=subData, doFit=FALSE) 
      TMBStruc$parameters$theta[1] = log(1e3) 
      TMBStruc$mapArg = list(theta=factor(c(NA,1)))
      
      TMBmod <- glmmTMB:::fitTMB(TMBStruc)

      print(paste(k,j,i))  ### Progress update!!
      
      AICm<-AIC(TMBmod) ### Extract the AIC

      if(AICm<Top){  ### Logical test: if the best model at this lag is better than previous best, overwrite old best model:
        coefs<-fixef(TMBmod) ## Extract model coefficients
        SEs<-sqrt(diag(vcov(TMBmod,full=TRUE)))[1] ## Extract SEs
        bestModOut<-rbind(coefs[1],SEs[1]) ## Put them together
        rownames(bestModOut)<-c(paste("Lag",i,"Coefs",sep=""),"SEs") ## Name the rows, including lag and model number
        assign(paste(k,j,"bestModOut",sep="_"),bestModOut) ## Assign it as a new object with the season and herd
        assign(paste(k,j,"bestMod",sep="_"),TMBmod)
        Top<-AICm ## Reset the value of the top model to this new best
      }else{  
        ## If the model at this lag isn't better than the model to beat, do nothing
      }
      AICs2<-c(i,AIC(TMBmod))## Put the AIcs together with the value of the lag
      if(t==1){
        AICdata<-AICs2 ## First time: Make AICdata object and name them
        names(AICdata)<-c("Lag", "AIC")
      }else{
        AICdata<-rbind(AICdata,AICs2) ## Subsequently: rbind the new data to the previous data
      }
      t<-t+1
    }
    ### After lags finish, between herd iterations
    rownames(AICdata)<-NULL ## Get rid of non-sensical rownames
    AICdata<-cbind(AICdata[,1],AICdata[,2]-min(AICdata[,2])) ## Take the AIC data and turn it into a delta AIC
    assign(paste(k,j,"aicData",sep="_"),AICdata) ## Assign this data.frame to the correct herd/season
    Top<-10000000 ### Reset the AIC to beat for the next set of data
    t<-1 ### reset t
    
  }
}

### Some models throw warnings - "NA/NaN function evaluation"
### Appears to not be a problem, the optimizer goes into forbidden parameter space but still converges:
### https://mran.microsoft.com/snapshot/2017-12-11/web/packages/glmmTMB/vignettes/troubleshooting.html


### Combine all the AIC matrices into a list:
AIC_list<-list(MigIRG_all_aicData,MigIRG_BUCHANS_aicData,MigIRG_GREY_aicData,
               MigIRG_LAPOILE_aicData,MigIRG_MIDRIDGE_aicData,MigIRG_TOPSAILS_aicData,
               MigSnow_all_aicData,MigSnow_BUCHANS_aicData,MigSnow_GREY_aicData,
               MigSnow_LAPOILE_aicData,MigSnow_MIDRIDGE_aicData,MigSnow_TOPSAILS_aicData,
               CalvIRG_all_aicData,CalvIRG_BUCHANS_aicData,CalvIRG_GREY_aicData,
               CalvIRG_LAPOILE_aicData,CalvIRG_MIDRIDGE_aicData,CalvIRG_TOPSAILS_aicData)

### Combine all the top summaries into a list:
TopModOutput_list<-list(MigIRG_all_bestModOut,MigIRG_BUCHANS_bestModOut,MigIRG_GREY_bestModOut,
                        MigIRG_LAPOILE_bestModOut,MigIRG_MIDRIDGE_bestModOut,MigIRG_TOPSAILS_bestModOut,
                        MigSnow_all_bestModOut,MigSnow_BUCHANS_bestModOut,MigSnow_GREY_bestModOut,
                        MigSnow_LAPOILE_bestModOut,MigSnow_MIDRIDGE_bestModOut,MigSnow_TOPSAILS_bestModOut,
                        CalvIRG_all_bestModOut,CalvIRG_BUCHANS_bestModOut,CalvIRG_GREY_bestModOut,
                        CalvIRG_LAPOILE_bestModOut,CalvIRG_MIDRIDGE_bestModOut,CalvIRG_TOPSAILS_bestModOut)

### Combine all the top models into a list:
TopMod_list<-list(MigIRG_all_bestMod,MigIRG_BUCHANS_bestMod,MigIRG_GREY_bestMod,
                  MigIRG_LAPOILE_bestMod,MigIRG_MIDRIDGE_bestMod,MigIRG_TOPSAILS_bestMod,
                  MigSnow_all_bestMod,MigSnow_BUCHANS_bestMod,MigSnow_GREY_bestMod,
                  MigSnow_LAPOILE_bestMod,MigSnow_MIDRIDGE_bestMod,MigSnow_TOPSAILS_bestMod,
                  CalvIRG_all_bestMod,CalvIRG_BUCHANS_bestMod,CalvIRG_GREY_bestMod,
                  CalvIRG_LAPOILE_bestMod,CalvIRG_MIDRIDGE_bestMod,CalvIRG_TOPSAILS_bestMod)

### Name them
List_names<-c("MigIRG_all","MigIRG_Buchans","MigIRG_Grey",
              "MigIRG_Lapoile","MigIRG_MidRidge","MigIRG_Topsails",
              "MigSnow_all","MigSnow_Buchans","MigSnow_Grey",
              "MigSnow_Lapoile","MigSnow_MidRidge","MigSnow_Topsails",
              "CalvIRG_all","CalvIRG_Buchans","CalvIRG_Grey",
              "CalvIRG_Lapoile","CalvIRG_MidRidge","CalvIRG_Topsails")

names(AIC_list)<-List_names
names(TopMod_list)<-List_names
names(TopModOutput_list)<-List_names

### Save the output!!
saveRDS(AIC_list,"Output/RSFs/AICs_glmmTMB.RDS")
saveRDS(TopMod_list,"Output/RSFs/Top_models_glmmTMB.RDS")
saveRDS(TopModOutput_list,"Output/RSFs/Top_models_output_glmmTMB.RDS")


Sys.time()-time
##################################### END #######################################

