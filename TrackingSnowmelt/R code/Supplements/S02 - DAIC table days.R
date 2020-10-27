############################################################################################################
############################################################################################################

############# Snow-off or green-up?             ############################################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# May 17th, 2020                    ############################################################

############# Script S02: Additional results, delta LL table  ##############################################

############################################################################################################
############################################################################################################


AICs<-readRDS("Output/RSFs/AICs_glmmTMB.RDS")

ModList<-names(AICs)

t<-1
for(i in ModList){
  mod<-AICs[[t]]
  Est<-mod[which.min(mod[,2]),1]
  lower<-round(approx(x=mod[1:which.min(mod[,2]),2],
                y=mod[1:which.min(mod[,2]),1],xout=2)$y,1)
  if(mod[nrow(mod),2]==0){
    upper<-NA
  }else{
  upper<-round(approx(x=mod[which.min(mod[,2]):length(mod[,2]),2],
                y=mod[which.min(mod[,2]):length(mod[,2]),1],
                xout=2)$y,1)
  }
  range<-paste(Est," (", lower, ", ",upper, ")",sep="")
  Dif<-upper-lower
  Output<-c(range,round(Dif,3))
  if(t==1){
  Out<-Output
  }else{
  Out<-rbind(Out,Output)  
  }
  t<-t+1
}
Out2<-cbind(ModList,Out)

colnames(Out2)<-c("Model","Range","Difference")

Out2[7:12,2:3]
herds<-c("All","Buchans","Grey River","Lapoile","Middle Ridge","Topsails")

### Extracting LLs for table 
models<-readRDS("Output/RSFs/Top_models_glmmTMB.RDS")

MigSnowLLs<-c(logLik(models$MigSnow_all),logLik(models$MigSnow_Buchans),logLik(models$MigSnow_Grey),
              logLik(models$MigSnow_Lapoile),logLik(models$MigSnow_MidRidge),logLik(models$MigSnow_Topsails))
CalvIRGLLs<-c(logLik(models$CalvIRG_all),logLik(models$CalvIRG_Buchans),logLik(models$CalvIRG_Grey),
              logLik(models$CalvIRG_Lapoile),logLik(models$CalvIRG_MidRidge),logLik(models$CalvIRG_Topsails))
MigIRGLLs<-c(logLik(models$MigIRG_all),logLik(models$MigIRG_Buchans),logLik(models$MigIRG_Grey),
             logLik(models$MigIRG_Lapoile),logLik(models$MigIRG_MidRidge),logLik(models$MigIRG_Topsails))

Table<-data.frame(herds,Out2[7:12,2:3],MigSnowLLs,Out2[13:18,2:3],CalvIRGLLs,Out2[1:6,2:3],MigIRGLLs)
colnames(Table)<-c("Herd","Range - MigSnow","Difference - MigSnow","LLs - MigSnow", "Range - CalvIRG",
                   "Difference - CalvIRG","LL - CalvIRG", "Range - MigIRG", "Difference - MigIRG","LL - MigIRG")

Table

write.csv(Table, "Output/Results/dAICsDay.csv")




##################################### END #######################################
