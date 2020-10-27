##################################################################################

##------------------------------------------------------------------------------##

## Script for performing k-fold cross-validation, based on the script from
## Roberts et al 2017 Ecography. Performs the cross-validation and generates
## the cross-validation score and SE.

## Arguments:

## "data":     The dataset for which models should be run
## "formula":  The formula of the model to be tested. Include response and explanatory variables,
##             do NOT include a "data" argument,and do not include function name (e.g.: "Pres~x1+x2")
## "modType":  The type of model you want to fit. One of: "glmer" (default), "glm","lmer", and "mclogit".
## "binVar":   The variable that you want to bin observations by. Typically animal ID. Must >= than K.
## "k":        Number of training and testing bins to use. Default = 5.
## "resp":     The name of the response variable in your data. Default = Pres
## "modFam":   The family of the model (default = binomial)
## "setSeed":  Whether or not to set the randomization to the same each run (default = True)
## "TMBtheta": The value to pass along to "TMBStruc$parameters$theta[1]" before running the model.
##             This argument is used for the Muff et al (2020) method to fit conditional mixed
##             effects models.
## "TMBmaparg":The value to pass along to "TMBStruc$mapArg" both when setting of the funtion and before fitting.
##             This argument is used for the Muff et al (2020) method to fit conditional mixed
##             effects models. Seems to be necessary to get "predict" to work when setting the variance
##             structure manually for this method.

## Note: For function to load properly, it must be sourced, or the entire function must be highlighted
## and run. Running line-by-line from the start of the function causes it to only load part of the function.

##------------------------------------------------------------------------------##

##################################################################################

## Example:

## data<-readRDS("TestData/Test.RDS")

## source("KFold.R")
## kfold<-KfoldXVal(data=data,formula="Pres~Forest+Wetland+Rocky, family=binomial",
##                 binVar="AnimID",k=5,modType="glmer",resp="Pres")

## kfold

KfoldXVal<-function(
  data,
  formula,
  modType=glmer,
  binVar,
  k=5,
  resp="Pres",
  modFam="binomial",
  setSeed=T,
  TMBtheta=NULL,
  TMBmaparg=NULL
)
{
  x<-1:length(unique(eval(parse(text=paste("data$",binVar,sep="")))))
  
  spl<-split(x,cut(x,k,labels=FALSE))
  
  # split individuals randomly
  newdata <- data.frame(binVar = unique(eval(parse(text=paste("data$",binVar,sep="")))))
  if(setSeed==T){
    set.seed(k)
  }else{}
 
  random_sample <- data.frame(binVar = sample(newdata$binVar, 
                                              length(unique(eval(parse(text=paste("data$",binVar,sep="")))))))
  random_sample$rand.vec <- 0
  
  for(i in 1:k){
    random_sample$rand.vec[min(spl[[i]]):max(spl[[i]])]<-i
  }
  
  data <- merge(data, random_sample, by.x = binVar, by.y = "binVar", all.x = T )
  print("Fitting the models")
  if(modType=="glmer"){
    require(lme4)
    for(i in 1:k){
      assign(paste("Mod",i,sep=""),glmer(eval(parse(text=paste(formula))),family=modFam,data[data$rand.vec != i,]))
      print(i)
    }
  }else{
    if(modType=="glm"){
      for(i in 1:k){
        assign(paste("Mod",i,sep=""),glm(eval(parse(text=paste(formula))),family=modFam,data[data$rand.vec != i,]))
        print(i)
      }
    }else{
      if(modType=="lmer"){
        require(lme4)
        for(i in 1:k){
          assign(paste("Mod",i,sep=""),lmer(eval(parse(text=paste(formula))),data[data$rand.vec != i,]))
          print(i)
        }
      }else{
        if(modType=="mclogit"){
          require(mclogit)
          for(i in 1:k){
            assign(paste("Mod",i,sep=""),mclogit(eval(parse(text=paste(formula))),data[data$rand.vec != i,]))
            print(i)
          }
      }else{
          if(modType=="glmmTMB"){
            require(glmmTMB)
            for(i in 1:k){
              if(is.null(TMBmaparg)){
              TMBStruc = glmmTMB(eval(parse(text=paste(formula))), family=modFam,doFit=FALSE,data[data$rand.vec != i,])
              }else{
              TMBStruc = glmmTMB(eval(parse(text=paste(formula))), map=TMBmaparg,family=modFam,doFit=FALSE,data[data$rand.vec != i,])
              TMBStruc$mapArg = TMBmaparg
              }
              
              if(is.null(TMBtheta)){
                
              }else{
               TMBStruc$parameters$theta[1] = TMBtheta  
              }
            
              assign(paste("Mod",i,sep=""),glmmTMB:::fitTMB(TMBStruc))
              print(i)
            }
        }else{
          print("error:model type not supported")
        }
      }
    }
  }
  }
  print("Predicting values")
  for(i in 1:k){
    data$RSFscores[data$rand.vec == i]<-exp(predict(eval(parse(text=paste("Mod",i,sep=""))), 
                                                    newdata=subset(data,rand.vec == i), type="link",
                                                    allow.new.levels=T))
    print(i)
  }
  #### Why the F is this the same on each run? The models aren't identical, so...
  
  
  # Run the k-fold CV evaluation sensu Boyce et al. 2002
  dataset <- data[complete.cases(data[,"RSFscores"]),]
  rho_model <- numeric(k) ## it will store Spearman's coefficients
  
  for (w in 1:k){
    fold <- subset(dataset,rand.vec==unique(dataset$rand.vec)[w])
    q.pp <- quantile(fold$RSFscores,probs=seq(0,1,.1)) ## computing quantiles of RSF scores
    bin <- rep(NA,length(fold$RSFscores))
    for (j in 1:10){
      bin[fold$RSFscores>=q.pp[j]& fold$RSFscores<q.pp[j+1]] = j  ## binning RSF scores (10 bins)
    }
    used<-eval(parse(text=paste("fold$",resp,sep="")))
    # --------------------------------------------------------
    a <- table(used,bin) ## area adjusted freq in used/available for each bin
    a <- t(a) #transpose the table
    a <- as.data.frame.matrix(a) ## the next few lines compute area-adjusted frequency of categories (bins) of RSF scores 
    a$areaadjusted <- rep(NA,length(10))
    sum0 <- sum(a[,1])
    sum1 <- sum(a[,2])
    a$areaadjusted <- (a[,2] / sum1 ) / (a[,1] / sum0)
    a$bins <- seq(1,10,by=1);a
    rho_model[w] <- with(a,cor.test(bins,areaadjusted,method="spearm"))$estimate }
  
  
  ## store Spearman correlation coefficients that will be used for final plots below ##
  Rho_random_individuals <- rho_model
  
  kfold<-mean(Rho_random_individuals)
  error<-sd(Rho_random_individuals)
  output<-list(kfold,error,Rho_random_individuals)
  names(output)<-c("Kfold","SE","Scores")
  out<-output
}

#########################   END   ###################################
