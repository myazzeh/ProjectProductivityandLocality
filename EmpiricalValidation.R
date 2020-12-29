EmpiricalValidation=function(mydata)
{
  library(e1071)
  library(party)
  source("RTModel.R")
  source("SVRModel.R")
  source("SRModel.R")
  source("evaluation.R")
  
  
  estimatedeffort=data.frame(numeric(), numeric(), numeric(), numeric(), numeric())
  FoldNo = nrow(mydata)
  folds <- caret::createFolds(mydata$EFFORT, k = FoldNo, list = FALSE)
  equation<-formula(PDR ~ UAW+UUCW+TCF+ECF)
  #equation<-formula(PDR ~ UAW+UUCW+ECF)
  #equation<-formula(PDR ~ UAW+UUCW+TCF)
  #equation<-formula(PDR ~ UUCW+TCF+ECF)
  #equation<-formula(PDR ~ UAW+UUCW)
  #equation<-formula(PDR ~ TCF+ECF)
  #equation<-formula(PDR ~ UAW+ECF)
  #equation<-formula(PDR ~ UUCW+ECF)
  #equation<-formula(PDR ~ UAW+TCF)
  #equation<-formula(PDR ~ UUCW+TCF)
  #equation<-formula(PDR ~ UAW)
  #equation<-formula(PDR ~ UUCW)
  #equation<-formula(PDR ~ ECF)
  #equation<-formula(PDR ~ TCF)
  for(i in 1:FoldNo)
  { 
    
    print(i)
    trainingData <<- mydata[which(folds!=i),]
    testingData  <<- mydata[which(folds==i),]
    
    #1: SVR Model
    fit.svr <- svm(equation, data =trainingData)
    productivity = predict(fit.svr, newdata =  testingData)
    prd.svr=testingData[,"UCP"]*productivity
    
    
    #2: SR Model
    fit.sr <- step(lm(equation, data = trainingData), direction = "backward")
    #fit.sr<-lm(equation, data = trainingData)
    productivity = predict(fit.sr, newdata =  testingData)
    prd.sr=testingData[,"UCP"]*productivity
    
      
    
    #3: RT Model
    fit.rt<- ctree(equation, data =trainingData)
    productivity = predict(fit.rt, newdata =  testingData)
    prd.rt=testingData[,"UCP"]*productivity
    
    #4: Ensemble
    prd.ens=(prd.rt+prd.sr+prd.svr)/3
    
    
    temp=data.frame(testingData$EFFORT, prd.svr,prd.sr,prd.rt, prd.ens)
    estimatedeffort=rbind(estimatedeffort,temp)
    #colnames(estimatedeffort)=c("Actual","SVR","SR","RT","Ens")
    
  }
  
  acc.svr=evaluation(estimatedeffort[,1],estimatedeffort[,2])
  acc.sr=evaluation(estimatedeffort[,1],estimatedeffort[,3])
  acc.rt=evaluation(estimatedeffort[,1],estimatedeffort[,4])
  acc.ens=evaluation(estimatedeffort[,1],estimatedeffort[,5])
  
  acc=matrix(
      c(acc.svr$MAE,acc.svr$MBRE,acc.svr$MIBRE,acc.svr$SA$SA,acc.svr$SA$Delta,
        acc.sr$MAE,acc.sr$MBRE,acc.sr$MIBRE,acc.sr$SA$SA,acc.sr$SA$Delta,
        acc.rt$MAE,acc.rt$MBRE,acc.rt$MIBRE,acc.rt$SA$SA,acc.rt$SA$Delta,
        acc.ens$MAE,acc.ens$MBRE,acc.ens$MIBRE,acc.ens$SA$SA,acc.ens$SA$Delta
        ),nrow = 4, byrow = T)
  
  AE=list(acc.svr$AE,acc.sr$AE,acc.rt$AE,acc.ens$AE, acc)
  names(AE)=c("SVR","SR","RT","ENS", "acc")
  return (AE)
}

