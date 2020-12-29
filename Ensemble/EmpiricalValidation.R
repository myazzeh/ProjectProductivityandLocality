EmpiricalValidation=function(dataset)
{
  source("MLP.R")
  source("RBF.R")
  source("MLRModel.R")
  source("RTModel.R")
  source("SVRModel.R")
  source("SRModel.R")
  source("FuzzyModel.R")
  source("evaluation.R")
  source("ComputeWeight.R")
  
  mydata=read.csv(dataset)
  
 
  estimatedeffort1=vector(mode="numeric", length=0)
  estimatedeffort2=vector(mode="numeric", length=0)
  estimatedeffort3=vector(mode="numeric", length=0)
  
  nFolds = nrow(mydata)
  #folds = rep_len(1:nFolds, nrow(data))
  equation<-formula(PDR ~ UAW+UUCW+TCF+ECF)
  
  for (fold in 1:nFolds) 
  {
    # actual split of the data
    # fold = which(folds == k)
    trainingData=mydata[-fold,]
    testingData=mydata[fold,]
    
    
    #1: SVR Model
    svr=SVRModel(trainingData,trainingData) #SVR contains predictions
    e1=evaluation(trainingData[,"Effort"],svr)
    prd1=SVRModel(trainingData,testingData)
    estimatedeffort1=rbind(estimatedeffort1, prd1) 
    
    #2: SR Model
    sr=SRModel(trainingData,trainingData) #SR contains predictions
    e2=evaluation(trainingData[,"Effort"],sr)
    prd2=SRModel(trainingData,testingData)
    estimatedeffort2=rbind(estimatedeffort2,prd2 )  
    
    #3: RT Model
    rt=FuzzyModel(trainingData,trainingData) #RT contains predictions
    e3=evaluation(trainingData[,"Effort"],rt)
    prd3=RTModel(trainingData,testingData)
    estimatedeffort3=rbind(estimatedeffort3, prd3)  
    
    
    # find weights
    mae=c(e1$MAE,e2$MAE,e3$MAE)
    mbre=c(e1$MBRE,e2$MBRE,e3$MBRE)
    mibre=c(e1$MIBRE,e2$MIBRE,e3$MIBRE)
    w1=ComputeWeight(mae)
    w2=ComputeWeight(mbre)
    w3=ComputeWeight(mibre)
    
    #we=1:7
    
    #w1=we[rank(mae)]
    #w2=we[rank(mbre)]
    #w3=we[rank(mibre)]
    
    w=(w1+w2+w3)/3
    # aggregation
    prd=c(prd1,prd2,prd3)
    effort=sum(prd*w)/sum(w)
    #effort=median(prd)
    estimatedeffort=rbind(estimatedeffort,effort)
    
    
  }
  
  actualeffort=mydata[,"Effort"]
  acc1=evaluation(actualeffort,estimatedeffort)
  acc2=evaluation(actualeffort,estimatedeffort1)
  acc3=evaluation(actualeffort,estimatedeffort2)
  acc4=evaluation(actualeffort,estimatedeffort3)
  acc5=evaluation(actualeffort,estimatedeffort4)
  acc6=evaluation(actualeffort,estimatedeffort5)
  acc7=evaluation(actualeffort,estimatedeffort6)
  acc8=evaluation(actualeffort,estimatedeffort7)
  
  acc=matrix(
      c(acc1$MAE,acc1$MBRE,acc1$MIBRE,
        acc2$MAE,acc2$MBRE,acc2$MIBRE,
        acc3$MAE,acc3$MBRE,acc3$MIBRE,
        acc4$MAE,acc4$MBRE,acc4$MIBRE,
        acc5$MAE,acc5$MBRE,acc5$MIBRE,
        acc6$MAE,acc6$MBRE,acc6$MIBRE,
        acc7$MAE,acc7$MBRE,acc7$MIBRE,
        acc8$MAE,acc8$MBRE,acc8$MIBRE),ncol=3,nrow = 8,byrow = T)
  
  rownames(acc)=c("Ensemble","MLR","Fuzzy","SVR","SR","RT","MLP","RBF")
  colnames(acc)=c("MAE","MBRE","MIBRE")
  
  AE=list(acc1$AE,acc2$AE,acc3$AE,acc4$AE,acc5$AE,acc6$AE,acc7$AE,acc8$AE)
  names(AE)=c("Ensemble","MLR","Fuzzy","SVR","SR","RT","MLP","RBF")
  return (acc)
}