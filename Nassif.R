Nassif=function(TrainingData, TestingData)
{
  library(FuzzyR)
  prod_sum=(1.4-TestingData[,"ECF"])/0.03
  source("Ali_FIS.R")
  fis=Ali_FIS()
  productivity=evalfis(prod_sum,fis)
  Y=TrainingData[,"EFFORT"]
  X=TrainingData[,"UCP"]
  mod=nls(Y ~ A*(X^B), data=list(cbind(X,Y)), start=list(A=1, B=1))
  estimatedEffort=predict(mod, newdata=list(X=TestingData[,"UCP"]))
  estimatedEffort=estimatedEffort/productivity;
  return(estimatedEffort)
  
}

