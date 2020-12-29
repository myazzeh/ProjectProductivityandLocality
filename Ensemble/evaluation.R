evaluation=function(actual,predicted)
{
  Result=cbind(actual, predicted)
  colnames(Result)=c("ActualEffort", "PredictedEffort")
  
  AE=abs(Result[,"ActualEffort"]-Result[,"PredictedEffort"])
  MAE=mean(AE);
  
  MinR=apply(Result,1,min)
  MaxR=apply(Result,1,max)
  MBRE=mean(AE/MinR)
  MIBRE=mean(AE/MaxR)
  source("SA1.R")
  acc=SA1(actual,AE)
  Outcome=list(Result,AE,MAE,MBRE,MIBRE,acc$SA,acc$Delta)
  names(Outcome)=c("Predictions","AE","MAE","MBRE","MIBRE","SA","Delta")
  return (Outcome) 
  
}