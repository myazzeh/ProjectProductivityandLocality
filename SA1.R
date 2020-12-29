SA1=function(actualEffort,AbsoluetErrors)
{
  # This function computes the SA and Delta accuracSA1es for one model only
  source("randomguessing.R")
  r=randomguessing(actualEffort)
  MeanAE=mean(AbsoluetErrors)
  sa=(1-(MeanAE/r$MAE))*100
  delta=abs(MeanAE-r$MAE)/r$SD
  
  result=list(sa,delta)
  names(result)=c("SA","Delta")
  return(result)
  
}