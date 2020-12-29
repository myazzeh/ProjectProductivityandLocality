SA=function(actualEffort,AbsoluetErrors)
{
  # This function computes the SA and Delta accuracies for multiple models
  r=randomguessing(actualEffort)
  MeanAE=apply(AbsoluetErrors,2,mean) #by model
  
  sa=(1-(MeanAE/r$MAE))*100
  delta=abs(MeanAE-r$MAE)/r$SD
  
  result=list(sa,delta)
  names(result)=c("SA","Delta")
  return(result)
  
}