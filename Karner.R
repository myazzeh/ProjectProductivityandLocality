Karner=function(data)
{
  prd=c(20)
  prd=rep(prd,nrow(data))
  estimatedeffort=data[,"UCP"]*prd
  source("evaluation.R")
  accuracy=evaluation(data[,"EFFORT"],estimatedeffort)
  return (accuracy)
  
}