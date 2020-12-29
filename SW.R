SW=function(data)
{
  estimatedeffort=data[,"UCP"]*data[,"SW"]
  source("evaluation.R")
  accuracy=evaluation(data[,"EFFORT"],estimatedeffort)
  return (accuracy)
  
}