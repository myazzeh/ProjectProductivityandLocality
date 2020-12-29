RTModel=function(trData,tsData)
{
  # Regression Tree model
  library(party)
  fit.rt<- ctree(PDR ~ E1+E2+E2+E3+E4+E5+E6+E7+E8, data = trData)
  productivity = predict(fit.rt, newdata =  tsData)
  estimatedEffort=tsData[,"UCP"]*productivity
  return (estimatedEffort)
}