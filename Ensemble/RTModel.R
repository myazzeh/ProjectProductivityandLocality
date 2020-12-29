RTModel=function(trData,tsData)
{
  # Regression Tree model
  library(party)
  equation<-formula(PDR ~ UAW + UUCW + TCF + ECF)
  fit.rt<- ctree(formula = equation, data = trData)
  productivity = predict(fit.rt, newdata =  tsData)
  estimatedEffort=tsData[,"UCP"]*productivity
  return (estimatedEffort)
}