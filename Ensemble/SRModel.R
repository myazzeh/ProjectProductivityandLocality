SRModel=function(trData,tsData)
{
  equation<-formula(PDR ~ UAW+UUCW+TCF+ECF)
  fit.sr<-step(lm(formula = equation, data = trData), direction = "backward")
  productivity = predict(fit.sr, newdata =  tsData)
  estimatedEffort=tsData[,"UCP"]*productivity
  return (estimatedEffort)
}