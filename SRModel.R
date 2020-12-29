SRModel=function(trData,tsData)
{
  
  fit.sr<-step(lm(PDR ~ E1+E2+E2+E3+E4+E5+E6+E7+E8, data = trData), direction = "backward")
  productivity = predict(fit.sr, newdata =  tsData)
  estimatedEffort=tsData[,"UCP"]*productivity
  return (estimatedEffort)
}