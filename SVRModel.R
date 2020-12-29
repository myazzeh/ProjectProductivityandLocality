SVRModel=function(trData,tsData)
{
  # upport Vector Regression model
  library(e1071)
  fit.svr <- svm(PDR ~ E1+E2+E2+E3+E4+E5+E6+E7+E8, data = trData)
  productivity = predict(fit.svr, newdata =  tsData)
  estimatedEffort=tsData[,"UCP"]*productivity
  return (t(as.data.frame(estimatedEffort)))
}