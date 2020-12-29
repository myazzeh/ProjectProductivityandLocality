SVRModel=function(trData,tsData)
{
  # upport Vector Regression model
  library(e1071)
  equation<-formula(PDR ~ UAW+UUCW+TCF+ECF)
  fit.svr <- svm(formula=equation, data = trData)
  productivity = predict(fit.svr, newdata =  tsData)
  estimatedEffort=tsData[,"UCP"]*productivity
  return (t(as.data.frame(estimatedEffort)))
}