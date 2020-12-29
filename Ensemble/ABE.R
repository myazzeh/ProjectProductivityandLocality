ABE = function(TrainingDataIn, TestingDataIn, AnalogyNumber) {
  
  # this function is designed to use basic ABE method
  # 1. replicate the test vector
  #TestingDataIn = do.call("rbind", replicate(nrow(TrainingDataIn), TestingDataIn, simplify = FALSE))
  
  trdata=TrainingDataIn[,c("E1","E2","E3","E4","E5","E6","E7","E8")]
  tsdata=TestingDataIn[,c("E1","E2","E3","E4","E5","E6","E7","E8")]
  
  productivity=knn(trdata, test=tsdata, k = AnalogyNumber, prob = FALSE,use.all = TRUE)
  estimatedEffort=tsData[,"UCP"]*productivity$pred
  return (estimatedEffort)
  
}