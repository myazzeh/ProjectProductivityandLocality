ComputeWeight=function(errors)
{
  errors=(errors-min(errors))/(max(errors)-min(errors))
  weight=1/(1+exp(15*(errors-median(errors))))
  return(weight)
}