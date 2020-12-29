randomguessing=function(actualEffort)
{
  r=length(actualEffort)
  iter=1000
  estimatedeffort=vector(mode="numeric", length = r)
  e=matrix(0,nrow = r,ncol = iter)
  prd=matrix(rep(actualEffort,iter),nrow = r, ncol = iter)
  for(i in 1:r)
  {
    for (j in 1:iter)
    {
      e[i,j]=sample(actualEffort[-i],1, replace = TRUE)
    }
    
  }
  Errors=abs(prd-e);
  AErrors=apply(Errors, 1, mean)
  SDRow=apply(Errors, 1, sd)
  
  MAE=mean(AErrors)
  SD=mean(SDRow)
  result=list(MAE,SD)
  names(result)=c("MAE","SD")
  return (result)
}

