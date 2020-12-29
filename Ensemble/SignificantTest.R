SignificatTest=function(AE)
{
  col=ncol(AE)
  pval=matrix(1,col,col)
  colnames(pval)=names(AE)
  rownames(pval)=names(AE)
  for(i in 1:(col-1))
  {
    for(j in (i+1):col)
    {
      p=wilcox.test(AE[,i],AE[,j])
      pval[i,j]=p$p.value
      pval[j,i]=p$p.value
    }
  }
  return (pval)
}