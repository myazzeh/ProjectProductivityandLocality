Validate=function(split, mydata)
{
  #mydata=read.csv("data.csv", row.names = 1)
  source("EmpiricalValidation.R")
  source("Karner.R")
  source("SW.R")
  source("SignificantTest.R")
  if(split==1)
  {
    data1=mydata[mydata$E1==1 | mydata$E1==2 | mydata$E1==0, ]
    data2=mydata[mydata$E1==3,]
    data3=mydata[mydata$E1==4 | mydata$E1==5,]
  }
  
  if(split==2)
  {
    data1=mydata[mydata$E2==1 | mydata$E2==2 | mydata$E2==0, ]
    data2=mydata[mydata$E2==3,]
    data3=mydata[mydata$E2==4 | mydata$E2==5,]
  }
  
  if(split==3)
  {
    data1=mydata[mydata$E3==1 | mydata$E3==2 | mydata$E3==0, ]
    data2=mydata[mydata$E3==3,]
    data3=mydata[mydata$E3==4 | mydata$E3==5,]
  }
  if(split==4)
  {
    data1=mydata[mydata$E4==1 | mydata$E4==2 | mydata$E4==0, ]
    data2=mydata[mydata$E4==3,]
    data3=mydata[mydata$E4==4 | mydata$E4==5,]
  }
  if(split==5)
  {
    data1=mydata[mydata$E5==1 | mydata$E5==2 | mydata$E5==0, ]
    data2=mydata[mydata$E5==3,]
    data3=mydata[mydata$E5==4 | mydata$E5==5,]
  }
  if(split==6)
  {
    data1=mydata[mydata$E6==1 | mydata$E6==2 | mydata$E6==0, ]
    data2=mydata[mydata$E6==3,]
    data3=mydata[mydata$E6==4 | mydata$E6==5,]
  }
  if(split==7)
  {
    data1=mydata[mydata$E7==1 | mydata$E7==2 | mydata$E7==0, ]
    data2=mydata[mydata$E7==3,]
    data3=mydata[mydata$E7==4 | mydata$E7==5,]
  }
  if(split==8)
  {
    data1=mydata[mydata$E8==1 | mydata$E8==2 | mydata$E8==0, ]
    data2=mydata[mydata$E8==3,]
    data3=mydata[mydata$E8==4 | mydata$E8==5,]
  }
  if(split==9)
  {
    x=kmeans(mydata[,c("E1","E2","E3","E4","E5","E6","E7","E8")],4)
    data1=mydata[which(x$cluster==1),]
    data2=mydata[which(x$cluster==2),]
    data3=mydata[which(x$cluster==3),]
    data4=mydata[which(x$cluster==4),]
    #data5=mydata[which(x$cluster==5),]
    #data6=mydata[which(x$cluster==6),]
    
    ac1=EmpiricalValidation(data1)
    ac2=EmpiricalValidation(data2)
    ac3=EmpiricalValidation(data3)
    ac4=EmpiricalValidation(data4)
    #ac5=EmpiricalValidation(data5)
    #ac6=EmpiricalValidation(data6)
    
    ac=(ac1$acc+ac2$acc+ac3$acc+ac4$acc)/4
    return(ac)
  }
  
  if(split==10)
  {
    ac=EmpiricalValidation(mydata);
    return(ac$acc);
  }
  if(split==11)
  {
    ac=Karner(mydata);
    acc=matrix(c(ac$MAE, ac$MBRE, ac$MIBRE), nrow = 1)
    return(acc);
  }
  if(split==12)
  {
    ac=SW(mydata);
    acc=matrix(c(ac$MAE, ac$MBRE, ac$MIBRE), nrow = 1)
    return(acc);
  }
  
  ac1=EmpiricalValidation(data1)
  ac2=EmpiricalValidation(data2)
  ac3=EmpiricalValidation(data3)
  ac=(ac1[["acc"]]+ac2[["acc"]]+ac3[["acc"]])/3
  colnames(ac)=c("MAE","MBRE","MIBRE","SA","Delta")
  rownames(ac)=c("SVR","SR","RT","ENS")
  
  SVR=c(ac1$SVR,ac2$SVR,ac3$SVR)
  SR=c(ac1$SR,ac2$SR,ac3$SR)
  RT=c(ac1$RT,ac2$RT,ac3$RT)
  ENS=c(ac1$ENS,ac2$ENS,ac3$ENS)
  dd=data.frame(SVR,SR,RT,ENS)
  dd1=log(dd)
  dd1$id=rownames(dd1)
  mdata <- melt(dd1,id=c("id"))
  mdata=mdata[,2:3]
  colnames(mdata)=c("x","y")
  sk1 <- with(mdata,SK(x=x,y=y,model='y ~ x',which='x'))
  plot(sk1,col=rainbow(max(sk1$groups)), mm.lty=3, id.las=1, rl=FALSE, rl.lty=1,
       cex.main=0.95, cex.axis=1.5, font=2, lwd=2, xlab = "Class Imbalance Method",
       ylab="Means of errors",
       title="ScottKnott Analysis of predictions")
  
  sig=SignificatTest(dd)
  print(sig)
  
  #boxplot(dd,outline=FALSE, col=8)
  return (ac)
  
}


