y1=data[E1==1,]
y2=data[E1==2,]
y3=data[E1==3,]
y4=data[E1==4,]
y5=data[E1==5,]

boxplot(y1$Productivity, 
        y2$Productivity, 
        y3$Productivity, 
        y4$Productivity, 
        y5$Productivity, 
        col=c("cadetblue4", "palegoldenrod", "darkolivegreen","royalblue1","firebrick4"), 
        main="Productivity vs. E1", 
        ylab="Productivity", 
        xlab="E1 (Familiar with RUP)",
        names=c("=1","=2","=3","=4","=5"),
        las=1
        )


y1=data[E2==1,]
y2=data[E2==2,]
y3=data[E2==3,]
y4=data[E2==4,]
y5=data[E2==5,]
boxplot(y1$Productivity, 
        y2$Productivity, 
        y3$Productivity, 
        y4$Productivity, 
        y5$Productivity, 
        col=c(2,3,4,5,6), 
        main="Productivity vs. E2",
        ylab="Productivity",
        xlab="E2 (Application Experience)",
        names=c("=1","=2","=3","=4","=5"),
        las=1)
y1=data[E3==1,]
y2=data[E3==2,]
y3=data[E3==3,]
y4=data[E3==4,]
y5=data[E3==5,]
boxplot(y1$Productivity, 
        y2$Productivity, 
        y3$Productivity, 
        y4$Productivity, 
        y5$Productivity, 
        col=c(2,3,4,5,6), names=c("=1","=2","=3","=4","=5"),
        las=1, main="Productivity vs. E3", ylab="Productivity", xlab="E3 (Object-Oriented experience)")

y1=data[E4==1,]
y2=data[E4==2,]
y3=data[E4==3,]
y4=data[E4==4,]
y5=data[E4==5,]
boxplot(y1$Productivity, 
        y2$Productivity, 
        y3$Productivity, 
        y4$Productivity, 
        y5$Productivity, 
        col=c(2,3,4,5,6), names=c("=1","=2","=3","=4","=5"),
        las=1, main="Productivity vs. E4", ylab="Productivity", xlab="E4 (Lead Analyst Capability)")

y1=data[E5==1,]
y2=data[E5==2,]
y3=data[E5==3,]
y4=data[E5==4,]
y5=data[E5==5,]
boxplot(y1$Productivity, 
        y2$Productivity, 
        y3$Productivity, 
        y4$Productivity, 
        y5$Productivity, 
        col=c(2,3,4,5,6), names=c("=1","=2","=3","=4","=5"),
        las=1,main="Productivity vs. E5", ylab="Productivity", xlab="E5 (Motivation)")

y1=data[E6==1,]
y2=data[E6==2,]
y3=data[E6==3,]
y4=data[E6==4,]
y5=data[E6==5,]
boxplot(y1$Productivity, 
        y2$Productivity, 
        y3$Productivity, 
        y4$Productivity, 
        y5$Productivity, 
        col=c(2,3,4,5,6), names=c("=1","=2","=3","=4","=5"),
        las=1, main="Productivity vs. E6", ylab="Productivity", xlab="E6 (Stable Requirements)")

y1=data[E7==1,]
y2=data[E7==2,]
y3=data[E7==3,]
y4=data[E7==4,]
y5=data[E7==5,]
boxplot(y1$Productivity, 
        y2$Productivity, 
        y3$Productivity, 
        y4$Productivity, 
        y5$Productivity, 
        col=c(2,3,4,5,6),names=c("=1","=2","=3","=4","=5"),
        las=1,  main="Productivity vs. E7", ylab="Productivity", xlab="E7 (Part-Time Staff)")

y1=data[E8==1 || E8==0,]
y2=data[E8==2,]
y3=data[E8==3,]
y4=data[E8==4,]
y5=data[E8==5,]
boxplot(y1$Productivity, 
        y2$Productivity, 
        y3$Productivity, 
        y4$Productivity, 
        y5$Productivity, 
        col=c(2,3,4,5,6),names=c("=1","=2","=3","=4","=5"),
        las=1, main="Productivity vs. E8", ylab="Productivity", xlab="E8 (Difficult Programming Language)")

