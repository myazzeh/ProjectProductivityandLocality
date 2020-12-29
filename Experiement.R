library(ScottKnott)

######model1#######
sk1 <- with(dat,SK(x=E2,y=PDR, model='y ~ x', which='x'))
summary(sk1)
plot(sk1,cex=1.4,lwd=2,las=1, col=rainbow(max(sk1$groups)), rl=FALSE,title="E2",xlab = "Levels" ,ylab = "PDR Mean")

#################################

av <- with(dat,
           aov(PDR ~ E1,
               data=dat))
sk3 <- SK(x=av,
            which='E1')
summary(sk3)
plot(sk3,
     las=1,
     col=rainbow(max(sk3$groups)),
     rl=FALSE,
     id.las=2,
     title=NULL)

######################################
######################################
library(ggplot2)
tgc <- summarySE(data, measurevar="PDR", groupvars=c("E8"))
pd <- position_dodge(0.1)
ggplot(tgc, aes(x=E8, y=PDR, colour=E8, group=E8)) +
  geom_line(position=pd) +
  geom_errorbar(aes(ymin=PDR-se, ymax=PDR+se), width=0.5, position=pd, lwd=1) +
  geom_point(position=pd, size=3, shape=21, fill="DarkBlue")+
  theme(text = element_text(size=15))


#########################################################################
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
