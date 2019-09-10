data <- read.csv("C:/Users/huyen/Dropbox/Stage_data/CPU_DSI66_1305.csv",
                 sep = ";")
colnames(data) = c('time', 'FD')
x=data[,2]
summary(x)
plot(x,type = 'l')
plot.ts(x)
boxplot(x)
boxplot.stats(x)$out
(a1 <- which(x %in% boxplot.stats(x)$out))


#####" segmentation######
library(segclust2d)
data(simulshift)
shift_seg <- segmentation(data,lmin = 250, Kmax= 3,seg.var='FD',
                          scale.variable = FALSE)
plot(shift_seg)





tsoutliers <- function(x,plot=FALSE)
{
  x <- as.ts(x)
  if(frequency(x)>1)
    resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  else
  {
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }
  resid.q <- quantile(resid,prob=c(0.25,0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
  if(plot)
  {
    plot(x)
    x2 <- ts(rep(NA,length(x)))
    x2[score>0] <- x[score>0]
    tsp(x2) <- tsp(x)
    points(x2,pch=19,col="red")
    return(invisible(score))
  }
  else
    return(score)
}
tsoutliers(x)

##############################
stl(x, robust = TRUE)# ham nay chi ap dung cho period

