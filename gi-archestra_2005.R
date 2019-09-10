data <- read.csv("C:/Users/huyen/Dropbox/Stage_data/gi-archestra_2005.csv",
                 sep = ";")
x=data[,2]
summary(x)
plot(x,type = 'l')
plot.ts(x)
boxplot(x)
boxplot.stats(x)$out
(a1 <- which(x %in% boxplot.stats(x)$out))
data[20,1]
data[37,1]
############################
library(strucchange)
myts1 <- structure(x)
test2 <- Fstats(myts1~1) #Gets a sequence of fstatistics for all possible 
# break points within the middle 70% of myts1
myts1.fs <- test2$Fstats #These are the fstats
bp.myts1 <- breakpoints(myts1~1) #Gets the breakpoint based on the F-stats
plot(myts1) #plots the series myts1
lines(bp.myts1) #plots the break date implied by the sup F test
bd.myts1 <- breakdates(bp.myts1) #Obtains the implied break data (2018.35, 
# referring to day 128 (0.35*365 = day number))
sctest(test2) #Obtains a p-value for the implied breakpoint
ci.myts1 <- confint(bp.myts1) #95% CI for the location break date
plot(myts1)
lines(ci.myts1) #This shows the interval around the estimated break date
