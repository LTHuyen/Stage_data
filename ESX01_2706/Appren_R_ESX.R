library(tidyr)
library(MASS)
library(scales)
library(pls)
library(prospectr)
# Lecture des données
df = read.csv("/Users/lehuyen/Dropbox/Stage_data/ESX01_2706/ESX01_2706_S.csv",sep = ";",na.strings="null", dec =",")
# Vérification du contenu
colnames(df)= c('Time','CPU','MEM','REC', 'TRAN','RLATE','WLATE')

df =df[,-1]
summary(df)
par(mfrow=c(2,3))
for (i in 1:length(df)) {
  plot.ts(df[,i], main=names(df[i]), type="l")
  
}
df = na.omit(df)
dim(df)
par(mfrow=c(2,3))
for (i in 1:length(df)) {
  boxplot(df[,i], main=names(df[i]), type="l")
  
}
options(repr.plot.width=8, repr.plot.height=8)
pairs(df[,c(1:6)])
boxplot.stats(df$CPU)$out
(a1 <- which(df$CPU %in% boxplot.stats(df$CPU)$out))
max(df$CPU)
boxplot(df$MEM)
boxplot.stats(df$MEM)$out
(a2 <- which(df$MEM%in% boxplot.stats(df$MEM)$out))
boxplot(df$REC)
boxplot.stats(df$REC)$out
(a3 <- which(df$REC%in% boxplot.stats(df$REC)$out))
boxplot(df$TRAN)
boxplot.stats(df$TRAN)$out
(a4 <- which(df$TRAN%in% boxplot.stats(df$TRAN)$out))
boxplot(df$RLATE)
boxplot.stats(df$RLATE)$out
(a5 <- which(df$RLATE%in% boxplot.stats(df$RLATE)$out))
boxplot(df$WLATE)
boxplot.stats(df$WLATE)$out
(a6 <- which(df$WLATE%in% boxplot.stats(df$WLATE)$out))
df[447,]
Reduce(intersect, list(a1,a2,a3))
Reduce(intersect, list(a3,a4,a5,a6))
intersect(a4,a6)
intersect(a3,a4)
min(df$REC)
#### Analyse par ACP ######
library(FactoMineR)
library(factoextra)
library(ade4)
par(mfrow = c(1,1))
X = scale(df)
res.pca = PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30))
# Graphique des variables 
var <- get_pca_var(res.pca)
var
# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)
# Coordonnées des variables
head(var$coord, 4)
fviz_pca_var(res.pca)
fviz_pca_var(res.pca, col.var = "black")

# Biplot des individus et des variables
fviz_pca_biplot(res.pca, axes = c(1, 2), geom = c("point", "text"),
                label = "all", invisible = "none", labelsize = 4, pointsize = 2,
                habillage = "none", addEllipses = FALSE, ellipse.level = 0.95,
                col.ind = "black", col.ind.sup = "blue", alpha.ind = 1,
                col.var = "steelblue", alpha.var = 1, col.quanti.sup = "blue",
                col.circle = "grey70")

############################
df[,"DepSeuil"]=as.factor(df[,"RLATE"]>25)

# Variable cible
Y=df[,"DepSeuil"]
# Variables explicatives
#X=df[,-c(7)]
n=nrow(X); p=ncol(X)
summary(Y); #summary(X)
#######LOF########
library(Rlof)
dfR= X
atypLof=lof(dfR,k=c(1:6),cores=3)
options(repr.plot.width=3, repr.plot.height=4)
boxplot(atypLof)
#table(atypLof[,1]>200,Y)

atypLofInd=which(atypLof[,1]> 2)
coul=as.integer(df[,"DepSeuil"])+2
taille=rep(0.5,length(coul))
#acp=princomp(dfR,cor=TRUE)
options(repr.plot.width=4, repr.plot.height=4)
coul[atypLofInd]=2
taille[atypLofInd]=.8
plot(res.pca$ind$coord,col=coul, pch=17+coul-2,cex=taille, main = 'LOF')
legend("topright",legend=c('Outliers','Ind','DepSeuil'),text.col=c(2:4))
####### SVM ########
library(kernlab)
dfOcc=ksvm(x=as.matrix(dfR),y=NULL,type="one-svc",
              kernel="rbfdot",nu = 0.005)
atypOcc=!fitted(dfOcc)
dfOcc
coul=as.integer(df[,"DepSeuil"])+2
taille=rep(.5,length(coul))
options(repr.plot.width=4, repr.plot.height=4)
coul[atypOcc]=2
taille[atypOcc]=0.8
plot(res.pca$ind$coord,col=coul, pch=17+coul-2,cex=taille, main = 'SVM')
legend("topright",legend=c('Outliers','Ind','DepSeuil'),text.col=c(2:4))
library(randomForest)

dfRF=randomForest(X,Y,proximity=TRUE)
atypRF=outlier(dfRF)
options(repr.plot.width=2, repr.plot.height=3)
boxplot(atypRF)
atypRFInd=which(atypRF>4)
coul=as.numeric(Y)+2
options(repr.plot.width=4, repr.plot.height=4)
plot(atypRF,type="h",col=coul, main = 'RF')
legend("topright",legend=levels(Y),text.col=c(3:4))
coul=as.integer(df[,"DepSeuil"])+2
taille=rep(.5,length(coul))
#acp=princomp(dfR,cor=TRUE)
options(repr.plot.width=4, repr.plot.height=4)
coul[atypRFInd]=2
taille[atypRFInd]=.8
plot(res.pca$ind$coord,col=coul, pch=17+coul-2,cex=taille, main ='RF')
legend("topright",legend=c('Outliers','Ind','DepSeuil'),text.col=c(2:4))
#### RD non supervisé
library(randomForest)
set.seed(10)
dfURF <- randomForest(x=dfR,y=NULL,proximity=TRUE)
atypURF=outlier(dfURF)
atypURF
options(repr.plot.width=2, repr.plot.height=3)
boxplot(atypURF)
atypURFInd=which(atypURF>1.5)
coul=as.numeric(Y)+2
options(repr.plot.width=4, repr.plot.height=4)
plot(atypURF,type="h",col=coul)
legend("topright",legend=levels(Y),text.col=c(3:4))
coul=as.integer(df[,"DepSeuil"])+2
taille=rep(.5,length(coul))
options(repr.plot.width=4, repr.plot.height=4)
coul[atypURFInd]=2
taille[atypURFInd]=.8
plot(res.pca$ind$coord,col=coul, pch=17+coul-2,cex=taille, main = 'URF')
legend("topright",legend=c('Outliers','Ind','DepSeuil'),text.col=c(2:4))


## contains functions for multivariate outlier detection/removal
library(mvoutlier) # used for chi-square plots
library(robustbase)

###########################################
# Multivariate outlier detection using robust Mahalanobis distance
# calculated using minimum covariance determinent (MCD).
# Outliers are identified as points that cause the distribution
# of robust distances to deviate from a chi-squared distribution
# as follows: a linear model was fit to the ordered robust distances
# against the quantiles of a chi-squared distribution. Subsequently,
# outliers were identified by iteratively removing the point
# corresponding to the largest robust distance until the coefficient
# of the determination for the linear model passes a threshold.
# Inputs:
#        dat: a 2D matrix of size nxp, where n is the number of
#            observations and p is the number of variables.
#        adj.r.square.thresh: threshold on adjusted r-square on the
#            linear model.
#        plt: boolean value indicating whether to plot qq-plots
#        title: title of the qq-plots
# Output:
#        a boolean vector of length n, where TRUE indicates outlier
##########################################
outlier.mcd.qq_chisq <- function(dat, adj.r.square.thresh = 0.9, plt = F, plot_title = ''){
  mcd <- covMcd(dat, nsamp = "deterministic")
  if ( is.list(mcd$singularity) )
    stop(strwrap(" Error in solve.default(cov, ...) : 
  le système est numériquement singulier "))
  cm <- mcd$center
  S <- mcd$cov
  # Take square root to get the distance
  d <- sqrt(apply(dat, 1, function(x) t(x-cm) %*% solve(S) %*% (x-cm)))
  
  outlier <- logical(length(d))
  
  x <- sort(d^2)
  y <- qchisq(ppoints(length(d)),df = ncol(dat))
  model <- lm(y ~ x)
  m <- summary(model)$coefficients['x','Estimate']
  r <- summary(model)$adj.r.squared
  if (plt){
    qqplot(x,y)
    abline(model$coefficients)
    title(paste(plot_title,'\n','slope=',m,'\n','adj.r.squared=',r))
  }
  while ((r<adj.r.square.thresh) & (sum(!is.na(d))>2)){
    outlier[which.max(d)] <- T
    d[which.max(d)] <- NA
    x <- sort(d^2)
    
    y <- qchisq(ppoints(sum(! is.na(d))),df = ncol(dat))
    model <- lm(y ~ x)
    m <- summary(model)$coefficients['x','Estimate']
    r <- summary(model)$adj.r.squared
    if (plt){
      qqplot(x,y)
      abline(model$coefficients)
      title(paste(plot_title,'\n','slope=',m,'\n','adj.r.squared=',r))
    }
  }
  return(outlier)
}
mcd <- covMcd(dat, nsamp = "deterministic")
mcd
cm <- mcd$center
cm
S <- mcd$cov
solve(S)
d <- sqrt(apply(df, 1, function(x) {t(x-cm) %*% solve(S) %*% (x-cm)}))
outlier.mcd.qq_chisq(df, 0.9, F, 'Mahalanobis')









# ACP réduite
acp=princomp(df,cor=TRUE)
# Décroissance des valeurs propres
options(repr.plot.width=6, repr.plot.height=3)
par(mfrow = c(1,2))
plot(acp); boxplot(data.frame(acp$scores))
options(repr.plot.width=6, repr.plot.height=6)
par(mfrow = c(1,1))
biplot(acp, col=c("grey","blue"))
#############################
res.acp=dudi.pca(df[,c(1:6)],scale =TRUE)
### Extraire les valeurs propres ####
round(res.acp$eig,3)
#### Etude de l'inertie et calcul des % ###
round(res.acp$eig/sum(res.acp$eig)*100,3)
round(cumsum(100*res.acp$eig/sum(res.acp$eig)),3) ### cumul %
# Analyse des variables 
inertie = inertia.dudi(res.acp,col.inertia = TRUE)
round(res.acp$co,3)
## Contribution des variables en %
round(inertie$col.abs/100,3)
# Qualité des variables en %
round(inertie$col.re/100,3)

## cercle des corrélation 
res.acp$co[,1]=res.acp$co[,1]
res.acp$co[,2]=-res.acp$co[,2]
s.corcircle(res.acp$co, xax = 1, yax = 2)
res.acp$li[,1]=res.acp$li[,1]
res.acp$li[,2]=-res.acp$li[,2]
s.label(res.acp$li, xax = 1, yax = 2)
#s.class(dfxy = res.acp$li, fac = bhp$V22, col = c('red', 'blue','green','purple'), xax=1,yax=2)
# Analyse des observations `
A =round(res.acp$li,3)
which.min(A[,1]) ## Observation 584
A[584,] ## -6.395, -1.102
which.max(A[,1])## observation 447
A[447,] #6.12, 0.157
which.min(A[,2])## observation 201
A[565,] # -2.516,-5.125
which.max(A[,2])## observation 209
A[447,] # -5.234, 5.798
A[571,] # 5.534, 3.251
A[1069,]# -5.234, 5.798
A[1061,] # -2.516, -5.125
A[869,]# 6.12, 0.157
A[882,]#-6.395, -1.102
biplot(res.acp$li,res.acp$co,xlab = 'Axe 1',ylab = 'Axe 2')
res.acp$li
res.acp$co
