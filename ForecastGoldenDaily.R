suppressMessages(library( fields))
suppressMessages(library( fda))
suppressMessages(library( lubridate))

setwd("~/Dropbox/Home/Teaching/FDA/theCourse/Modules/Mod07FunctionalLinearModels")

# load the Golden data
load("~/Dropbox/Home/Teaching/FDA/data/GOzone2021.rda")

# reshape as day and hour

tDay<- 1:365
load("O3CompleteExample.rda")
# SVD of data,  day by hour
lookSVD<- svd( O3Complete)
#print( lookSVD$d)
D<- diag(lookSVD$d)
# adjust signs to be more interpretable
# 
V<- -1*lookSVD$v
U<- -1*lookSVD$u
# weighted basis functions (columns)
VB<- V %*%D

# How well do 4 PCs match the daily cycle

O3PC4<- U[,1:4]%*%t(VB[,1:4])

RMSE<-  sqrt( 
             rowSums( O3Complete - O3PC4)^2
             )
plot( tDay, (RMSE))

N<- nrow( U)
UY<- U[-1,]
UX<- U[-N,]

par( mar=c(3,3,2,1))
set.panel(4,4)
for(j in 1:4)
for( k in 1:4){
  #fields.style()
matplot( UX[,j], UY[,k], pch=16, cex=.5)
title(paste("lagged PC", j, " predict PC", k))
}

UX4<- UX[,1:4]
look1<- lm(UY[,1:4] ~  UX4 )

sum1<- summary( look1)
for( k in 1:4){
cat( fill=TRUE)
cat( "PC", k, "summary", fill=TRUE)
print(sum1[[k]]$coefficients)
}

UHat<- look1$fitted.values
O3PC4Hat<- UHat%*%t(VB[,1:4])

RMSEPredict<-  sqrt( 
  rowMeans( O3Complete[-1,] - O3PC4Hat)^2
)

stats( cbind(RMSE[-1], RMSEPredict) )





