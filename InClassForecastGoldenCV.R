suppressMessages(library( fields))
suppressMessages(library( fda))
suppressMessages(library( lubridate))

setwd("~/Dropbox/Home/Teaching/FDA/theCourse/Modules/Mod07FunctionalLinearModels")

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

# omit the first day everwhere to make this easy to match up later
O3Target<- O3Complete[-1,]
tDay<- 2:365
# How well do 4 PCs match the daily cycle

O3PC4<- U[,1:4]%*%t(VB[,1:4])
O3PC4<- O3PC4[-1,]

# oracle RMSE
RMSE<-  sqrt( 
             rowSums( O3Target - O3PC4)^2
             )

# set up regression with lagged X's
N<- nrow( U)
UY<- U[-1,] # tommorrow
UX<- U[-N,] # today
UX4<- UX[,1:4] # use 4 PCS/basis functions 

# lm can fit all the models at once!
fitPC4<- lm(UY[,1:4] ~  UX4 )


# note fitted.values are the predicted U's
O3PC4<- fitPC4$fitted.values%*%t(VB[,1:4])
dim(O3PC4)
RMSEPredict<-  sqrt( 
  rowMeans( O3Target - O3PC4)^2
)

stats( cbind(RMSE, RMSEPredict) )

# OK now use CV to get a more accurate estimte of RMSE in 
# case of over fitting

# pesky indexing for 10 fold CV

IGrid<- seq( 1,364,36)

L<- length( IGrid)-1
IStart<- IGrid[-(L+1)]

IEnd<- IStart + (36-1)
# makes sure last interval is right 
IEnd[L]<- 364
cbind( IStart, IEnd)

#
#NCV<- L
# NCV <- 1
NCV<- 30
RMSECV<- rep(NA,NCV)
#
set.seed(498)
 
for( k in 1: NCV){
  cat(k, " ")
# this is bad ---  set.seed(498)
# matt's idea 
# I<- 255:364
# Ethan's idea with help from Tori
 I<- sample( 1:364,36,replace=FALSE)
# leave-one-out CV
#  I<- k
# this is 10 fold   
#I<- IStart[k]:IEnd[k]
  
# data for training 
YCV<- UY[-I,1:4]
XCV<- UX4[-I,] 
#
fitCV<- lm(YCV ~ XCV )
UHatCV<-  cbind( 1,UX4[I,]) %*%fitCV$coefficients
# special case of leave-one-out
#UHatCV<-  c( 1,UX4[I,]) %*%fitCV$coefficients
O3HatCV<- UHatCV%*%t(VB[,1:4])
RMSECV[k]<- sqrt( mean(c((O3Target[I,] - O3HatCV)^2 )) )
}

stats( RMSECV)  

