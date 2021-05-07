#################################################
# Chapter 1
#################################################
library(bayesm)
library(dummies)
library(pROC)

load("stc-cbc-respondents-v3(1).RData")
str(resp.data.v3)

taskV3 <- read.csv("stc-dc-task-cbc -v3(1).csv", sep="\t")
str(taskV3)

load("efCode.RData")
str(efcode.att.f)
str(efcode.attmat.f)

apply(resp.data.v3[4:39], 2, function(x){tabulate(na.omit(x))})

task.mat <- as.matrix(taskV3[, c("screen", "RAM", "processor", "price", "brand")])
dim(task.mat)
head(task.mat)

X.mat=efcode.attmat.f(task.mat) # Here is where we do effects coding
dim(X.mat)
head(X.mat)

pricevec=taskV3$price-mean(taskV3$price)
head(pricevec)
str(pricevec)

X.brands=X.mat[,9:11]
dim(X.brands)
str(X.brands)

X.BrandByPrice = X.brands*pricevec
dim(X.BrandByPrice)
str(X.BrandByPrice)

X.matrix=cbind(X.mat,X.BrandByPrice)
dim(X.matrix)
str(X.matrix)

X2.matrix=X.matrix[,1:2]
dim(X2.matrix)

det(t(X.matrix) %*% X.matrix)

ydata=resp.data.v3[,4:39]
names(ydata)
str(ydata)

ydata=na.omit(ydata)
str(ydata)

ydata=as.matrix(ydata)
dim(ydata)

zowner <- 1*(!is.na(resp.data.v3$vList3))

lgtdata = NULL
for (i in 1:424) { lgtdata[[i]]=list( y=ydata[i,],X=X.matrix )}
length(lgtdata)
str(lgtdata)

#################################################
# Chapter 2
#################################################

mcmctest=list(R=5000, keep=5)
Data1=list(p=3,lgtdata=lgtdata)
testrun1=rhierMnlDP(Data=Data1,Mcmc=mcmctest)
names(testrun1)

betadraw1=testrun1$betadraw
dim(betadraw1)

plot(1:length(betadraw1[1,1,]),betadraw1[1,1,])
plot(density(betadraw1[1,1,701:1000],width=2))

summary(betadraw1[1,1,701:1000])
betameansoverall <- apply(betadraw1[,,701:1000],c(2),mean)
betameansoverall

perc <- apply(betadraw1[,,701:1000],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc

#################################################
# Chapter 3
#################################################

zownertest=matrix(scale(zowner,scale=FALSE),ncol=1)
Data2=list(p=3,lgtdata=lgtdata,Z=zownertest)
testrun2=rhierMnlDP(Data=Data2,Mcmc=mcmctest)

dim(testrun2$deltadraw)

apply(testrun2$Deltadraw[701:1000,],2,mean)
apply(testrun2$Deltadraw[701:1000,],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))

betadraw2=testrun2$betadraw
dim(betadraw2)

#################################################
# Chapter 4
#################################################

betameans <- apply(betadraw1[,,701:1000],c(1,2),mean)
str(betameans)
dim(betameans)

xbeta=X.matrix%*%t(betameans)
dim(xbeta)

xbetamatrix=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbetamatrix)

expxbeta=exp(xbetamatrix)
rsumvec=rowSums(expxbeta)
pchoicemat=expxbeta/rsumvec
head(pchoicemat)
dim(pchoicemat)

custchoice <- max.col(pchoicemat)
str(custchoice)
head(custchoice)

ydatavec <- as.vector(t(ydata))
str(ydatavec)
table(custchoice,ydatavec)

roctest <- roc(ydatavec, custchoice, plot=TRUE)
auc(roctest)

logliketest <- testrun2$loglike
mean(logliketest)
m <- matrix(custchoice, nrow =36, byrow=F)
m2 <- t(m)
apply(m2, 2, function(x){tabulate(na.omit(x))})

##repeat this process for betadraw2##
betameans2 <- apply(betadraw2[,,701:1000],c(1,2),mean)
str(betameans2)
dim(betameans2)

xbeta2=X.matrix%*%t(betameans2)
dim(xbeta2)

xbetamatrix2=matrix(xbeta2,ncol=3,byrow=TRUE)
dim(xbetamatrix2)

expxbeta2=exp(xbetamatrix2)
rsumvec2=rowSums(expxbeta2)
pchoicemat2=expxbeta2/rsumvec2
head(pchoicemat2)
dim(pchoicemat2)

custchoice2 <- max.col(pchoicemat2)
str(custchoice2)
head(custchoice2)

ydatavec2 <- as.vector(t(ydata))
str(ydatavec2)
table(custchoice2,ydatavec2)

roctest2 <- roc(ydatavec2, custchoice2, plot=TRUE)
auc(roctest2)

logliketest2 <- testrun2$loglike
mean(logliketest2)
m_beta2 <- matrix(custchoice2, nrow =36, byrow=F)
m2_beta2 <- t(m_beta2)
apply(m2_beta2, 2, function(x){tabulate(na.omit(x))})

#################################################
# Chapter 5
#################################################

ex_scen <- read.csv("extra-scenarios(1).csv")
Xextra.matrix <- as.matrix(ex_scen[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9", "V10","V11","V12","V13","V14")])

betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)
xextrabeta=Xextra.matrix%*%(betavec) 
xbetaextra2=matrix(xextrabeta,ncol=3,byrow=TRUE)
dim(xbetaextra2)

expxbetaextra2=exp(xbetaextra2)
rsumvec=rowSums(expxbetaextra2)
pchoicemat=expxbetaextra2/rsumvec 
pchoicemat