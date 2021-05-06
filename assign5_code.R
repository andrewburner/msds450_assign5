library(tidyverse)
library(bayesm)
library(dummies)
library("pROC")

# Reading in data files and then checking the first couple rows of each
respondent_df <- read.csv('stc-dc-task-cbc -v3(1).csv', sep = '\t')
head(respondent_df)

load('stc-cbc-respondents-v3(1).RData')
head(resp.data.v3)

extra_df <- read.csv('extra-scenarios-v3.csv', sep = ',')
head(extra_df)


# This loads the two efcode functions
load('efCode.RData')

# Prints the functions to the console to see what they do
efcode.att.f
efcode.attmat.f


task.mat <- respondent_df %>%
  select(screen, RAM, processor, price, brand) %>%
  as.matrix()

X.mat <- efcode.attmat.f(task.mat)

dim(task.mat)
dim(X.mat)

head(task.mat)

# In X.mat, cols 1&2 are for the screen variable
# cols 3&4 = RAM, cols 5&6 = processor
# cols 7&8 = price, cols 9-11 = brand
# A value of 0 for an attribute in task.mat = -1's for all corresponding
# columns in X.mat
head(X.mat)

pricevec <- (respondent_df %>% pull(price)) - mean(respondent_df$price)

X.brands <- X.mat[,9:11]
X.BrandByPrice <- X.brands*pricevec

X.matrix <- cbind(X.mat, X.BrandByPrice)

ydata <- resp.data.v3 %>%
  select(starts_with("DCM1_"), -dcm1_timer) %>% 
  as.matrix()

sum(is.na(ydata))

zowner <- 1 * (!is.na(resp.data.v3$vList3))

#####################################
# This is the end of step 2
#####################################

lgtdata = NULL # a starter placeholder for your list 
for (i in 1:424) {
lgtdata[[i]]=list(y=ydata[i,],X=X.matrix)
}

length(lgtdata)

lgtdata[[3]]

#####################################
# This is the end of chapter 1
#####################################

lgtdata100 <- lgtdata[1:100]
mcmctest <- list(R=5000,keep=5)
Data1 <- list(p=3,lgtdata=lgtdata100)

testrun1 <- rhierMnlDP(Data=Data1,Mcmc=mcmctest)

names(testrun1)
dim(testrun1$betadraw)

betadraw1 <- testrun1$betadraw
dim(betadraw1)

plot(1:length(betadraw1[1,1,]),betadraw1[1,1,])
plot(density(betadraw1[1,1,701:1000],width=2))
summary(betadraw1[1,1,701:1000])

apply(betadraw1[,,701:1000],c(2),mean)
apply(betadraw1[,,701:1000],c(1,2),mean)

summary((betadraw1[1,1,701:1000]-betadraw1[1,2,701:1000]))
plot(density(betadraw1[1,1,701:1000]-betadraw1[1,2,701:1000],width=2))

#####################################
# This is the end of chapter 2
#####################################

names(testrun1)

zownertest <- matrix(scale(zowner,scale=FALSE),ncol=1)
Data2 <- list(p=3,lgtdata=lgtdata,Z=zownertest)
testrun2 <- rhierMnlDP(Data=Data2,Mcmc=mcmctest)
names(testrun2)
dim(testrun2$Deltadraw)

apply(testrun2$Deltadraw[701:1000,],2,mean)
apply(testrun2$Deltadraw[701:1000,],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))

betadraw2 <- testrun2$betadraw
dim(betadraw2)

#####################################
# This is the end of chapter 3
#####################################

betameans <- apply(betadraw1[,,701:1000],c(1,2),mean) 
str(betameans)
dim(betameans)

xbeta <- X.matrix%*%t(betameans)
dim(xbeta)

xbeta2 <- matrix(xbeta,ncol=3,byrow=TRUE) 
dim(xbeta2)
expxbeta2 <- exp(xbeta2)
rsumvec <- rowSums(expxbeta2) 
pchoicemat <- expxbeta2/rsumvec 
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



