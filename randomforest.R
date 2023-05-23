#loading all the packages
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(cowplot)

#Reading the cleaned data onto a dataframe
randf <- read.csv("pastUse.csv", header = T)
mydata <-randf[!(randf$EV_Q02=="Valid Skip" | randf$EV_Q02=="Dont Know"| randf$EV_Q02=="Refusal"),]
#mydata <-randf[!(randf$isLabourForce=="Not in the labour force"),]

mydata
str(mydata)

index = sample(2, nrow(mydata), replace=T,prob=c(0.7,0.3))
trainrf = mydata[index==1,]
testrf = mydata[index==2,]

trainrf$GCAGEGR6 <- as.factor(trainrf$GCAGEGR6)
trainrf$CSEX <- as.factor(trainrf$CSEX)
trainrf$PROVINCE <- as.factor(trainrf$PROVINCE)
trainrf$G_CEDUC <- as.factor(trainrf$G_CEDUC)
trainrf$G_CLFSST <- as.factor(trainrf$G_CLFSST)
trainrf$PU_Q03 <- as.factor(trainrf$PU_Q03)
trainrf$PU_Q06E <- as.factor(trainrf$PU_Q06E)
trainrf$PU_Q06J <- as.factor(trainrf$PU_Q06J)
trainrf$PU_Q06K <- as.factor(trainrf$PU_Q06K)
trainrf$PU_G06 <- as.factor(trainrf$PU_G06)
trainrf$PU_Q06A <- as.factor(trainrf$PU_Q06A)
trainrf$G_URBRUR <- as.factor(trainrf$G_URBRUR)
trainrf$G_HSTUD <- as.factor(trainrf$G_HSTUD)
trainrf$PU_Q02 <- as.factor(trainrf$PU_Q02)
trainrf$PU_Q01 <- as.factor(trainrf$PU_Q01)
trainrf$EV_Q01 <- as.factor(trainrf$EV_Q01)
trainrf$EV_Q02 <- as.factor(trainrf$EV_Q02)
trainrf$G_HEDUC <- as.factor(trainrf$G_HEDUC)
trainrf$G_HHSIZE <- as.integer(trainrf$G_HHSIZE)
trainrf$GFAMTYPE <- as.factor(trainrf$GFAMTYPE)
trainrf$G_CSTUD <- as.factor(trainrf$G_CSTUD)

testrf$GCAGEGR6 <- as.factor(testrf$GCAGEGR6)
testrf$CSEX <- as.factor(testrf$CSEX)
testrf$PROVINCE <- as.factor(testrf$PROVINCE)
testrf$G_CEDUC <- as.factor(testrf$G_CEDUC)
testrf$G_CLFSST <- as.factor(testrf$G_CLFSST)
testrf$PU_Q03 <- as.factor(testrf$PU_Q03)
testrf$PU_Q06E <- as.factor(testrf$PU_Q06E)
testrf$PU_Q06J <- as.factor(testrf$PU_Q06J)
testrf$PU_Q06K <- as.factor(testrf$PU_Q06K)
testrf$PU_G06 <- as.factor(testrf$PU_G06)
testrf$PU_Q06A <- as.factor(testrf$PU_Q06A)
testrf$G_URBRUR <- as.factor(testrf$G_URBRUR)
testrf$G_HSTUD <- as.factor(testrf$G_HSTUD)
testrf$PU_Q02 <- as.factor(testrf$PU_Q02)
testrf$PU_Q01 <- as.factor(testrf$PU_Q01)
testrf$EV_Q01 <- as.factor(testrf$EV_Q01)
testrf$EV_Q02 <- as.factor(testrf$EV_Q02)
testrf$G_HEDUC <- as.factor(testrf$G_HEDUC)
testrf$G_HHSIZE <- as.integer(testrf$G_HHSIZE)
testrf$GFAMTYPE <- as.factor(testrf$GFAMTYPE)
testrf$G_CSTUD <- as.factor(testrf$G_CSTUD)



rfmodel <- randomForest(EV_Q02~GCAGEGR6+CSEX , data = trainrf, proximity = TRUE)
rfmodel
