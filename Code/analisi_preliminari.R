library(readr)
library(tree)
library(caret)
library(pROC)
library(MASS)
library(mlbench)
library(corrplot)
pulsar_stars <- read_csv("C:/Users/aughi/Desktop/inferenza/predicting-a-pulsar-star/pulsar_stars.csv")
pulsar_stars<-pulsar_stars[complete.cases(pulsar_stars),]
Real_Names=c("mean of IP","StdDev of IP", "Excess Kurtosis of IP","Skewness of IP","mean of DM_SNR","StdDev of DM_SNR", "Excess Kurtosis of DM_SNR","Skewness of DM_SNR")
pulsar_stars <- setNames(as.data.frame(pulsar_stars), c(paste0("Var", 1:8),"True"))
attach(pulsar_stars)
#costruzione Training Set e Test Set
train.index <- createDataPartition(pulsar_stars$True, p = .75, list = FALSE)
train <- pulsar_stars[ train.index,]
test  <- pulsar_stars[-train.index,]


###dimensioni dataset###
par(mfcol=c(2,3))
height0=c(length(pulsar_stars[which(pulsar_stars$True==0),][,1]),length(pulsar_stars[which(pulsar_stars$True==1),][,1]))
height1=c(length(train[which(train$True==0),][,1]),length(train[which(train$True==1),][,1]))
height2=c(length(test[which(test$True==0),][,1]),length(test[which(test$True==1),][,1]))
table0=table(pulsar_stars$True)
cols=c("red","green")
labs=c("not a pulsar","pulsar")
pie(table0,main= "DATASET",labels = paste(labs,table0),col = cols)
barplot(height0, horiz = TRUE,width=0.2,col = cols)
table1=table(train$True)
pie(table1,main= "TRAINSET",labels = paste(labs,table1),col = cols)
barplot(height1, horiz = TRUE,col = cols)
table2=table(test$True)
pie(table2,main= "TESTSET",labels = paste(labs,table2),col = cols)
barplot(height2, horiz = TRUE,col = cols)

#####distribuzione delle variabili####
par(mfcol=c(1,1))
hist(train$Var1, freq=FALSE,main="Distribuzione della Media del Profilo integrato", xlab = "Media del profilo integrato")
lines(density(train$Var1))

hist(train$Var2, freq=FALSE,main="Distribuzione della deviazione std del Profilo integrato", xlab = "deviazione std del Profilo integrato")
lines(density(train$Var2))

hist(train$Var3, freq=FALSE,main="Distribuzione del curtosi del Profilo integrato", xlab = "curtosi del Profilo integrato")
lines(density(train$Var3))

hist(train$Var4, freq=FALSE,main="Distribuzione della skewness del Profilo integrato", xlab = "skewness del Profilo integrato")
lines(density(train$Var4))

hist(train$Var5, freq=FALSE,main="Distribuzione della Media della DM_SNR", xlab = "Media della DM_SNR")
lines(density(train$Var5))

hist(train$Var6, freq=FALSE,main="Distribuzione della dev std del DM_SNR", xlab = "dev std del DM_SNR")
lines(density(train$Var6))

hist(train$Var7, freq=FALSE,main="Distribuzione della curtosi del DM_SNR", xlab = "curtosi del DM_SNR")
lines(density(train$Var7))

hist(train$Var8, freq=FALSE,main="Distribuzione della skewness del DM_SNR", xlab = "skewness del DM_SNR")
lines(density(train$Var8))



##BOXPLOT#########################
par(mfcol=c(1,2))
boxplot((train[which(train$True==0),])$Var1, xlab="class 0",ylab="mean of integrated profile")
abline(h=mean(train$Var1))
boxplot((train[which(train$True==1),])$Var1, xlab="class 1",ylab="mean of integrated profile")
abline(h=mean(train$Var1))

par(mfcol=c(1,2))
boxplot((train[which(train$True==0),])$Var2, xlab="class 0",ylab="devstd of integrated profile")
abline(h=mean(train$Var2))
boxplot((train[which(train$True==1),])$Var2, xlab="class 1",ylab="devstd of integrated profile")
abline(h=mean(train$Var2))

par(mfcol=c(1,2))
boxplot((train[which(train$True==0),])$Var3, xlab="class 0",ylab="Excess Kurthosis of integrated profile")
abline(h=mean(train$Var3))
boxplot((train[which(train$True==1),])$Var3, xlab="class 1",ylab="Excess Kurthosis of integrated profile")
abline(h=mean(train$Var3))

par(mfcol=c(1,2))
boxplot((train[which(train$True==0),])$Var4, xlab="class 0",ylab="skewness of integrated profile")
abline(h=mean(train$Var4))
boxplot((train[which(train$True==1),])$Var4, xlab="class 1",ylab="skewness of integrated profile")
abline(h=mean(train$Var4))

par(mfcol=c(1,2))
boxplot((train[which(train$True==0),])$Var5, xlab="class 0",ylab="mean of DM-SNR")
abline(h=mean(train$Var5))
boxplot((train[which(train$True==1),])$Var5, xlab="class 1",ylab="mean of DM-SNR")
abline(h=mean(train$Var5))

par(mfcol=c(1,2))
boxplot((train[which(train$True==0),])$Var6, xlab="class 0",ylab="devstd of DM-SNR")
abline(h=mean(train$Var6))
boxplot((train[which(train$True==1),])$Var6, xlab="class 1",ylab="devstd of DM-SNR")
abline(h=mean(train$Var6))

par(mfcol=c(1,2))
boxplot((train[which(train$True==0),])$Var7, xlab="class 0",ylab="Excess Kurthosis of DM-SNR")
abline(h=mean(train$Var7))
boxplot((train[which(train$True==1),])$Var7, xlab="class 1",ylab="Excess Kurthosis of DM-SNR")
abline(h=mean(train$Var7))

par(mfcol=c(1,2))
boxplot((train[which(train$True==0),])$Var8, xlab="class 0",ylab="skewness of DM-SNR")
abline(h=mean(train$Var8))
boxplot((train[which(train$True==1),])$Var8, xlab="class 1",ylab="skewness of DM-SNR")
abline(h=mean(train$Var8))

###BArPLOT###############
par(mfrow=c(1,1))
colours=c("darkgreen","blue","orchid","orange","red","pink","yellow","brown")
Var=c("Var1","Var2","Var3","Var4","Var5","Var6","Var7","Var8")
mean_height0=c(mean(train[which(train$True==0),][,1]),mean(train[which(train$True==0),][,2]),mean(train[which(train$True==0),][,3]),
               mean(train[which(train$True==0),][,4]),mean(train[which(train$True==0),][,5]),mean(train[which(train$True==0),][,6]),
               mean(train[which(train$True==0),][,7]),mean(train[which(train$True==0),][,8]))
mean_height1=c(mean(train[which(train$True==1),][,1]),mean(train[which(train$True==1),][,2]),mean(train[which(train$True==1),][,3]),
               mean(train[which(train$True==1),][,4]),mean(train[which(train$True==1),][,5]),mean(train[which(train$True==1),][,6]),
               mean(train[which(train$True==1),][,7]),mean(train[which(train$True==1),][,8]))
barplot(mean_height0,names.arg=Var,beside=TRUE,col=colours,xlab="class 0", ylab="mean of the predictor",ylim=c(0,120))
barplot(mean_height1,names.arg=Var,beside=TRUE,col=colours,xlab="class 1", ylab="mean of the predictor",ylim=c(0,120))