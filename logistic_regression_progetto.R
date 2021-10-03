library(rms)
library(readr)
library(ResourceSelection)
pulsar_stars <- read_csv("C:/Users/aughi/Desktop/inferenza/predicting-a-pulsar-star/pulsar_stars.csv")
View(pulsar_stars)

attach(pulsar_stars)

pulsar_stars<-pulsar_stars[complete.cases(pulsar_stars),]
Real_Names=c("mean of IP","StdDev of IP", "Excess Kurtosis of IP","Skewness of IP","mean of DM_SNR","StdDev of DM_SNR", "Excess Kurtosis of DM_SNR","Skewness of DM_SNR")
pulsar_stars <- setNames(as.data.frame(pulsar_stars), c(paste0("Var", 1:8),"True"))

#costruzione Training Set e Test Set
train.index <- createDataPartition(pulsar_stars$True, p = .75, list = FALSE)
train <- pulsar_stars[ train.index,]
test  <- pulsar_stars[-train.index,]


m=min(test$Var3)
M=max(test$Var3)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar3 = cut( test$Var3, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar3, mean )
plot( test$Var3, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'Excess Kurthosis of integrated profile', ylab = 'class', main = 'verifica logregressione su var3', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )



m=min(test$Var1)
M=max(test$Var1)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar1 = cut( test$Var1, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar1, mean )
plot( test$Var1, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'mean of integrated profile', ylab = 'class', main = 'verifica logregressione su var1', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )



m=min(test$Var2)
M=max(test$Var2)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar2 = cut( test$Var2, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar2, mean )
plot( test$Var2, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'Devstd of integrated profile', ylab = 'class', main = 'verifica logregressione su var2', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

m=min(test$Var4)
M=max(test$Var4)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar4 = cut( test$Var4, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar4, mean )
plot( test$Var4, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'skewness of integrated profile', ylab = 'class', main = 'verifica logregressione su var4', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

m=min(test$Var5)
M=max(test$Var5)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar5 = cut( test$Var5, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar5, mean )
plot( test$Var5, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'Mean of DN-SMR curve', ylab = 'class', main = 'verifica logregressione su var5', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

m=min(test$Var6)
M=max(test$Var6)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar6 = cut( test$Var6, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar6, mean )
plot( test$Var5, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'Devstd of DN-SMR curve', ylab = 'class', main = 'verifica logregressione su var6', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

m=min(test$Var7)
M=max(test$Var7)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar7 = cut( test$Var7, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar7, mean )
plot( test$Var7, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'Excess Kurthosis of DN-SMR curve', ylab = 'class', main = 'verifica logregressione su var7', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

m=min(test$Var8)
M=900.5457 #max è outlier?
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar8 = cut( test$Var8, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar8, mean )
plot( test$Var5, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'skewness of DN-SMR curve', ylab = 'class', main = 'verifica logregressione su var8', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )


## profilo integrato buono per regressione logistica, DN-SMR curve ?
nnn


mod = glm( test$True ~ test$Var3, family = binomial( link = logit ) )
summary( mod )
m=min(test$Var3)
M=max(test$Var3)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar1 = cut( test$Var3, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar1, mean )
plot( test$Var3, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'mean of integrated profile', ylab = 'class', main = 'verifica logregressione su var3', lwd = 2, cex = 1.5 )
points(test$Var3,mod$fitted,col='orange',pch=0.5)
points( mid, y, col = "blue", pch = 16 )

mod = glm( test$True ~ test$Var2, family = binomial( link = logit ) )
summary( mod )
m=min(test$Var2)
M=max(test$Var2)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar2 = cut( test$Var2, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar2, mean )
plot( test$Var2, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'devstd of integrated profile', ylab = 'class', main = 'verifica logregressione su var2', lwd = 2, cex = 1.5 )
points(test$Var2,mod$fitted,col='orange',pch=0.5)
points( mid, y, col = "blue", pch = 16 )

mod = glm( test$True ~ test$Var4, family = binomial( link = logit ) )
summary( mod )
m=min(test$Var4)
M=max(test$Var4)
range=M-m
cost=range/10
x = c( m, m+cost, m+2*cost,m+3*cost ,m+4*cost, m+5*cost, m+6*cost, m+7*cost,m+8*cost, m+9*cost,m+10*cost )
# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
# Suddividiamo i dati nelle classi che abbiamo creato
GRVar4 = cut( test$Var4, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( test$True, GRVar4, mean )
plot( test$Var4, test$True, pch = ifelse( test$True == 1, 3, 4 ),
      col = ifelse( test$True == 1, 'forestgreen', 'red' ),
      xlab = 'skewness of integrated profile', ylab = 'class', main = 'verifica logregressione su var4', lwd = 2, cex = 1.5 )
points(test$Var4,mod$fitted,col='orange',pch=0.5)
points( mid, y, col = "blue", pch = 16 )

####regressione multipla
modmultcompl= glm( train$True ~.,data=train,family = binomial( link = logit ) )
summary( modmultcompl)
# estimate variable importance
importance <- varImp(modmultcompl, scale=FALSE)
# summarize importance
print(importance)
# plot importance
barplot(importance$Overall, width = 1, space = NULL,
        names.arg = c("Var1","Var2","Var3","Var4","Var5","Var6","Var7","Var8"), col=c(1:8), main= "importance of variables")
hoslem.test( modmultcompl$y, fitted( modmultcompl), g = 9)
plot(modmultcompl$coefficients,main = "coefficienti regressione logistica",xlab="variables", ylab="value")
abline(h=0,col="red")


modmultsign= glm( train$True ~ Var1+Var2+Var3+Var4+Var5+Var6+ Var8,data=train,family = binomial( link = logit ) )
summary( modmultsign)
# estimate variable importance
importance <- varImp(modmultsign, scale=FALSE)
# summarize importance
print(importance)
# plot importance
barplot(importance$Overall, width = 1, space = NULL,
        names.arg = c("Var1","Var2","Var3","Var4","Var5","Var6","Var8"), col=c(1:6), main= "importance of variables")
hoslem.test( modmultsign$y, fitted( modmultsign), g = 8 )



modmult1= glm( train$True ~ Var3+Var4,data=train,family = binomial( link = logit ) )
# estimate variable importance
importance <- varImp(modmult1, scale=FALSE)
# summarize importance
print(importance)
# plot importance
barplot(importance$Overall, width = 1, space = NULL,
        names.arg = c("Var3","Var4"), col=c(1:4), main= "importance of variables")
summary( modmult1)
hoslem.test( modmult1$y, fitted( modmult1), g = 3 )


step(modmultcompl)

#cook distance
n=dim(train)[1]
p=8
Cdist = cooks.distance( modmultcompl )
watchout_ids_Cdist = which( Cdist > 4/(n-p) )
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist
plot( modmultcompl$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( modmultcompl$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], col = 'green', pch = 16 )
id_to_keep = !( 1:n %in% watchout_ids_Cdist )
modmultcomplcd= glm( train[id_to_keep,]$True ~., train[id_to_keep, ],family = binomial( link = logit ) )





