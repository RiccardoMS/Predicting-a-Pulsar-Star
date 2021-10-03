


library(tree)
library(caret)
library(pROC)



### modello completo
setup=tree.control(nrow(train), mindev=0.01) 
soglia = 0.5
tree_model_compl <- tree( as.factor(True) ~., data=train,control=setup)
summary(tree_model_compl)
plot(tree_model_compl,lwd=3, main="albero 0 modello completo")
text(tree_model_compl,pretty=0,cex=1.2,col="blue")

set.seed (3)                                   #cross validation modello completo
cv.tree_model_compl =cv.tree(tree_model_compl ,FUN=prune.misclass)
plot(cv.tree_model_compl$size,cv.tree_model_compl$dev,type="b", lwd=3,col="blue",xlab="Nodi terminali", ylab="RSS",main="Cost complexity pruning modello completo" )
#ottengo numero best di nodi 
prune.tree_model_compl=prune.tree(tree_model_compl, best =2)
plot(prune.tree_model_compl,lwd=3, main = "Albero classificazione potato modello completo")
text(prune.tree_model_compl ,pretty =0,cex=1.2,col="blue")

previsione_mod_compl=predict(prune.tree_model_compl,newdata=test)
valori.reali = test$True
valori.predetti = as.numeric( previsione_mod_compl[,1] < soglia )
confusionMatrix_modcompl= table( valori.reali, valori.predetti )
confusionMatrix_modcompl
perc_casicorretti=round( sum( diag( confusionMatrix_modcompl ) ) / sum( confusionMatrix_modcompl ), 2 )
perc_casicorretti
perc_misclass=round( ( confusionMatrix_modcompl[ 1, 2 ] +  confusionMatrix_modcompl[ 2, 1 ] ) / sum( confusionMatrix_modcompl ), 2 )
perc_misclass
sensitivita_modcompl = confusionMatrix_modcompl [ 2, 2 ] /( confusionMatrix_modcompl [ 2, 1 ] + confusionMatrix_modcompl [ 2, 2 ] )
sensitivita_modcompl
specificita_modcompl = confusionMatrix_modcompl [ 1, 1 ] /( confusionMatrix_modcompl [ 1, 2 ] + confusionMatrix_modcompl [ 1, 1 ] )
specificita_modcompl

### modello integrated profile
setup=tree.control(nrow(train), mindev=0.01)
soglia = 0.5
tree_model_1 <- tree( as.factor(True) ~ Var1 + Var2+ Var3 +Var4, data=train, control=setup)
summary(tree_model_1)
plot(tree_model_1,lwd=3,main="albero 0 modello 1")
text(tree_model_1,pretty=0,cex=1.2,col="blue")

set.seed (3)                                   #cross validation modello completo
cv.tree_model_1 =cv.tree(tree_model_1,FUN=prune.misclass )
plot(cv.tree_model_1$size,cv.tree_model_1$dev,type="b", lwd=3,col="blue",xlab="Nodi terminali", ylab="RSS",main="Cost complexity pruning modello 1" )
#ottengo numero best di nodi 
prune.tree_model_1=prune.tree(tree_model_1, best =2)
plot(prune.tree_model_1,lwd=3, main = "Albero classificazione potato modello integrated profile")
text(prune.tree_model_1 ,pretty =0,cex=1.2,col="blue")

previsione_mod_1=predict(prune.tree_model_1,newdata=test)
valori.reali1 = test$True
valori.predetti1 = as.numeric( previsione_mod_1[,1] < soglia )
confusionMatrix_mod1= table( valori.reali1, valori.predetti1 )
confusionMatrix_mod1
perc_casicorretti=round( sum( diag( confusionMatrix_mod1 ) ) / sum( confusionMatrix_mod1 ), 2 )
perc_casicorretti
perc_misclass=round( ( confusionMatrix_mod1[ 1, 2 ] +  confusionMatrix_mod1[ 2, 1 ] ) / sum( confusionMatrix_mod1 ), 2 )
perc_misclass
sensitivita_mod1 = confusionMatrix_mod1 [ 2, 2 ] /( confusionMatrix_mod1 [ 2, 1 ] + confusionMatrix_mod1 [ 2, 2 ] )
sensitivita_mod1
specificita_mod1 = confusionMatrix_mod1 [ 1, 1 ] /( confusionMatrix_mod1 [ 1, 2 ] + confusionMatrix_mod1 [ 1, 1 ] )
specificita_mod1

### modello DSM
setup=tree.control(nrow(train), mindev=0.01)
soglia = 0.5
tree_model_2 <- tree( as.factor(True) ~ Var5 + Var6  + Var7+Var8, data=train, control=setup)
summary(tree_model_2)
plot(tree_model_2,lwd=3, main="albero 0 modello 2")
text(tree_model_2,pretty=0,cex=1.2,col="blue")

set.seed (3)                                   #cross validation modello completo
cv.tree_model_2 =cv.tree(tree_model_2,FUN=prune.misclass )
plot(cv.tree_model_2$size,cv.tree_model_2$dev,type="b", lwd=3,col="blue",xlab="Nodi terminali", ylab="RSS",main="Cost complexity pruning modello 2" )
#ottengo numero best di nodi 
prune.tree_model_2=prune.tree(tree_model_2, best =5)
plot(prune.tree_model_2,lwd=3, main = "Albero classificazione potato modello DM-SNR curve")
text(prune.tree_model_2,pretty =0,cex=1.2,col="blue")

previsione_mod_2=predict(prune.tree_model_2,newdata=test)
valori.reali2 = test$True
valori.predetti2 = as.numeric( previsione_mod_2[,1] < soglia )
confusionMatrix_mod2= table( valori.reali2, valori.predetti2 )
confusionMatrix_mod2
perc_casicorretti=round( sum( diag( confusionMatrix_mod2 ) ) / sum( confusionMatrix_mod2 ), 2 )
perc_casicorretti
perc_misclass=round( ( confusionMatrix_mod2[ 1, 2 ] +  confusionMatrix_mod2[ 2, 1 ] ) / sum( confusionMatrix_mod2 ), 2 )
perc_misclass
sensitivita_mod2 = confusionMatrix_mod2 [ 2, 2 ] /( confusionMatrix_mod2 [ 2, 1 ] + confusionMatrix_mod2 [ 2, 2 ] )
sensitivita_mod2
specificita_mod2 = confusionMatrix_mod2 [ 1, 1 ] /( confusionMatrix_mod2 [ 1, 2 ] + confusionMatrix_mod2 [ 1, 1 ] )
specificita_mod2


