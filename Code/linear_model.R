library( MASS )
library( corrplot )
library(ElemStatLearn)
library( faraway )
library( lars )
library( Matrix )
library(car)

pairs(pulsar_stars)       #modello lineare non sembra la scelta ottimale

cor_pulsar_stars=cor(pulsar_stars) #collinearita dei predittori
cor_pulsar_stars
corrplot( cor_pulsar_stars, method = 'ellipse' )

attach(pulsar_stars)

linear_reg=lm(True~., data=train)   #conferma collinearità
vif(linear_reg)

step(linear_reg,direction = "backward") #miglior modello lineare è quello completo

avPlots( linear_reg )                 #predittori più significativi sembrano essere Var3 var4

linear_model1=lm(True~Var3+Var4,data=train)
AIC(linear_reg,linear_model1)       #comuqnue meglio modello completo
anova(linear_reg,linear_model1)     #conferma test anova



#verifica ipotesi
par( mfrow = c( 2, 2 ), mar = c( 5, 4, 5, 4 ) + 0.1 )
plot( linear_reg)
fictitious_linear_reg=lm(True~.,data=test)
shapiro.test(fictitious_linear_reg$residuals)
