#curva ROC e AUC per modello completo
soglia_roc = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc = rep( NA, lens )
ordinata_roc = rep( NA, lens )
for ( k in 1 : lens )
{
  sogliak = soglia_roc [ k ]
  classification = as.numeric( sapply( previsione_mod_compl[,2], function( x ) ifelse( x < sogliak, 0, 1 ) ) )
  ordinata_roc[ k ] = sum( classification[ which( test$True == 1 ) ] == 1 ) /
    length( which( test$True == 1 ) )
  ascissa_roc[ k ] = sum( classification[ which( test$True== 0 ) ] == 1 ) /
    length( which( test$True == 0 ) )
}
plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )
# qual era il nostro punto?
abline( v = 1 - specificita_modcompl, h = sensitivita_modcompl, lty = 3, col = 'blue' )
points( 1 - specificita_modcompl, sensitivita_modcompl, pch = 4, lwd = 3, cex = 1.5, col = 'blue' )

auc.modcompl=auc(valori.reali,valori.predetti)
auc.modcompl


#curva ROC e AUC per modello 1
soglia_roc = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc = rep( NA, lens )
ordinata_roc = rep( NA, lens )
for ( k in 1 : lens )
{
  sogliak = soglia_roc [ k ]
  classification = as.numeric( sapply( previsione_mod_1[,2], function( x ) ifelse( x < sogliak, 0, 1 ) ) )
  ordinata_roc[ k ] = sum( classification[ which( test$True == 1 ) ] == 1 ) /
    length( which( test$True == 1 ) )
  ascissa_roc[ k ] = sum( classification[ which( test$True== 0 ) ] == 1 ) /
    length( which( test$True == 0 ) )
}
plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )
# qual era il nostro punto?
abline( v = 1 - specificita_mod1, h = sensitivita_mod1, lty = 3, col = 'blue' )
points( 1 - specificita_mod1, sensitivita_mod1, pch = 4, lwd = 3, cex = 1.5, col = 'blue' )

auc(valori.reali1,valori.predetti1)

#curva ROC e AUC per modello 2
soglia_roc = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc = rep( NA, lens )
ordinata_roc = rep( NA, lens )
for ( k in 1 : lens )
{
  sogliak = soglia_roc [ k ]
  classification = as.numeric( sapply( previsione_mod_2[,2], function( x ) ifelse( x < sogliak, 0, 1 ) ) )
  ordinata_roc[ k ] = sum( classification[ which( test$True == 1 ) ] == 1 ) /
    length( which( test$True == 1 ) )
  ascissa_roc[ k ] = sum( classification[ which( test$True== 0 ) ] == 1 ) /
    length( which( test$True == 0 ) )
}
plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )
# qual era il nostro punto?
abline( v = 1 - specificita_mod2, h = sensitivita_mod2, lty = 3, col = 'blue' )
points( 1 - specificita_mod2, sensitivita_mod2, pch = 4, lwd = 3, cex = 1.5, col = 'blue' )

auc.mod2=auc(valori.reali2,valori.predetti2)
auc.mod2



