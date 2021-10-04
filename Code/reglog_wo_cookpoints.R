soglia = 0.5
valori.reali = test$True
previsione_modmultcomplcd=predict(modmultcomplcd,newdata=test,type="response")
valori.predetti = as.numeric( previsione_modmultcomplcd > soglia )
tabcompl = table( valori.reali, valori.predetti )
tabcompl

# % di casi classificati correttamente:
perc_class=round( sum( diag( tabcompl ) ) / sum( tabcompl ), 2 )
perc_class
# % di casi misclassificati:
perc_misclass=round( ( tabcompl [ 1, 2 ] + tabcompl [ 2, 1 ] ) / sum( tabcompl ), 2 )
perc_misclass

sensitivita = tabcompl [ 2, 2 ] /( tabcompl [ 2, 1 ] + tabcompl [ 2, 2 ] )
sensitivita

specificita = tabcompl [ 1, 1 ] /( tabcompl [ 1, 2 ] + tabcompl [ 1, 1 ] )
specificita

soglia_roc = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc = rep( NA, lens )
ordinata_roc = rep( NA, lens )
for ( k in 1 : lens )
{
  sogliak = soglia_roc [ k ]
  classification = as.numeric( sapply( previsione_modmultcomplcd, function( x ) ifelse( x < sogliak, 0, 1 ) ) )
  ordinata_roc[ k ] = sum( classification[ which( test$True == 1 ) ] == 1 ) /
    length( which( test$True == 1 ) )
  ascissa_roc[ k ] = sum( classification[ which( test$True == 0 ) ] == 1 ) /
    length( which( test$True == 0 ) )
}
plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )
# qual era il nostro punto?
abline( v = 1 - specificita, h = sensitivita, lty = 3, col = 'blue' )
points( 1 - specificita, sensitivita, pch = 4, lwd = 3, cex = 1.5, col = 'blue' )

AUC.modmultcomplcd=auc(valori.reali,valori.predetti)
AUC.modmultcomplcd