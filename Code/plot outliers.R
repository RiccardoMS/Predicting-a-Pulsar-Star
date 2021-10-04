library(readr)
attach(pulsar_stars)
pulsar_stars <- read_csv("C:/Users/daniele/Desktop/inferenza statistica/pulsar_stars.csv")
library(caret)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
  
  set.seed(1)
x <- var8
y <- remove_outliers(x)
## png()
par(mfrow = c(1, 2))
z=rnom 
qqplot
boxplot(var8~var9, main="boxplot per classi", ylab="skewness of DM-SNR curve", xlab="pulsar no/yes", col=c("red", "green"))
boxplot(y~var9, main="without outliers", ylab="skewness of DM-SNR curve", xlab="pulsar no/yes", col=c("red", "green"))
z=rnorm(n=lenght(y), mean=0, sd=1)
qqplot(z, y , main="qqplot mean of I.P.",ylab="mean of I.P.", xlab = "")
qqline(data$Var1, lty=2,lwd=2, col='red')