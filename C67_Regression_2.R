# First 6 observations
datatrans <- read.table("datatrans-1.txt", header= 1)
head(datatrans)

# Simple linear regression model of y on x
x <- datatrans$x
y <- datatrans$y
plot(x, y)

fit = lm(y~x)
b0 <- summary(fit)$coefficients[1,1]
b1 <- summary(fit)$coefficients[2,1]
# The estimated regression equation is: yhat=0.16876-0.00149*x

# Table with x-values, y-values, predicted values (yhat) and residuals
yhat=b0+b1*x
ei=y-yhat
table <- data.frame(x_values=x, y_values=y, predicted_values=yhat, residuals=ei)
head(table)

# Plot residuals against x values
plot(x, ei)

# Normal quantile plot of residuals
qqnorm(ei)

# Shapiro-Wilks test to test for Normality of residuals (alpha=0.05)
shapiro.test(ei)
# p-value<<0.05, therefore the p-value is too small. We reject the null hypothesis of normality for alpha=0.05

# Brown-Forsythe test to test for constancy of error variance (alpha=0.05)
table1 <- cbind(x, ei)
orderedbyx <- table1[order(x),]
orderedbyx <- data.frame(orderedbyx)
eorderedbyx <- orderedbyx$ei
n <- nrow(table1)
n1 <- ceiling(n/2)
n2 <- n - n1
e1 <- eorderedbyx[1:n1]
e2 <- eorderedbyx[c((n1+1):n)]
med1 <- median(e1)
med2 <- median(e2)
d1 <- abs(e1-med1)
dbar1 <- mean(d1)
d2 <- abs(e2-med2)
dbar2 <- mean(d2)
s_sq <- (sum((d1-dbar1)^2)+sum((d2-dbar2)^2))/(n-2)
s <- sqrt(s_sq)
tBF <- (dbar1-dbar2)/(s*sqrt(1/n1+1/n2))
p_value <- 2*(1-pt(abs(tBF), n-2))
# The median of e1 is -0.016515178836402, median of e2 is 0.00262721675892492, dbar1 is 0.0213989982563871,
# dbar2 is 0.0111378363320415, t-test statisitc is 2.3272564854971, and the p-value is 0.0221701796535707. The p-va lue<<0.05, 
# therefore the p-value is too small. We reject the null hypothesis for alpha=0.05 and the variance is not constant

# Use Box-Cox transformation on y and find the MLE of lambda
library(MASS)
bc <- boxcox(y~x)
lambda <- bc$x[which.max(bc$y)]

# Use estimated value of lambda to transform the y-values
K2 <- prod(y)^(1/length(y))
yt <- (y^lambda-1)/(lambda*(K2^(lambda-1)))
head(yt)

# Fit the regression model for transformed y on x and obtain residuals
# Compare the plot of "residuals against x-values" against "the residuals from the transformed model against x-values" 
fitt = lm(yt~x)
et <- fitt$residuals
par(mfrow=c(1,2))
plot(ei, main="Original")
plot(et, main="Transformed")

# Compare the plot of the normal quantile plots for the residuals from "the original model" again "the transformed model"
par(mfrow=c(1,2))
qqnorm(ei, main="Original")
qqnorm(et, main="Transformed")

# Use Shapiro-Wilks test to test for Normality of residuals from the Box-Cox transformed model (alpha=0.05)
shapiro.test(et)
# p-value>>0.05, therefore the p-value is too large. We failed to reject the null hypothesis of normality for alpha=0.05.

# Use Brown-Forsythe test for constancy of error variance for the residuals from the Box-Cox transformed model (alpha=0.05)
tablet <- cbind(x, et)
orderedbyx <- tablet[order(x),]
orderedbyx <- data.frame(orderedbyx)
etorderedbyx <- orderedbyx$et
n <- nrow(tablet)
n1 <- ceiling(n/2)
n2 <- n - n1
e1 <- etorderedbyx[1:n1]
e2 <- etorderedbyx[c((n1+1):n)]
med1 <- median(e1)
med2 <- median(e2)
d1 <- abs(e1-med1)
dbar1 <- mean(d1)
d2 <- abs(e2-med2)
dbar2 <- mean(d2)
s_sq <- (sum((d1-dbar1)^2)+sum((d2-dbar2)^2))/(n-2)
s <- sqrt(s_sq)
tBF <- (dbar1-dbar2)/(s*sqrt(1/n1+1/n2))
p_value <- 2*(1-pt(abs(tBF), n-2))
# The median of e1 is 0.000117049349565166, median of e2 is -0.000312080173253057, dbar1 is 0.00444018467427865, 
# dbar2 is 0.00415972526785299, t-test statisitc is 0.362124545337581, and the p-value is 0.71809854744486. 
# The p-value>>0.05, therefore the p-value is too large. We failed to reject the null hypothesis for alpha=0.05 and the variance is constant
