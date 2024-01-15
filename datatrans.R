datatrans <- read.table("datatrans-1.txt", header= 1)
head(datatrans)

x <- datatrans$x
y <- datatrans$y
plot(x, y)

fit = lm(y~x)
summary(fit)

b0 <- summary(fit)$coefficients[1,1]
b1 <- summary(fit)$coefficients[2,1]
#The estimated regression equation is: yhat=0.16876-0.00149*x

yhat=b0+b1*x
ei=y-yhat
table <- data.frame(x_values=x, y_values=y, predicted_values=yhat, residuals=ei)
head(table)

plot(x, ei)
