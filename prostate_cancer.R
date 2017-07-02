# Consider the prostate cancer dataset available on eLearning. It consists of data on 97 men
# with advanced prostate cancer. Following is a description of the variables:
# 1. Take PSA level is as the response variable. Make scatterplots of PSA level with other
# variables. Based on these, choose one quantitative variable that you think may be
# used effectively to predict PSA level. Highlight any potential outliers on the scatterplot
# of this variable with PSA level.
# 2. Fit a simple linear regression model and carry out regression diagnostics. The analysis
# should include an assessment of the degree to which the key regression assumptions
# are satisfied. If an assumption is not met, attempt to remedy the situation.
# Comment on the fit of the final model using appropriate tests and statistics.
# 3. Use the final model to predict the PSA level for a patient whose predictor variable
# value is at the median of the variable.

#reading csv
data = read.csv(file.choose(),header = T,sep = ",")
#data[,2]
#plotting PSA against all the given varaiables - delete variables that are not necessary
plot(data[,3],data[,2],xlab = "CANCER VOLUME", ylab = "PSA", main = "CVOL vs PSA")
abline(lm(data[,2]~data[,3]))
cor(data[,3],data[,2])
plot(data[,4],data[,2],xlab = "WEIGHT", ylab = "PSA", main = "WEIGHT vs PSA")
cor(data[,4],data[,2])
abline(lm(data[,2]~data[,4]))
plot(data[,5],data[,2],xlab = "AGE", ylab = "PSA", main = "AGE vs PSA")
cor(data[,5],data[,2])
abline(lm(data[,2]~data[,5]))
plot(data[,6],data[,2],xlab = "BENPROS", ylab = "PSA", main = "BENPROS vs PSA")
cor(data[,6],data[,2])
abline(lm(data[,2]~data[,6]))
plot(data[,7],data[,2],xlab = "VESINV", ylab = "PSA", main = "VESINV vs PSA")
cor(data[,7],data[,2])
abline(lm(data[,2]~data[,7]))
plot(data[,8],data[,2],xlab = "CAPSPEN", ylab = "PSA", main = "CAPSPEN vs PSA")
cor(data[,8],data[,2])
abline(lm(data[,2]~data[,8]))
plot(data[,9],data[,2],xlab = "GLEASON", ylab = "PSA", main = "GLEASON vs PSA")
cor(data[,9],data[,2])
abline(lm(data[,2]~data[,9]))
#considering cvol to be the quantitative variable
boxplot(data[,2],data[,3],main = "Boxplot of PSA and CANCER VOLUME")
x = data[,3]
y = data[,2]

canc.reg <- lm(y~x)
summary(canc.reg)
x.new <- data.frame(x=median(y)) 
predict(canc.reg,x.new)
#fitted(canc.reg)
#resid(canc.reg)
plot(data[,3],data[,2],xlab = "CANCER VOLUME", ylab = "PSA", main = "CVOL vs PSA",abline(canc.reg))

#key regression assumptions

plot(fitted(canc.reg), resid(canc.reg),main = 'Fitted vs Resid')
abline(h=0)

# QQ plot

qqnorm(resid(canc.reg))
qqline(resid(canc.reg))


# Time series plot of residuals

plot(resid(canc.reg), type="l",main = 'Time plot of Residuals')
abline(h=0)

#modelling the linear model for log(PSA) and lof(cancervol)
logpsa = log(data[,2])
logcancervol = log(data[,3])
newcanc.reg = lm(logpsa~logcancervol)
summary(newcanc.reg)
hist(resid(newcanc.reg), main="HISTOGRAM OF ERRORS")
qqnorm(resid(newcanc.reg), main="QQPLOT")
qqline(resid(newcanc.reg))
plot(resid(newcanc.reg), type="l", main ="TIME SERIES PLOT")
abline(h=0)
plot(fitted(newcanc.reg),resid(newcanc.reg),main="RESIDUAL PLOT")
abline(h=0)
new_predict <- exp(1)^predict(newcanc.reg,data.frame(logcancervol = median(logpsa)))
new_predict


