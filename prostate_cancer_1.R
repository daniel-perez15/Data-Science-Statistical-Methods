# 1. Consider the prostate cancer dataset  Choose one more quantitative
# or qualitative variable to add to the final model built in that project and justify
# your choice. Fit a linear model using the two variables and carry out regression diagnostics.
# The analysis should include an assessment of the degree to which the key regression
# assumptions are satisfied. If an assumption is not met, attempt to remedy the
# situation. Comment on the fit of the final model using appropriate tests and statistics.
# 2. Use the final model to predict the PSA level for a patient whose predictor variables
# are at the sample medians of the variables.
#read the csv file
data = read.csv(file.choose(),header = T,sep = ",")

#convert to log scale
psa = log(data$psa)  
canc=log(data$cancervol)

#Using build model of PSA and canc
linear.reg<- lm(psa ~ canc)

#factor variable for vesinv
vesinv.factor = factor(data$vesinv)

#factor variable for gleason
gleason.factor = factor(data$gleason)

#compare the first model with vesinv
anova(linear.reg,lm(psa ~ canc + vesinv.factor))

#compare the first model with gleason
anova(linear.reg,lm(psa ~ canc + gleason.factor))

#compare the first model with capspen
anova(linear.reg,lm(psa ~ canc + data$capspen))

#compare the first model with benpros
anova(linear.reg,lm(psa ~ canc + data$benpros))

#compare the first model with weight
anova(linear.reg,lm(psa ~ canc + data$weight))

#since p value of vesinv is low we add to the model
finalmodel.reg = lm(psa ~ canc + vesinv.factor)

#summary of the model
summary(finalmodel.reg)


#time series plot
qqnorm(resid(finalmodel.reg), main="QQPLOT")
qqline(resid(finalmodel.reg))

#time series plot
plot(resid(finalmodel.reg), type="l", main ="TIME SERIES PLOT")
abline(h=0)

#residual plot
plot(fitted(finalmodel.reg),resid(finalmodel.reg),main="RESIDUAL PLOT")
abline(h=0)

#median of canc and vesinv = 0
dataframe <- data.frame(canc= median(canc) ,vesinv.factor= as.factor(0))
predictval= exp(1)^predict(finalmodel.reg,dataframe)
predictval

mean(resid(finalmodel.reg))


