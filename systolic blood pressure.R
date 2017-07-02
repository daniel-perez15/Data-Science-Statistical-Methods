# Consider the dataset stored in the file bp.txt. This dataset contains one measurement of
# systolic blood pressure (in mmHg) made by each of two methods—a finger method and
# an arm method—from the same 200 patients.
# (a) Perform an exploratory analysis of the data by examining the distributions of the
# measurements from the two methods using boxplots. Comment on what you see.
# Do the two distributions seem similar? Justify your answer.
# (b) Use histograms and QQ plots to examine the shapes of the two distributions.
# Comment on what you see. Does the assumption of normality seem reasonable?
# Justify your answer.
# (c) Construct an appropriate 95% confidence interval for the difference in the means
# of the two methods. Interpret your results. Can we conclude that the two methods
# have identical means? Justify your answer. What assumptions, if any, did you
# make to construct the interval? Do the assumptions seem to hold?
# (d) Perform an appropriate 5% level test to see if there is any difference in the means
# of the two methods. Be sure to clearly set up the null and alternative hypotheses.
# State your conclusion. What assumptions, if any, did you make to construct the
# interval? Do they seem to hold?
# (e) Do the results from (c) and (d) seem consistent? Justify your answer.

#read data into the file
data = read.csv(file.choose(),header = T,sep = ",")
#data into armsys
armsys = data[,1]
#data into fingsys
fingsys = data[,2]
#armsys
#fingsys
#boxplots for data
boxplot(armsys,main = "Armsys data")
boxplot(fingsys,main = "Fingsys data")
#histogram for data
hist(armsys,main = "Histogram of armsys")
hist(fingsys,main = "Histogram of fingsys")
#Q-Q plots
qqnorm(armsys,main = "Armsys data")
qqline(armsys)
qqnorm(fingsys,main = "Fingsys data")
qqline(fingsys)
armsys_mean = mean(armsys)
fingsys_mean = mean(fingsys)
armsys_mean
fingsys_mean
armsys_var = var(armsys)
armsys_var
fingsys_var = var(fingsys)
fingsys_var
#armsys_sd = sqrt(armsys_var)
#fingsys_sd = sqrt(fingsys_var)
se = sqrt((armsys_var/length(armsys))+(fingsys_var/length(fingsys)))
se
CI = (armsys_mean - fingsys_mean)+c(-1,1)*qnorm(.975)*se
CI
t.test(armsys,fingsys)
#	Welch Two Sample t-test
#data:  armsys and fingsys
#t = -1.7533, df = 394.35, p-value = 0.08032
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -9.1109747  0.5209747
#sample estimates:
#  mean of x mean of y 
#128.520   132.815 
#critical value
#alpha = 0.05
#cv = qnorm(1-(alpha/2))
#cv
#looking at the results of the t-test, we can conclude that there is not much of a difference between 
#the means. Hence, we can accept our Null hypothesis.