#A study shows that 61 of 414 adults who grew up in a single-parent household report that
#they suffered at least one incident of abuse during childhood. By contrast, 74 of 501
#adults who grew up in two-parent households report abuse.
#Is there a difference in single-parent and two-parent households when it comes to
#reporting abuse? Answer this question by computing an appropriate 95% confidence
#interval.
#calculate CI for proportions
px = 61/414
py = 74/501

varx = (px*(1-px))/414
varx
vary = (py*(1-py))/501
vary
var = varx + vary
var
se = sqrt(var)
se

CI = (px-py)+c(-1,1)*(qnorm(0.975)*se)
CI

Px = 61/414			#probability of single-parent household
Py = 74/501			#probability of two-parent household
VARx = (Px*(1-Px))/414	#Variance of single-parent household
VARy = (Py*(1-Py))/501	#Variance of two-parent household
TotalVAR = VARx + VARy	#Total variance of single-parent and two-parent household
StdErr = sqrt (TotalVAR)	#Standard error of two sample variables
ConfidenceInterval = (Px - Py) + c(-1,1)*(qnorm(0.975)* StdErr) 
ConfidenceInterval
#Confidence interval 
