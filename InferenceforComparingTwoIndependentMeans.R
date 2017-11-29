#Estimate the difference between the average post-meal snack 
#consumption between those who eat with and without distractions. 
#The confidence interval will be of the form point estimate, 
#that is the difference between the two sample means, plus or minus a margin of error. 
#That is a critical T-score times the standard error of the difference between the two sample means.

rm ( list = ls ( all = TRUE ) )

xbar_distract <- 52.1 #sample mean of treatment group
xbar_nodistract <- 27.1 #sample mean of control group
xbar_diff <- xbar_distract - xbar_nodistract
s_distract <- 45.1 #sample standard deviation of treatment group
s_nodistract <- 26.4 #sample standard deviation of control group
n <- 22 #degrees of freedom (sample size is the same for both groups)
tval <- qt(0.975, df = 21) #compute t-score
se <- sqrt ( ( s_distract^2 / n ) + ( s_nodistract^2 / n ) ) #compute standard error
CI <- c ( xbar_diff - ( tval * se ),  xbar_diff + ( tval * se ) ) #compute Confidence Interval
CI #print Confidence Interval

#Is there a statisically significant difference between the two sample means?
#Our null hypothesis is zero because we assume there is NO difference between the two means
#Our alternative hypothesis is the difference between the sample means

tscore <- ( xbar_diff - 0 ) / se #calculate the t-score of the alternative hypothesis
pval <- pt ( tscore, df = 21, lower.tail = FALSE ) * 2 #calculate the p-value of the t-score using BOTH tails (multiply by 2)
pval #print p-value
