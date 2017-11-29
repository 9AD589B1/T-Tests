#Estimate the average after-lunch snack consumption (in grams)
#of people who eat lunch distracted using a 95% CI

rm ( list = ls ( all = TRUE ) )

tval <- qt(0.975, df = 21) #compute t-score
xbar <- 52.1 #sample mean
s <- 45.1 #sample standard deviation
n <- 22 #degrees of freedom
se <- s / sqrt(n) #compute standard error
CI <- c ( xbar - ( tval * se ), xbar + ( tval * se ) ) #compute Confidence Interval
CI #print Confidence Interval

#Suppose the suggested serving of these biscuits is 30 grams. 
#Do these data provide convincing evidence that the amount of 
#snacks consumed by distracted eaters post lunch is different 
#than the suggested serving size?

tscore <- ( xbar - 30 ) / se #calculate the t-score of the alternative hypothesis
pval <- pt ( tscore, df = 21, lower.tail = FALSE ) * 2 #calculate the p-value of the t-score using BOTH tails (multiply by 2)
pval #print p-value
