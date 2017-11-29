#Researchers are interested in the effect of a drug to lower blood
#pressure. There are 100 patients per group, and the standard deviation
#of the patient's blood pressure is 12 mmHg. This will be a test of 
#comparing two independent means

rm ( list = ls ( all = TRUE ) )

n <- 100 #sample size for each group
s <- 12 #standard deviation of patient's blood pressures - same for both groups
se <- sqrt ( (s^2/n) + (s^2/n) ) #calculate standard error for the two independent means

#For what values of the difference between the observed averages of
#blood pressure in treatment and control groups (effect size) would we
#reject the null hypothesis at a 5% significance level?
#Note our null hypothesis is that there is no difference (Ho = 0)
#We will have a two-tailed rejection region, using a significance level of 0.05
#So, to calculate the critical value, we multiply the z-score by the standard error

zquant <- qnorm(0.975) * -1 #z-quantile for the two-tailed rejection region
zvalue <- zquant * se #the critical value is the z-quantile * the standard error
zvalue #print the critical value

#Suppose that the company researchers care about finding any effect on blood pressure 
#that is 3 mm of mercury or larger versus the standard medication. 
#What is the power of the test that can detect this effect?

#We have established a critical value of approximately 3.33. We want to know the
#area under the normal curve of our critical value relative to the 3mm effect. We

power_zscore <- ( zvalue - (-3) ) / se #calculate the z-score of the critical value
power_area <- pnorm(power_zscore) #calculate the area under the normal curve
power_area #print the power area

#We want to know the required sample size to achieve 80% power. We can work
#backwards to determine this. 

desired_zquant <- qnorm(0.8) #Z-score of the 80th percentile
#Note the effect size of 3mm is 2.8 * SE, that is (0.84 * SE) + (1.96* SE).
#In other words, the distance between 0 (the null value) and 3 is SE * 2.8

desired_se <- 3 / (desired_zquant + (zquant * -1)) #calculate the SE

#To get the required sample size, solve for n
#desired_se = sqrt ( s^2/n + s^2/n )
#desired_se^2 = 2s^2/n
#n = 2s^2/desired_se^2

desired_n = ( 2 * s^2 ) / desired_se^2 #solve for n
desired_n #print the desired sample size

#We will need a sample size of at least 252