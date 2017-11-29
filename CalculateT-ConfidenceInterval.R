tval <- qt(0.975, df = 21) #compute t-score
xbar <- 52.1 #sample mean
s <- 45.1 #sample standard deviation
n <- 22 #degrees of freedom
se <- s / sqrt(n) #compute standard error
CI <- c ( xbar - ( tval * se ), xbar + ( tval * se ) ) #compute Confidence Interval
CI #print Confidence Interval