#what is the area within [-1, 1] standard deviations? you are feeding it z-values

print(pnorm(1)) #not the right answer
print(pnorm(1) - pnorm(-1)) #this is how you do it. need to remove the left half

#what is the area within [-2, 2] standard deviations?

print(pnorm(2))
print(pnorm(2) - pnorm(-2))

#what is the area within [-3, 3] standard deviations?

print(pnorm(3))
print(pnorm(3) - pnorm(-3))

#qnorm() function will give me the z-value I need I just need to pick the percentage
#Find a 99% confidence interval for z if z ∼ N(0,1)
print(qnorm(0.99))

##############################
#how to create a boxplot in R
##############################
x <- c(5,39,75,79,85,90,91,93,93,98)
boxplot(x)

##############################
#how to create a histogram in R
##############################
hist(x)
hist(x, breaks=c(0,25,50,75,100))

##############################
#how to create anormal plot in R
##############################
nscores <- qnorm(seq(0.2,0.8,0.2))
print(nscores)
#the nscores vector gives me the z-values neeeded for the % I specified

##############################
#how to calc pvalue in R
##############################

#-4.03 is the lower bound of the z-value 
print(2 * pnorm(-4.03))
  