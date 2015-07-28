#Project 1
#R script for Project 1 Problems

##################################
#Part A. Univariate Data Analysis#
##################################

#1. Read in data from paper1.txt and print to verify
p1 <- read.table("datasets\\paper1.txt",header = T)
cat("The paper1 file contains:\n")
print(p1)

#2. Read in data from paper2.txt and print to verify
p2 <- read.table("datasets\\paper2.txt",header = T)
cat("The paper2 file contains:\n")
print(p2)

#3. Obtain sample mean, sample st dev, sample median, sample IQR, and percentiles 5,10,15,25,75,90,95 for each brand
cat("The sample mean for brand A is:\n")
print(A_xbar <- mean(p1$A))
cat("The sample st. dev. for brand A is:\n")
print(A_stdev <- sd(p1$A))
cat("The sample median for brand A is:\n")
print(A_med <- median(p1$A))
cat("The sample IQR for brand A is:\n")
print(A_intqr <- IQR(p1$A))
cat("The sample 5, 10, 15, 25, 75, 90, 95 percentiles for brand A is:\n")
print(A_prcntles <- quantile(p1$A, c(.05,.10,.15,.25,.75,.9,.95)))

cat("The sample mean for brand B is:\n")
print(B_xbar <- mean(p1$B))
cat("The sample st. dev. for brand B is:\n")
print(B_stdev <- sd(p1$B))
cat("The sample median for brand B is:\n")
print(B_med <- median(p1$B))
cat("The sample IQR for brand B is:\n")
print(B_intqr <- IQR(p1$B))
cat("The sample 5, 10, 15, 25, 75, 90, 95 percentiles for brand B is:\n")
print(B_prcntles <- quantile(p1$B, c(.05,.10,.15,.25,.75,.9,.95)))

#4. Find 95% confidence intervals for the true thickness for each type of paper separately.

#95% confidence interval for true thickness of Brand A and Brand B.
#Must use t-distribution because population st. dev. is unkown
cat("The upper and lower bound t-scores for a 95% confidence interval at (22 - 1) degrees of freedom:\n")
print(tscore_high <- qt(0.975,21))
print(tscore_low <- (-1)*tscore_high)
cat("We previously determined the sample mean for Brand A is 0.1103 and sample st. dev. is 0.0177.
Now we need to calculate the std error (SE).
SE = (st.dev) / sqrt(n)
SE = (0.0177) / sqrt(22)
SE = ")
cat(A_SE <- (A_stdev / sqrt(22)))
cat("\n\nNow we can plug our equation into the t-statistic (t*) and solve for mu:\n
t-low <= t* <= t-high
-2.08 <= (0.1103 - mu) / 0.00377 <= 2.08
-0.00784 <= (0.1103 - mu) <= 0.00784
-0.1181 <= (-mu) <= -0.1025
0.1181 >= mu >= 0.1025 or\n
The 95% confidence interval for the true thickness of the population of Brand A is [0.1025,0.1181]\n
Confirm my confidence interval calculations for Brand A using R:")
print(t.test(p1$A,y=NULL,alternative = "two.sided",paired = FALSE,conf.level = 0.95))

cat("\n\nWe previously determined the sample mean for Brand B is 0.0807 and sample st. dev. is 0.0045.
Now we need to calculate the std error (SE).
    SE = (st.dev) / sqrt(n)
    SE = (0.0045) / sqrt(22)
    SE = ")
cat(B_SE <- (B_stdev / sqrt(22)))
cat("\n\nNow we can plug our equation into the t-statistic (t*) and solve for mu:\n
    t-low <= t* <= t-high
    -2.08 <= (0.0807 - mu) / 0.00095 <= 2.08
    -0.001976 <= (0.0807 - mu) <= 0.001976
    -0.0827 <= (-mu) <= -0.0787
    0.0827 >= mu >= 0.0787 or\n
    The 95% confidence interval for the true thickness of the population of Brand A is [0.0787,0.0827]\n
    Confirm my confidence interval calculations for Brand A using R:")
print(t.test(p1$B,y=NULL,alternative = "two.sided",paired = FALSE,conf.level = 0.95))

#5a. Create a histogram using the default settings
hist(p2$thickness,main = "Thickness of Combined Paper Brands", xlab = "mm (using default bins)")
cat("Using default settins there are 7 bins at intervals of 0.02 mm")

#5b. Create a histogram with more bins than the default
hist(p2$thickness,main = "Thickness of Combined Paper Brands", xlab = "mm (using more bins than default)", breaks = seq(0.06,0.20,0.01))

#5c. Create a histogram with less bins than the default
hist(p2$thickness,main = "Thickness of Combined Paper Brands", xlab = "mm (using less bins than default)", breaks = seq(0.05,0.20,0.05))

#6. Create side-by-side boxplots of the thicknesses for Brand A and Brand B.
#Discuss what the boxplots tell you. Are there any outliers?
boxplot(p2$thickness ~ p2$brand, xlab = "Brand of Paper", ylab = "Thickness (mm)")

