#Project 1
#R script for Project 1 Problems

##################################
#Part A. Univariate Data Analysis#
##################################

#1. Read in data from paper1.txt and print to verify
p1 <- read.table("paper1.txt",header = T)
cat("The paper1 file contains:\n")
print(p1)

#2. Read in data from paper2.txt and print to verify
p2 <- read.table("paper2.txt",header = T)
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
cat("The boxplot is one way to see how your data is distributed. Specifically it shows you
    the median (second quartile/50th percentile), the first quartile (25th percentile),
    third quartile (75th percentile), as well as inner fences that designate where outliers
    begin. In the dataset there are 2 outliers for Brand A and 1 outlier for Brand B.\n\n")

###########################
#Part B. One-sample t-test#
###########################

#1. Create a dataset containing the number of concurrent users at each location.
# nusers = c(scan( ))
# 17.2  22.1  18.5  17.2  18.6  14.8  21.7  15.8  16.3  22.8
# 24.1  13.3  16.2  17.5  19.0  23.9  14.8  22.2  21.7  20.7
# 13.5  15.8  13.1  16.1  21.9  23.9  19.3  12.0  19.9  19.4
# 15.4  16.7  19.5  16.2  16.9  17.1  20.2  13.4  19.8  17.7
# 19.7  18.7  17.6  15.9  15.2  17.1  15.0  18.8  21.6  11.9
cat("Read in the data using scan(). Now nusers contains:\n")
print(nusers)

#2. Create and interpret the normal plot for nusers
qqnorm(nusers)
cat("The normal plot for my nusers dataset appears to be linear with a slope ~1
    implying that the data is normally distributed.")

#3. Compute a 95% confidence interval for nusers.
cat("The upper and lower bound t-scores for a 95% confidence interval at (50 - 1) degrees of freedom:\n")
print(tscore2_high <- qt(0.975,50))
print(tscore2_low <- (-1)*tscore2_high)
cat("The sample mean for nusers is:\n:")
print(nusers_xbar <- mean(nusers))
cat("The sample st. dev. for nusers is:\n")
print(nusers_stdev <- sd(nusers))
cat("Now we need to calculate the std error (SE).
    SE = (st.dev) / sqrt(n)
    SE = (3.1573) / sqrt(50)
    SE = ")
cat(nusers_SE <- (nusers_stdev / sqrt(50)))
cat("\n\nNow we can plug our equation into the t-statistic (t*) and solve for mu:\n
    t-low <= t* <= t-high
    -2.0796 <= (17.954 - mu) / 0.4465 <= 2.0796
    -0.9285 <= (17.954 - mu) <= 0.9285
    -18.8825 <= (-mu) <= -17.0255
    18.8825 >= mu >= 17.0255 or\n
    The 95% confidence interval for the network load at locations is [17.0255,18.8825]\n
    Confirm my confidence interval calculations for network load using R:")
print(t.test(nusers,y=NULL,alternative = "two.sided",paired = FALSE,conf.level = 0.95))

#4. Show the five steps of the one-sample t-test at the a = 0.05 level to test whether nusers has
#changed in the past month.Usage data from last month shows an average of 17.2 thousand
#concurrent users.
cat("The null hypothesis is nusers average is 17.2. The alternative hypothesis is that the true value of nusers is NOT 17.2.")
cat("The t-static for this test is:")
print(nusers_tstat <- (nusers_xbar - 17.2)/nusers_SE)
cat("Using the t-table in my textbook to look up a 95% confidence at 49 dfs (est. using 40 df) returns [-2.021,2.021]")
cat("We fail to reject the null hypothesis b/c our t-statistic (1.69) falls within our 95% confidence interval [-2.021,2.021].
    This means that we can't be sure that the true value of nusers is NOT 17.2; AKA our sample nusers average of 17.954 may have been
    a coincidence.")
cat("The p-value for this test is computed via R:")
print(t.test(nusers,y=NULL,alternative = "two.sided",mu = 17.2,paired = FALSE,conf.level = 0.95))
cat("The p-value is ~9.7%, which is greater than our alpha level of 5%, meaning that there is a greater probability that random
    fluctuations generated my sample results, so I am not confident in rejecting the null hypothesis if I want to have a 5% Type I error risk")

###########################
#Part C. Two-sample t-test#
###########################

#1. Add print statements in your source code to explain what your output means. 
p1clean <- read.table("paper1-cleaned.txt",header = T)
cat("The paper1-cleaned file shows paper thickness grouped by measurer and brand type\n")
print(p1clean)

p2clean <- read.table("paper2-cleaned.txt",header = T)
cat("The paper2-cleaned file shows paper thickness not grouped\n")
print(p2clean)

#2. Create normal plots of the thicknesses separately for the paper brands A and B. Interpret these normal plots. 
cat("This is the normal plot for the thickness of brand A.The thickness of brand A appears to be normally distributed as the
    normal plot appears to have a linear relationship with a slope of ~1.")
qqnorm(p1clean$A)
cat("This is the normal plot for the thickness of brand B.The distribution of the thickness of Brand D does not appear to
    have a normal distribution b/c the points for the normal plot are not very linear. It appears the distribution may have thin tails.
    However, a histogram shows the distribution may be bimodal.")
qqnorm(p1clean$B)

#3. Type out the five steps of a 0.05-level paired-sample t-test to test the null hypothesis that there is no difference
#between the paper thicknesses in paper1-cleaned.txt.  Show relevent SAS or R output in your report.  You will need to obtain
#the confidence interval for the test statistic from the t-table.
cat("\n\nStep 1: The null hypothesis is that the average of the paired differences between the thickness of Brand A and Brand B is ZERO.
    The alternative hypothesis is that the average of the paired differences is NOT equal to ZERO.\n\n")

cat("Step 2: The test statistics is defined z = (dbar - mu) / SE, where dbar is the average paired difference, mu is the null hypothesis value of 0,
    and SE is the st. dev. of the paired differences divided by the sqrt of the sample size.
    dbar = ")
print(d <- mean(p1clean$A - p1clean$B))
cat("SE = ")
print(SEd <- sd(p1clean$A - p1clean$B)/sqrt(19))
cat("The tstat = ")
print(zd <- (d/SEd))
cat("\nStep 3: At the 5% alpha level, using the t-table in my book for (19 - 1) df, the confidence interval is [-2.101,2.101]\n")
cat("Step 4: Because our tstat of 25.54 is not within the range of our confidence interval [-2.101,2.101] we reject the null hypothesis
    and conclude that there is a difference between the thickness of Brand A and Brand B.\n")
cat("Step 5: The p-value for this test is computed via R:")
print(t.test(p1clean$A,p1clean$B,alternative = "two.sided",paired = TRUE, conf.level = 0.95))
cat("The p-value is extremely small (1.357e-15) meaning that there is virtually a zero probability that we acquired the observations
    in our sample by random chance (i.e., random fluctuations generated my sample results) so I am confident in rejecting my
    null hypothesis that the average paired difference is zero (i.e., I'm confident that there is a significant difference between the 
    thickness of Brands A and B).")

#4. Type out the five steps a 0.05-level independent two-sample t-test to test the null hypothesis that there is no difference
#between the paper thicknesses for brands A and B. Show your output and discuss what it means. You will need to obtain
#the confidence interval for the test statistic from the t-table.
cat("\n\nStep 1: The null hypothesis is that the sample average of both treatment groups are EQUAL.
    The alternative hypothesis is that the sample average of both treatment groups are NOT equal to ZERO.\n\n")

cat("Step 2: I am calculating the test statistic by hand:\n\n\n\n\n\n\n\n\n\n")
cat("\nStep 3: At the 5% alpha level, using the t-table in my book for (19 + 19 - 2) df (approx. using 40 df), the confidence interval is [-2.021,2.021]\n")
cat("Step 4: Because our tstat of 23.5 (assuming equal variances) is not within the range of our confidence interval [-2.021,2.021] we reject the null hypothesis
    and conclude that there is a difference between the thickness of Brand A and Brand B.\n")
cat("Step 5: The p-value for this test is computed via R:")
print(t.test(p1clean$A,p1clean$B,alternative = "two.sided",paired = FALSE, var.equal = TRUE, conf.level = 0.95))
cat("The p-value is extremely small (2.2e-16) meaning that there is virtually a zero probability that we acquired the observations
    in our sample by random chance (i.e., random fluctuations generated my sample results) so I am confident in rejecting my
    null hypothesis that the sample average of both treatment groups are ZERO (i.e., I'm confident that there is a significant difference between the 
    thickness of Brands A and B).")

#5. Is the paired sample or the independent two-sample t-test is more appropriate to decide if the true thickness
#of a sheet of paper is different for Brand A or Brand B? Explain your answer. How do the p-values compare for the two t-tests?
#Is this what you would expect?
cat("The paired sample t-test is more appropriate because they were measured by the same person so each treatment group has something
    in common and can be paired with a measurement for each paper thickness. It is difficult to distinguis the p-values because they are
    so small. However, the independent two-sample t-test produces a smaller p-value. This is not as intuitive because I thought a paired
    sample would produce a smaller p-value b/c, in the paper thickness example, you are accounting for experiment error by having the 
    same person taking each measurement.What if everyone has trouble working the micro measurer the first time and assuming everyone
    measures Paper A first, everyone understates Paper A thickness by a few mm. An independent test would bake that experiment error into
    its sample average and Paper A thickness would be artificially less than Paper B.")
