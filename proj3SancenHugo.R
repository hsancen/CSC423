#Project 3
#Hugo Sance (hugosancen@gmail.com)

########
#Part A#
########
cat("Load in my dataset:")
print(ChemReaction <- read.table(file = "ChemReaction.txt", header = T))

#1. Find the regression equation for predicting rate from monomer and dimer
print(LinChemReact <- lm(ChemReaction$rate ~ ChemReaction$monomer + ChemReaction$dimer,data = ChemReaction))
cat("The regression equation for predicting the reaction rate from monomer and dimer is:
    rate^ = 1.491*monomer + 8.954*dimer - 1.723\n\n")

#2.Obtain the estimated parameters
cat("Here is a summary of my model and the estimated parameters:")
print(summary(LinChemReact))

#3. Find a 95% confidence interval for each estimated parameter compute them by hand
cat("\n\n\n\n\n\n\n\n")

cat("Confirming my answers for the 95% confidence intervals of my estimate parameters:")
print(confint(LinChemReact))

#4. Obtain the predicted values
cat("I can obtain the predicted values for my model by using the fitted function:")
print(LinChemReact.fit <- fitted(LinChemReact))

#5. Obtain the residuals
cat("I can obtain the residuals for my model by using the resid function:")
print(LinChemReact.res <- resid(LinChemReact))

#6. If you try a new experiment with the amount 1.00 of monomer and 0.8 of dimer, what is the predicted reaction rate
cat("Confirming what is the predicted reaction rate for a new experiment
    with the amount 1.00 of monomer and 0.8 of dimer:\n\n")
cat("The result I get is:" , (-1.723 + (1*1.491)+(0.8*8.954)))

#7
cat("To calculate the predicted values, I first need to gather my independent variables:")
print(indep <- data.frame(ChemReaction$monomer, ChemReaction$dimer))
cat("I then feed my independent variables into my model using the predict function to get the prediction interval.
    95% is the default:\n")
print(predict(object = LinChemReact,newdata = indep,interval = "predict"))
cat("I then feed my independent variables into my model using the predict function with 'confidence' paramente
    to get the confidence interval. 95% is the default:\n")
print(predict(object = LinChemReact,newdata = indep,interval = "confidence"))

########
#Part B#
########

#1.Create and print a SAS dataset or R dataframe named Banking
cat("This is my Banking data frame:\n")
print(Banking <- read.table("banking.txt", header = T))

#2. Create scatterplots to visualize the associations between bank balance and the other five variables.
#Do the associations appear to be linear?
cat("From the scatterplot below it looks like bank balance appears to have a strong linear association with
    Wealth and Income. Home Value also appears to have a positive linear association but less so than Wealth and Income.")
pairs(Banking)

#3. Compute correlation values of bank balance vs the other variables.
#Interpret the correlation values. Which variables appear to be strongly associated.
cat("From the correlation matrix below, it appears that bank balance has the strongest correlation with Wealth and Income
    at r = 0.948 and r = 0.952, respectively. Home Value has the third strongest correlation at r = 0.766. Correlation measures
    how closely two variables move with each other. The closer the absolute value is to 1, the more one variable will move in
    lockstep with another variable.")
print(cor(Banking))

#4 Fit a regression model of balance vs the other five variables.
#Write the expression of the estimated regression model.
cat("The regression model of balance vs the other five variables is as follows:")
# started using Google's R Style Guide after this line
print(lm.balance <- lm(Banking$Balance ~ Banking$Age + Banking$Education
                       + Banking$Income + Banking$HomeVal + Banking$Wealth,
                       data = Banking))
cat("The expressiong for the estimated regression model is:\n")
cat("Balance^ = Age^*317.5 + Education^*590.3 + Income^*0.147 + HomeVal^*0.0099 + Wealth^*0.074 - 10330\n\n\n\n")

# 5. Are there any influence points for this model?
cat("Using R, there are 14 observations that are considered to be influence
    points. They are marked below with an asterisk.\n")
print(influence.measures(lm.balance))

# 6. Which of the five predictors have a significant effect on balance? (a=.05) 
cat("At the 5% level, my confidence interval for d.f. (n-k-1) of 96 is: [",
    qt(0.025,96),", ", qt(0.975,96),"]\n")
cat("Thus, at the 5% level, based on the t-values in the summary below, Age,
    Income, and Wealth have a significant effect on balance. This is confirmed
    because R puts a certain number of asterisks next to the predictors that are
    significant at certain levels.")
print(summary(lm.balance))

# 7. 
cat("I have removed HomeVal from this new regression model because it is not a
    significant ind. var. at p=0.37.\nThe new model is as follows:\n")
print(lm.balance2 <- lm(Banking$Balance ~ Banking$Age + Banking$Education
                        + Banking$Income + Banking$Wealth, data = Banking))
cat("The expression for the new model is: Balance^ = Age^*324.2 + Education^*749.8 + Income^*0.1615 + Wealth^*0.0726 - 12140")

# 8.
cat("Using the R Summary function for my model, the output shows me that all
    my regressors have a significant association with balance at the 5% level and
    all my regressors in this model even have significance at the 1% level\n")
print(summary(lm.balance2))

# 9. 
cat("For the final model, each unit increase in Age equates to a 324.2 increase
    in Balance. Each unit increase in Education equates to a 749.8 increase in
    Balance. Each unit increase in Income equates to a 0.1615 increase in Balance.
    And finally, each unit increase in Wealth equates to a 0.0726 increase in Balance.\n\n\n\n")

# 10. 
cat("Again, using my R Summary above for my new model, the R^2 value 0.946 suggests
    my model does a very good job of explaining the changes in Balance. Because this is
    a multiple regression model, it is best to use adjusted-R^2 value of 0.944, which is
    an equally good measure of the model's ability to explain the changes in balance.")

# 11. 
cat("The F-test steps:\n")
cat("Step 1: The null hypothesis is that all of my coefficients are equal to zero.
    The alternative hypothesis is that at least one of my coefficients is not equal to zero.\n")
cat("Step 2: The F-statistic for my model (from the summary output above) is 427.4")
cat("Step 3: The 95% confidence internval for (4,97) is [0,2.46] using the qf() formula in R.")
cat("Step 4: At the 5% level, we reject the null hypothesis because our F-stat of 427.4 
    is not in our 95% confidence interval of [0,2.46]")
cat("Step 5: From my summary output above, the p-value for the F-test is 0.00000000000000022")

# 12.
cat("The residual plots for my second model with 4 independent variables look
    pretty well-behaved. I am comfortable saying the residuals are sufficiently
    unbiased as well as sufficiently homoscedastic. Although I will admit there
    is a bit of cone shape formation but it is minimal. The normal plot is also
    very linear suggesting my residuals are normally distributed.")
res.lm.balance2 <- resid(lm.balance2)
pred.lm.balance2 <- fitted(lm.balance2)
plot(pred.lm.balance2, res.lm.balance2, ylim = c(-7000,7000), main = "Res Plot for r.*p")
plot(Banking$Age, res.lm.balance2, ylim = c(-7000,7000), main = "Res Plot for r.*Age")
plot(Banking$Education, res.lm.balance2, ylim = c(-7000,7000), main = "Res Plot for r.*Education")
plot(Banking$Income, res.lm.balance2, ylim = c(-7000,7000), main = "Res Plot for r.*Income")
plot(Banking$Wealth, res.lm.balance2, ylim = c(-7000,7000), main = "Res Plot for r.*Wealth")
qqnorm(res.lm.balance2, main = "Normal Plot for Residuals")

# 13.
cat("From my influence measures table below, R has designated 13 observations as influence points.
    It is worth nothing that this is a step above my previous model which had 14 influence points.")
print(influence.measures(lm.balance2))

########
#Part C#
########

# 1.
print(salary <- read.table("salary.txt", header = T))
HS_dum <- as.numeric(salary$educ == "1")
Col_dum <- as.numeric(salary$educ == "2")
Resp_dum <- as.numeric(salary$mgt == "1")
cat("My new data frame salary_dum with three dummy variables is as follows:\n")
print(salary_dum <- data.frame(YrsExp = salary$exper,
                               AttendHS = HS_dum,
                               AttendCol = Col_dum,
                               RespMgmt = Resp_dum,
                               Salary = salary$salary))

# 2. 
cat("A regression model that predicts salary from exper, educ, and mgt:\n")
print(lm.salary <- lm(Salary ~ YrsExp + AttendHS + AttendCol + RespMgmt,
                      data = salary_dum))

# 3. 
cat("It looks like salary and experience are the only scatter plots that
    make sense.")
pairs(salary)

# 4. 
pred.lm.salary <- fitted(lm.salary)
res.lm.salary <- resid(lm.salary)
plot(pred.lm.salary,res.lm.salary, main = "Residuals vs. Predicted")
plot(salary_dum$YrsExp,res.lm.salary, main = "Residuals vs. Yrs Exper")
plot(salary_dum$AttendHS,res.lm.salary, main = "Residuals vs. Attended HS")
plot(salary_dum$AttendCol,res.lm.salary, main = "Residuals vs. Attended College")
plot(salary_dum$RespMgmt,res.lm.salary, main = "Residuals vs. Mgmt Responsibilities")
qqnorm(res.lm.salary)

# 5. 
cat("According to my model, an additional year of experience is likely to
    produce an increase of $2,299.50 in salary.")

# 6.
cat("According to my model, a college graduate is predicted to earn more than
    a high school graduate by (621.5 - -12614.7) $13,236.2.")

# 7. 
cat("For a high school grad w/ 3 yrs exp. and no mgmt responsibilites, my model
    would predict this individual to earn a salary of (46444.2 + 2299.5*3 -12614.7)
    $40,728")

# 8.
cat("No, for this data set, a person with an advanced degree is expected to
    have a salary that is less than that of a person with a college degree by
    $621.5")

# 9.
cat("First I need to create an independent sample with the person in Problem
    7 to feed into my prediction function.")
print(independent.df <- data.frame(YrsExp=3,AttendHS=1,AttendCol=0,RespMgmt=0))
cat("The 95% prediction (default in R) interval for the person in Problem 7,
    using the predict() function, is [$31538.80, $49916.96]. The output is below:\n")
print(prediction.lm.salary <- predict(lm.salary,newdata = independent.df, interval = "predict"))