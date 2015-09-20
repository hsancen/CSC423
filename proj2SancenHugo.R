#Project 2
#Hugo Sancen (hugosancen@gmail.com)

###########
#Part A ###
###########

#1. Create and print a dataframe named flour
cat("The dataset 'flour' contains the following:\n")
print(flour <- read.table("flour.txt",header = TRUE))

#2. Compute the means and standard deviations for weight and nbags.
# Also compute the correlation between weight and nbags.
cat("\nThe mean of the weight is:")
print(xbar_weight <- mean(flour$Weight))
cat("The st. dev. of the weight is:")
print(sd_weight <- sd(flour$Weight))

cat("\nThe mean of nbags is:")
print(xbar_nbags <- mean(flour$NBags))
cat("The st. dev. of bnags is:")
print(sd_nbags <- sd(flour$NBags))

cat("\nThe correlation b/w weight and nbags is:")
print(corr_flour <- cor(flour$Weight,flour$NBags))

#3. Compute the regression model by hand using the formula: y - y = (rxy sy / sx)(x - x)

#4. Find the simple linear regression model for predicting nbags from weight. 
# Compare your hand calculations in Question A3 to the simple linear regression model obtained.
cat("\nUsing R, the simple linear regression model obtained is:")
print(linregflour <- lm(flour$NBags ~ flour$Weight, data = flour))
cat("Compared to my hand calculation, the R model is pretty acurate. My coefficient for X (weight) is very precise.
    It looks like my y-intercept is off slightly.\n\n\n\n")

#5. Create and interpret the residual plot and normal plot of the residuals. 
# Looking for "well-behaved" residuals in addition to an appropriately large R2 value.
res_linregflour <- residuals(linregflour)
pred_linregflour <- fitted(linregflour)
plot(pred_linregflour,res_linregflour, main = "Residual Plot of Flour Dataset",xlab = "Predicted Values", ylab = "Residuals",ylim = c(-50,50))
cat("The residual plot for the simple linear regression model of the flour dataset shows that the residuals
    appear to be biased. The average of the residuals for all ranges of predicted values do not appear to average
    to zero. Also, the residuals appear to be heterocedastic. The residual plot almost has a cone shape, implying that
    the residuals do not have the same st. dev. for ALL ranges of predicted values.\n\n")

qqnorm(res_linregflour, main = "Normal Plot for Residuals of Flour Data")
cat("The normal plot for the residuals of the flour data shows that the residuals distribution appears to be skewed to the right.\n\n\n\n")

#6. Compute the regression through the origin model by hand using the formula: 
# a = (x1y1 + ... + xnyn) / (x12 + ... + xn2),     y = ax.
cat("--If x1...xn is:", flour$Weight, "\n")
cat("--And y1...yn is:", flour$NBags,"\n")
cat("--Then x1*y1...xn*yn is:", flour$Weight*flour$NBags, "\n")
cat("--And the sum of this vector of products is:", sum(flour$Weight*flour$NBags), ". This is the value for the numerator of a.\n\n")
cat("--The denominator of a is equal to the sum of the squared independent variables.\n")
cat("--As previously stated, x1...xn is:", flour$Weight,"\n")
cat("--This vector squared is:", flour$Weight*flour$Weight,"\n")
cat("--The sum of this vector squared is:", sum(as.numeric(flour$Weight*flour$Weight)),". This is the value for the denominator of a.\n\n")
cat("--Thus, a = ",sum(flour$Weight*flour$NBags), "/",sum(as.numeric(flour$Weight*flour$Weight))," = ", (sum(flour$Weight*flour$NBags)) / (sum(as.numeric(flour$Weight*flour$Weight))),"\n")
cat("--The regression through the origin model for the flour dataset is: y = 0.0215X.\n\n\n\n")

#7. Find the regression through the origin model for predicting nbags from weight. 
#Compare your hand calculation in Question B6 to the regression through the origin model obtained. 
cat("\nUsing R, the regression model obtained is:")
print(linregflourOrigin <- lm(flour$NBags ~ flour$Weight + 0, data = flour))
cat("Compared to my hand calculation, the R model is pretty acurate. My coefficient for X (weight) is very precise.")

#8. Create and interpret the residual plot and normal plot of the residuals.
resOrigin <- residuals(linregflourOrigin)
predOrigin <- fitted(linregflourOrigin)
plot(predOrigin, resOrigin, main = "Residual Plot for Flour Regression Model thru Origin", ylim = c(-60,60))
cat("The residual plot for regression model thru the origin of the flour dataset shows that the residuals
    appear to be biased. The average of the residuals for all ranges of predicted values do not appear to average
    to zero. Also, the residuals appear to be heterocedastic. The residual plot almost has a cone shape, implying that
    the residuals do not have the same st. dev. for ALL ranges of predicted values.\n\n")

qqnorm(resOrigin, main = "Normal Plot of Flour Regression thru Origin")
cat("The normal plot for the residuals of the flour data shows that the residuals distribution appears to be skewed to the right.\n\n\n\n")

###########
#Part B ###
###########

#1. Print the data frame called UsedCars
cat("I acquired my used car dataset from Cars.com. I am using an Acura Integra as my make and model which has somewhat of a cult following among Japanese import enthuasiasts.
    My UsedCar data frame looks like this:\n")
print(UsedCars <- read.table("UsedCars.txt", header = TRUE))

#2. Create the pairwise scatterplots of year, miles, and price.
cat("The pairwise scatterplots of year, miles, and price looks like this:\n\n\n\n")
pairs(UsedCars)

#3. Find the pairwise correlations of year, miles, and price. Interpret them. 
cat("The pairwise correlations for my used car dataset is as follows:\n")
print(cor(UsedCars))
cat("\nThere are appears to be a modest correlation between all of my variable pairs. Price and year have a roughly positive linear relationship with
    correlation of 0.46. Price and Miles also have a roughly negative relationship with a correlation of -0.47. Year and Miles have the weakest absolute
    linear relationship (which is a negative linear relationship) with a correlation of -0.32.\n\n\n\n")

#4. Find the simple linear regression model price=year
cat("The simple linear regression model for price=year with price as the dependent is: Price^ = 403.7*Year - 801711.2 \n")
print(price_year <- lm(UsedCars$Price ~ UsedCars$Year, data = UsedCars))

#5. Create the residual plot residuals*predicted and the normal plot of the residuals. Interpret these plots.
cat("The residual plot for price = year appears to be biased because the residuals do not average zero for ALL ranges of predicted values.
    The residual plot is also heteroscedastic as it appears to have a cone shape with a widening standard deviation for the predicted values at higher
    ranges. A strong linear regression model should have an unbiased estimator and homoscedastic errors.")
respriceyear <- resid(price_year)
predpriceyear <- fitted(price_year)
plot(predpriceyear,respriceyear, main = "Residuals of price=year",ylim = c(-7000,7000))
cat("The normal plot for the residuals of my price=year model appear to be normally distributed, satisfying the requirement and ensuring that I
    can use t-distributions to obtain accurate confidence intervals for the estimated regression parameters.")
qqnorm(respriceyear,main = "Normal Plot for Residuals of price=year")

#6. Find the simple linear regression model price=miles
cat("The simple linear regression model for price=miles with price as the dependent is: Price^ = -0.02*Miles + 8077.28 \n")
print(price_miles <- lm(UsedCars$Price ~ UsedCars$Miles, data = UsedCars))

#7. Create the residual plot residuals*predicted and normal plot of the residuals. Interpret these plots
cat("The residual plot for price = miles appears to be less biased that price=years and could be deemed acceptable.However, 
    the residual plot is also heteroscedastic as it appears to have a cone shape with a widening standard deviation for the predicted values at higher
    ranges. A strong linear regression model should have an unbiased estimator and homoscedastic errors.")
respricemiles <- resid(price_miles)
predpricemiles <- fitted(price_miles)
plot(predpricemiles, respricemiles, main = "Residual Plot of price=miles", ylim = c(-7000,7000))
cat("The normal plot for the residuals of my price=miles model appear to be normally distributed, satisfying the requirement and ensuring that I
    can use t-distributions to obtain accurate confidence intervals for the estimated regression parameters.")
qqnorm(respricemiles,main = "Normal Plot for Residuals of price=miles")

#8. Find the multiple linear regression model price=year miles
cat("The multiple linear regression model for price=year+miles with price as the dependent is: Price^ = 3.01*Year - 1.73*Miles - 5.94 \n")
print(price_year_miles <- lm(UsedCars$Price ~ UsedCars$Year + UsedCars$Miles, data = UsedCars))

#9. Create residual plots and the normal plot of the residuals.
#Create these three residual plots: residuals*predicted, residuals*year, and residuals*miles. Interpret these plots. 
respriceyearsmiles <- resid(price_year_miles)
predpriceyearsmiles <- fitted(price_year_miles)
cat("For the residual plot of residuals*predicted I would say that is is more bias than price=miles, especially at the more extreme values. The residual
    plot also continues to be heteroscedastic like the other simple regression models.")
plot(predpriceyearsmiles, respriceyearsmiles, main = "Residual Plot of price = year + miles", ylim = c(-7000,7000))
cat("For the residual plot of residuals*year, this plot is the most homoscedastic I have seen so far and appears to be pretty unbiased. However, it
    the residuals start to look poorly behaved at the extreme values.")
plot(UsedCars$Year, respriceyearsmiles, main = "Residual Plot of residuals*year", ylim = c(-7000,7000))
cat("For the residual plot of residuals*miles, this plot is the most unbiased I think I have seen. However, it appears to be heteroscedastic
    at the low end of the car miles.")
plot(UsedCars$Miles, respriceyearsmiles, main = "Residual Plot of residuals*miles", ylim = c(-7000,7000))

#10. Which is the best regression model.  Explain your answer.
cat("I would say that the price=year+miles multiple regression model was the best. The R^2 and Adjusted R^2 values were the highest for this model compared
    R^2 of about 0.2 for the other models. Also, the multiple regression model appeared to have the most well-behaved residuals, all things considered, when excluding
    the extreme ends of the data. I expect this is appropriate because I imagine it is generally very difficult to fit your model to consider the extremem cases.
    They might have even been outliers and I did not control for them.")
