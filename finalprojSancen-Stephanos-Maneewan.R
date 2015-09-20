#load in data
comm <- read.table("CommViolData.csv", sep = ",", header = T,
                   stringsAsFactors = F)

#####################
# variable selection
#####################

#let's look for patterns b/w variables using scatterplots
pairs(~violentPerPop + pct12.21 + pct12.29 + pct16.24 + pct65up, data = comm,
      main = "Chart 1\nScatter Plot #1")
pairs(~violentPerPop + medIncome + pctNotHSgrad + pctCollGrad + pctUnemploy,
      data = comm, main = "Chart 2\nScatter Plot #2")
pairs(~violentPerPop + pop + perHoush + pctAllDivorc + persHomeless,
      data = comm, main = "Chart 3\nScatter Plot #3")

#let's look at linear relationship using correlation matrices
commNumeric <- comm[,3:15]
print(cor(commNumeric))

#build boxplots for pop and persHomeless to identify outliers
boxplot(comm$pop, main = "Chart 4\nBoxplot for 'pop'")
boxplot(comm$persHomeless, main = "Chart 5\nBoxplot for 'persHomeless'")

#create a new data frame that excludes some of these very extreme outliers
#identified in the boxplots
NoOutliers <- comm[comm$pop < 2000000 & comm$persHomeless < 6000,]

#create new boxplots to see if they look better without the outliers
boxplot(NoOutliers$pop)
boxplot(NoOutliers$persHomeless)
NoOutliers <- comm[comm$pop < 2000000 & comm$persHomeless < 500,]
boxplot(NoOutliers$persHomeless)
NoOutliers <- comm[comm$pop < 2000000 & comm$persHomeless < 200,]
boxplot(NoOutliers$persHomeless)
NoOutliers <- comm[comm$pop < 2000000 & comm$persHomeless < 50,]
boxplot(NoOutliers$persHomeless)
hist(NoOutliers$persHomeless, main = "Chart 6\n'persHomeless' after removing
     93 largest observations")

#'persHomeless' is no longer a candidate. Will remove outliers for 'pop' only
boxplot(NoOutliers$pop)
q3 <- quantile(comm$pop, c(.75))
q1 <- quantile(comm$pop, c(.25))
fence <- q3 + (1.5*(q3-q1))
NoOutliers <- comm[comm$pop < fence,]
boxplot(NoOutliers$pop, main = "Chart 7\n'pop' after removing 200 outliers")
print(cor(NoOutliers[,3:15]))

##################
# model selection
##################

#split data into 50% regression data (regdata) and validation data (valdata)
# Set seed for random number generator so everyone 
# using this code gets the same results. 
set.seed(34839)
#define a random set of row numbers, the quantity of which is half my dataset
indexes <- sample(nrow(commNumeric), size = 0.5*nrow(commNumeric))
#use these row numbers to select my first half of data
traindata <- commNumeric[indexes,]
#use the remaining row numbers to select the second half of data
testdata <- commNumeric[-indexes,]

#define full model
full <- lm(violentPerPop ~ ., data = traindata)

#perform forward selection
null <- lm(violentPerPop ~ 1, data = traindata)
summary(step(null, scope = list(lower=null, upper=full), direction = "forward"))

#perform backward selection
summary(step(full, direction = "backward"))



#what is the f-interval for the model?
print(qf(0.999,df1 = 8, df2 = 988))



###########################################
#residual anaylsis for Model 1
###########################################

#build model1
model1 <- lm(violentPerPop ~ pop + perHoush + pct65up + pctNotHSgrad
             + pctCollGrad + pctUnemploy + pctAllDivorc + persHomeless,
             data = traindata)
print(summary(model1))

#model 1 residual plot
res.model1 <- resid(model1)
pred.model1 <- fitted(model1)
plot(pred.model1, res.model1, ylim = c(-3500,3500), main = "Chart 8\nResiduals for Model 1")


#model 1 residual normal plot
qqnorm(res.model1, main = "Chart 9\nModel 1 Residuals Normal Plot")

#create charts to check for influence points
plot(model1)

#use vif to check for multicollinearity
install.packages("car")
require("car")
print(vif(model1))

#############################
#log-log transformation
#############################

#grab the old dataset, exluding persHomeless
logcommNumeric <- data.frame(pop = commNumeric$pop,
                             perHoush = commNumeric$perHoush,
                             pct12.21 = commNumeric$pct12.21,
                             pct12.29 = commNumeric$pct12.29,
                             pct16.24 = commNumeric$pct16.24,
                             pct65up = commNumeric$pct65up,
                             medIncome = commNumeric$medIncome,
                             pctNotHSgrad = commNumeric$pctNotHSgrad,
                             pctCollGrad = commNumeric$pctCollGrad,
                             pctUnemplpy = commNumeric$pctUnemploy,
                             pctAllDivorc = commNumeric$pctAllDivorc,
                             violentPerPop = commNumeric$violentPerPop)

#remove observations of zero
logcommNumeric <- logcommNumeric[logcommNumeric$violentPerPop != 0,]

#log the new dataset
logcommNumeric <- log(logcommNumeric)

#what does new correlation matrix look like
cor(logcommNumeric)

###########################
#log-log model building
###########################

#split the dataset so we can do derive log model
set.seed(34839)
#define a random set of row numbers, the quantity of which is half my dataset
logindexes <- sample(nrow(logcommNumeric), size = 0.5*nrow(logcommNumeric))
#use these row numbers to select my first half of data
logtraindata <- logcommNumeric[logindexes,]
#use the remaining row numbers to select the second half of data
logtestdata <- logcommNumeric[-logindexes,]

#derive a full log-log model
logfull <- lm(violentPerPop ~ ., data = logtraindata)

#perform backward selection
summary(step(logfull, direction = "backward"))

#build model2
model2 <- lm(violentPerPop ~ pop + perHoush + pct16.24 + pct65up
             + pctNotHSgrad + pctCollGrad + pctUnemplpy + pctAllDivorc,
             data = logtraindata)
print(summary(model2))

#############################
#log-log model residuals
#############################


#model 2 residual plot
res.model2 <- resid(model2)
pred.model2 <- fitted(model2)
plot(pred.model2, res.model2, ylim = c(-3.5, 3.5), main = "Chart 10\nResiduals for Model 2")


#model 2 residual normal plot
qqnorm(res.model2, main = "Chart 11\nModel 2 Residuals Normal Plot")

#create charts to check for influence points
plot(model2, which = 1:6)

#use vif to check for multicollinearity
print(vif(model2))

########################
#model 3
#######################

#build model3
model3 <- lm(violentPerPop ~ pop + perHoush + pct16.24 + pct65up
             + pctCollGrad + pctUnemplpy + pctAllDivorc,
             data = logtraindata)
print(summary(model3))

#############################
#model3 residuals
#############################


#model 3 residual plot
res.model3 <- resid(model3)
pred.model3 <- fitted(model3)
plot(pred.model3, res.model3, ylim = c(-3.5, 3.5), main = "Chart 12\nResiduals for Model 3")


#model 3 residual normal plot
qqnorm(res.model3, main = "Chart 13\nModel 3 Residuals Normal Plot")

#create charts to check for influence points
plot(model3, which = 1:6)

#use vif to check for multicollinearity
print(vif(model3))

########################
#model 4
#######################

#build model4
model4 <- lm(violentPerPop ~ pop + perHoush + pct16.24 + pct65up
             + pctUnemplpy + pctAllDivorc,
             data = logtraindata)
print(summary(model4))

#############################
#model4 residuals
#############################


#model 4 residual plot
res.model4 <- resid(model4)
pred.model4 <- fitted(model4)
plot(pred.model4, res.model4, ylim = c(-3.5, 3.5), main = "Chart 12\nResiduals for Model 4")


#model 4 residual normal plot
qqnorm(res.model4, main = "Chart 13\nModel 4 Residuals Normal Plot")

#create charts to check for influence points
plot(model4, which = 1:6)

#use vif to check for multicollinearity
print(vif(model4))

##########################
#model crossvalidation
##########################

# Obtain predicted values from test set.
p = predict(model4, newdata=logtestdata)
cat("Predicted values from test dataset.\n")
print(p)

# Obtain residuals from test dataset.
r = logtestdata$violentPerPop - p
cat("Residuals from test dataset:\n")
print(r)


sse = sum(r^2)
deviations = logtestdata$violentPerPop - mean(logtraindata$violentPerPop)
sst = sum(deviations^2)
cat("R-squared for prediction:\n")
print(1 - sse/sst)

#now le'ts try k-fold validation
install.packages("DAAG")
require("DAAG")

#perform 5-fold crossvalidation
cv.lm(df = logtestdata, model4, m = 5, seed = 997)
