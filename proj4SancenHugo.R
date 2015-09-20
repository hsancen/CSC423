#Hugo Sancen
#Project4

#1 Input and print the HeartAttack dataset
cat("This is what the Heart Attack dataset looks like:\n")
print(heart <- read.table("heart.txt", header = T))

#2 Type the logit model equation for predicting the probability that a patient
#has a second attack (ha2) from ang and sco
cat("This is the output for the logit model using heart attack data\n")
print(glm.heart <- glm(ha2 ~ ang + sco, data = heart,
                       family = binomial(link = "logit")))
cat("We interpret this model as follows:\n")
cat("<the probability of having a second heart attach> = -1.024*<1 if patient has had anger mgmt> + 0.119*<score on trait anxiety scale> -6.363")

#3 Find the predicted probability of having a second heart attack for each observation
cat("The following are the predicted probabilities for having a second heart attack for each observation in the data:\n")
print(pred.glm.heart <- fitted(glm.heart, type = "response"))

#4 If a new patient has ang=1 and sco=35, calculate the probability of a second heart attack
obs <- data.frame(ang = 1, sco = 35)
cat("Using R to confirm:\n")
print(predict(glm.heart, newdata = obs))
