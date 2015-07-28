#1. Create datasets named bp1 and bp2
bp1 <- read.table('datasets\\bp1.txt', header =T)
bp2 <- read.table('datasets\\bp2.txt', header =T)  

#2. Print each dataset in 1
print(bp1)
print(bp2)

#3. For bp1 and bp2, obtain x   sx   Percentiles 1, 5, 25, 50, 75, 95, 99   Interquartile Range
bp1_x_Standing <- mean(bp1$Standing)
bp1_x_Supine <- mean(bp1$Supine)
bp1_sx_Standing <- sd(bp1$Standing)
bp1_sx_Supine <- sd(bp1$Supine)
bp1_quantile_Standing <- quantile(bp1$Standing, c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
bp1_quantile_Supine <- quantile(bp1$Supine, c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
bp1_iqr_Standing <- IQR(bp1$Standing)
bp1_iqr_Supine <- IQR(bp1$Supine)

bp2_x <- mean(bp2$Press)
bp2_sx <- sd(bp2$Press)
bp2_quantile <- quantile(bp2$Press, c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
bp2_iqr <- IQR(bp2$Press)

#4. Find 95% confidence intervals of the true pressures of the populations represented in bp1. 
#First, I will use qnorm(%) to get what my left and right bound z-values should be
#I use 0.025 as my % because the 95% confidence interval is essentially two-tailed so I cut it in half for the left side z-score
left_z <- qnorm(.025)

#5. Create side-by-side boxplots for the data in bp2. 
boxplot(bp2$Press ~ bp2$Pos)

#6. Create histogram
hist(bp1$Standing)
