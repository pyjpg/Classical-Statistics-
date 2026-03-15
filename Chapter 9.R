# Example 9.1 Cotton / Tentsile strength data

rm(list = ls())
# Make the data frame 

r1 <- c(7,7,15,11,9)
r2 <- c(12,17,12,18,18)
r3 <- c(14,18,18,19,19)
r4 <- c(19,25,22,19,23)
r5 <- c(7,10,11,15,11)

strength <- c(r1,r2,r3,r4,r5)

# create list of cotton percentages 
p <- rep(c(15,20,25,30,35), c(5,5,5,5,5))
# Convert cotton percentages data to factor datatype
percent <- factor(p)

# Create dataframe with tensile strength and cotton percentages
fibre <- data.frame(strength, percent)

rm(r1, r2, r3, r4, r5, strength, p, percent)
fibre


# Calculate the point-estimates and 95% CIs for each treatment mean


# Mean for all the strength percentiles in the entry
mu_hat <-mean(fibre$strength)

# Mean for percentage 15
u1 <- mean(fibre$strength[fibre$percent==15])
# Mean for percentage 20
u2 <- mean(fibre$strength[fibre$percent==20])
# Mean for percentage 25
u3 <- mean(fibre$strength[fibre$percent==25])
# Mean for percentage 30
u4 <- mean(fibre$strength[fibre$percent==30])
# Mean for percentage 35
u5 <- mean(fibre$strength[fibre$percent==35])

# t_hat for each of the percentages
t_hat1 <- (u1 - mu_hat)
t_hat2 <- (u2 - mu_hat)
t_hat3 <- (u3 - mu_hat)
t_hat4 <- (u4 - mu_hat)
t_hat5 <- (u5 - mu_hat)

# ANOVA Table analysis of strength againist percentage
fibre.aov <- aov(strength ~ percent, data = fibre)
summary(fibre.aov)

# Chapter 10 residual analysis here and continue

# Calculate the observed residuals
fibre.residuals <- residuals(fibre.aov)
fibre.residuals

# For observed standard residuals for this particular dataset use MS-residuals and ni where ni = 5 in this case
sigma.squared.estimate <- 8.06

# Calculate observed standard residuals 
fibre.standardised.residuals <- fibre.residuals / sqrt(sigma.squared.estimate*(1-1/5))
fibre.standardised.residuals

# Plot the observed standardised residuals againist the observed fitted values
plot(fitted(fibre.aov), fibre.standardised.residuals)




