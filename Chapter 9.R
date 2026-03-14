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

