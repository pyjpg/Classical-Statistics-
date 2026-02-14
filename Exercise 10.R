install.packages("multcomp")
library("multcomp")
install.packages("MASS")
library("MASS")

# Cotton tensile strength table
r1 <- c(7,7,15,11,9)
r2 <- c(12,17,12,18,18)
r3 <- c(14,18,18,19,19)
r4 <- c(19,25,22,19,23)
r5 <- c(7,10,11,15,11)

# Strength holds all the values
strength <- c(r1,r2,r3,r4,r5)
# P assigns the percentage of cotton to the values in our rows
p <- rep(c(15,20,25,30,35), each=5)
percent <- factor(p)
# Create the data frame called fibre
fibre <- data.frame(strength, percent)

# Perform Anova calculation
fibre.aov <- aov(strength ~ percent, data = fibre)
summary(fibre.aov)

# Retrieve the fitted values 
fitted_values <- fitted(fibre.aov)
fitted_values

# Retrieve the observed residuals
obs_residuals <- residuals(fibre.aov)
obs_residuals

# To workout the standarised residuals we take the residuals/ mean squared residual value from anova /sqrt(1-1/5(in our case number of percents))
standarised_residuals <- obs_residuals/(sqrt(8.06*1-1/5))
standarised_residuals


# Using the package 'MASS' to get the standarised residuals
stdres(fibre.aov)

# Example 10.3 deriving the normality of our distribution.

# summary stats for observed standardised residuals of fibres
summary(stdres(fibre.aov))

# Create breakpoints for historgram
break.points <- c(-1.75, -1.25, -0.75, -0.25, 0.25, 0.75, 1.25, 1.75, 2.25)

# [plot histrogram with obs standarised residuals and break points ]

hist(stdres(fibre.aov), breaks = break.points)

## possibly skewed to the right, not serious normality in error dist, but not enough data for any firm conclusions.

# Exercise 10.1 Find the observed residuals and fitted values 

# Set up the dataframe of investigation (Colombian molasses)

molasses <- data.frame(
  A = c(81.6, 81.3, 82.0, 79.6, 78.4, 81.8, 80.2, 80.7),
  B = c(81.8, 84.7, 82.0, 85.6, 79.9, 83.2, 84.1, 85.0),
  C = c(82.1, 79.6, 83.1, 80.7, 81.8, 79.9, 82.6, 81.9)
)
molasses_stacked <- stack(molasses)


# Set up the ANOVA for the molasses
A = c(81.6, 81.3, 82.0, 79.6, 78.4, 81.8, 80.2, 80.7)
B = c(81.8, 84.7, 82.0, 85.6, 79.9, 83.2, 84.1, 85.0)
C = c(82.1, 79.6, 83.1, 80.7, 81.8, 79.9, 82.6, 81.9)
quantity <- c(A,B,C)

location <- rep(c("Location A","Location B","Location C"), each=8)
molasses.aov <- aov(values ~ ind, data = molasses_stacked)
summary(molasses.aov)
mol_residuals <- residuals(molasses.aov)
mol_residuals
fitted_values <- fitted(molasses.aov)
fitted_values
standardised_residuals <- stdres(molasses.aov)
standardised_residuals
plot(fitted_values, standardised_residuals) 
