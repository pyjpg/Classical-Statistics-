# Clear all variables
rm(list = ls())

# Install package MASS for leverages
install.packages("MASS")
library(MASS)
# Example 11.1 ~ stirring revs per minute | impurity percentages
# create stirring variable going from 20 - 42 in increments of 2
stirring.rpm <- seq(20,42,2)

# Input data
impurity.percentage <- c(8.4, 9.5, 11.8, 10.4, 13.3, 14.8, 13.2, 14.7, 16.4, 16.5, 18.9, 18.5)

# create data frame of rpms to impurities
paint.data <- data.frame(stirring.rpm,impurity.percentage) 

paint.data

attach(paint.data)
# Draw a scatter plot
plot(stirring.rpm, impurity.percentage, xlab="stirring rate (rpm)", ylab="impurity percentage")

# Perform a regression analysis on the paint data 
paint.linear.model <- lm(impurity.percentage ~ stirring.rpm, data=paint.data)
paint.linear.model
summary(paint.linear.model)
paint.aov <- anova(paint.linear.model)

# Example 11.4 observed Test statistic 
paint.aov

# Calculate Paint data frame observerd fitted values
paint.fitted.values <- fitted(paint.linear.model)
paint.fitted.values

# Calculate observed residuals
paint.residuals <- residuals(paint.linear.model)
paint.residuals

# Calculate the leverages using lm.influence()
paint.leverages <- lm.influence(paint.linear.model)$hat
paint.leverages

# Get the standard resdiauls using stdres() of the linear model
standard_res <- stdres(paint.linear.model)
standard_res

# Create a data frame to store the leverages againist the standard residuals
paint.diagnostics <- data.frame("leverages"=paint.leverages, "Standard residuals"=standard_res)
paint.diagnostics


# Exercise 11.1
initial.weight <- c(50,64,76,64,74,60,69,68,56,48,57,59,46,45,65)
weight.gain <- c(128,159,158,119,133,112,96,126,132,118,107,106,82,103,104)
kitty.data <- data.frame(initial.weight, weight.gain)


attach(kitty.data)
plot(initial.weight, weight.gain, xlab="initial weight (grams)", ylab="weight gain (grams)", main="weight gain vs inital weight")
kitty.linear.model <- lm(weight.gain ~ initial.weight, data=paint.data)
summary(kitty.linear.model)
kitty.linear.model

kitty.anova <- anova(kitty.linear.model)
kitty.anova

# There tends to be a somewhat linear relationship between initial weight and weight gain, with on average being 1.064 * initial weight larger however this is not very strong



# Exercise 11.b) Is there anything suspect

# My intuition tells me that this should be a comparsion againist leverages vs standard residuals and we should be able to outline
# What are our benchmarks for leverages and Standard residuals

# Leverages = h_hat = 2/n where n in our case is the number of kitty cats we got which is 15, so 2/15 x 3 = 2/5(0.4) is our benchmark for leverages
# Standard residuals benchmark is 95% of values within +- 2 so we need to find out the 95% CI for this
kitty.data
