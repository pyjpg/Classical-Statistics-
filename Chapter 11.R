# Clear all variables
rm(list = ls())

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




# Exercise 11.1
initial.weight <- c(50,64,76,64,74,60,69,68,56,48,57,59,46,45,65)
weight.gain <- c(128,159,158,119,133,112,96,126,132,118,107,106,82,103,104)
kitty.data <- data.frame(initial.weight, weight.gain)
