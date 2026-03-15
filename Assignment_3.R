rm(list = ls())


# load in the iris dataset
data_iris <- iris
iris

# Mean petal length for flowers of all types
mu_hat <-mean(data_iris$Petal.Length)

# Mean petal length for flower type "setosa"
u1 <- mean(data_iris$Petal.Length[data_iris$Species=="setosa"])

# Mean petal length for flower type "versicolor"
u2 <- mean(data_iris$Petal.Length[data_iris$Species=="versicolor"])

# Mean petal length for flower type "virginica"
u3 <- mean(data_iris$Petal.Length[data_iris$Species=="virginica"])


# t_hat value for flower type "setosa"
t_hat1 <- (u1 - mu_hat)

# t_hat value for flower type "versicolor"
t_hat2 <- (u2 - mu_hat)

# t_hat value for flower type "virginica"
t_hat3 <- (u3 - mu_hat)

# Create data frame specifically for petal length and species as those are the variables we are focusing on
species_petal_length <- data.frame(data_iris$Petal.Length, data_iris$Species)
species_petal_length
# ANOVA Table
species_petal_length.aov <- aov(data_iris$Petal.Length ~ data_iris$Species, data = species_petal_length)
summary(species_petal_length.aov)

# Work out the observed standard error
observed_standard_error <- sqrt(0.19)*sqrt(1/50+1/50)

# work out the least significant difference

lsd <- qt(0.975,147)*sqrt(0.19)*sqrt(1/50+1/50)
lsd

# u1 is the mean value for setosa and u3 is the mean value for virginica
difference <- u1 - u3

# calculate the lower and upper bounds for the 95% Confidence Interval
upper_bound <- difference + lsd
lower_bound <- difference - lsd

upper_bound
lower_bound


# Question 2

# Create a data frame subset only containing viriginca flowers petal length and petal width.
virginica_petal_info <- subset.data.frame(data_iris, data_iris$Species=="virginica", select = c(Petal.Length,
                                                                                                Petal.Width))

# Form a linear regression model 
virginica_petal_info.linearmodel <- lm(Petal.Length ~ Petal.Width, data = virginica_petal_info)

# Use summary to inspect the values in the linear regression model
summary(virginica_petal_info.linearmodel)

# ANOVA Table for virginica flower info
virginica_petal_info.aov <- aov(Petal.Length ~ Petal.Width, data = virginica_petal_info)
summary(virginica_petal_info.aov)

# Calculate the observed residuals
virginica.residuals <- residuals(virginica_petal_info.aov)
virginica.residuals

# For observed standard residuals and MS-residuals and ni where ni = 50 in this case
sigma.squared.estimate <- 0.2787

# Calculate observed standard residuals 
virginica.standardised.residuals <- virginica.residuals / sqrt(sigma.squared.estimate*(1-1/50))
virginica.standardised.residuals

# Plot the observed standardised residuals against the observed fitted values
plot(fitted(virginica_petal_info.aov), 
     virginica.standardised.residuals,
     xlab = "Fitted Values",
     ylab = "Standardised Residuals",
     main = "Fitted Values vs Standardised Residuals")

# Alternative equivalent method to produce the same scatter plot using the 'MASS' package
install.packages("MASS")
library(MASS)
plot(fitted(virginica_petal_info.linearmodel), 
     stdres(virginica_petal_info.linearmodel),
     xlab = "Fitted Values",
     ylab = "Standardised Residuals",
     main = "Fitted Values vs Standardised Residuals")


# Plot QQ-Plot of residuals against normal scores
qqnorm(stdres(virginica_petal_info.aov),
       main = "Normal QQ Plot of Standardised Residuals")
abline(0, 1)
