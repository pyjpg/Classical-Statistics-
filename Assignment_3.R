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
