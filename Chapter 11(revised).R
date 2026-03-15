# Creating the pant data frame to experiment with linear regression on (simple)
stirring.rpm <- seq(20, 42, 2)
impurity.percentage <- c(8.4,9.5, 11.8, 10.4, 13.3, 14.8, 13.2, 14.7, 16.4, 16.5, 18.9, 18.5)
# create the data frame
paint.data <- data.frame(stirring.rpm, impurity.percentage)
rm(stirring.rpm, impurity.percentage)
paint.data

# Create the fitted linear model 
paint.linear.model <- lm(impurity.percentage ~ stirring.rpm, data=paint.data)

# use summary to display the linear model values
summary(paint.linear.model)

# Construct an ANOVA table for the linear model
anova(paint.linear.model)

# So 