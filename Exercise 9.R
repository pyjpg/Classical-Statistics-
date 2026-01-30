install.packages("multcomp")
library("multcomp")

# input the tensile strength data
soil <- data.frame(
  A = c(12.8, 13.4, 11.2, 11.6, 9.4, 10.3, 14.1, 11.9, 10.5, 10.4),
  B = c(8.1, 10.3, 4.2, 7.8, 5.6, 8.1, 12.7, 6.8, 6.9, 6.4),
  C = c(9.8, 10.6, 9.1, 4.3, 11.2, 11.6, 8.3, 8.9, 9.2, 6.4),
  D = c(16.4, 8.2, 15.1, 10.4, 7.8, 9.2, 12.6, 11.0, 8.0, 9.8)
)
soilA_Mean <- mean(soil$A)
soilB_mean <- mean(soil$B)
soil_stacked <- stack(soil)

soil_stacked.aov <- aov(values ~ ind, data = soil_stacked)
summary(soil_stacked.aov)
anova_table <- anova(soil_stacked.aov)

MS_residuals <- anova_table["Residuals","Mean Sq"]

CI_lb_soilA_soilB <- (soilA_Mean - soilB_mean - (qt(0.975,36)* sqrt(MS_residuals)*sqrt(1/10+1/10)))
CI_ub_soilA_soilB <- (soilA_Mean - soilB_mean + (qt(0.975,36)* sqrt(MS_residuals)*sqrt(1/10+1/10)))



Standard_Error <- sqrt(MS_residuals/10)
# SOil CI Lower bound for 95%
CI_lb_soilA <- (soilA_Mean-qt(0.975,36)*Standard_Error)
# SOil CI upper bound for 95%
CI_lb_soilB <- (soilA_Mean+qt(0.975,20)*Standard_Error)


# pairwise comparsion with soil as the factor
soil_stacked.comparisons <- glht(soil_stacked.aov, linfct=mcp(ind="Tukey"))
# Display CI's
confint(soil_stacked.comparisons, calpha=univariate_calpha())

# Exercise 9.3

# Values 5%,2.5%,1%,0.5% - 22 degrees freedom
pont_5 <- qt(0.95, df=22)
pont_25 <- qt(0.975, df=22)
pont_1 <- qt(0.99, df=22)
pont_05 <- qt(0.995, df=22)

# Bi.) Observed T value is 2.57 at degrees of free 2.57, i = find p-value for 1-tailed test which rejects H0
T_Obs <- 2.57
p_one_tail <- 1 -pt(T_Obs, df=22)

#


# Example 9.1 - 9.2 

# combine all tensile strength data into one list
strength <- c(r1, r2, r3, r4, r5)
# create a corresponding list of cotton percentages
p <- rep(c(15, 20, 25, 30, 35), c(5, 5, 5, 5, 5)) # or rep(c(15, 20, 25, 30, 35), each=5)
 # convert the cotton percentages data to the factor datatype
percent <- factor(p)
# create the data frame with tensile strength and cotton percentages for the columns
fibre <- data.frame(strength, percent)
# delete unwanted objects from memory
rm(r1, r2, r3, r4, r5, strength, p, percent)
# display the data frame
fibre

y1 <- mean(fibre$strength[fibre$percent==15])
y2 <- mean(fibre$strength[fibre$percent==20])
y3 <- mean(fibre$strength[fibre$percent==25])
y4 <- mean(fibre$strength[fibre$percent==30])
y5 <- mean(fibre$strength[fibre$percent==35])

fibre.aov <- aov(strength ~ percent, data = fibre)
summary(fibre.aov)

se_yi <- sqrt(8.06/5)


# CI for 9.8 as in that was the ui = 1 value a.k.a y1
CI_lb <- (9.8-qt(0.975,20)*sqrt(8.06/5))
CI_ub <- (9.8+qt(0.975,20)*sqrt(8.06/5))
          
          
# Exercise 9 Q2 (part 1)

SE_mean_moisture_diff <- sqrt(5.467) * sqrt(1/10 + 1/10)

# Example 9.2 Continuation.

s_sqrted = sqrt(8.06)
treat_mean_diff = s_sqrted* (sqrt(1/5+1/5))


least_sig_diff = (qt(0.975,20)*sqrt(8.06)*(sqrt(1/10+1/10)))

fibre.multcomp <- glht(fibre.aov, linfct = mcp(percent = "Tukey"))
# display results of the multiple comparison tests
summary(fibre.multcomp, test=univariate())



confint(fibre.multcomp, calpha=univariate_calpha())

          