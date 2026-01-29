diamonds <- read.csv("diamonds.csv", header = TRUE)
dim(diamonds)
head(diamonds)
tail(diamonds)

# For the price variable

price <- diamonds$price


sample_min = min(price)
sample_max = max(price)
sample_mean = mean(price)
sample_standard_deviation = sd(price)
sample_median = median(price)
sample_lower_quart = quantile(price, p = 0.25)
sample_upper_quart = quantile(price, p = 0.75)
IQR = (sample_upper_quart - sample_lower_quart)
range_price = diff(range(price))

# beyond 1.5 x IQR is an outlier
lower_limit <- sample_lower_quart - (1.5*IQR)
upper_limit <- sample_upper_quart + (1.5*IQR)

# Boxplot reveals that there are outliers past the price 11886
boxplot(price, main = "Price of Diamonds", ylab = "Price")
# As the mean is > than the median 3932.8 > 2401 we can say that Histogram is positively skewed
hist(price, breaks = "Sturges", xlab="Price", col="lightblue", border="red", main = "Price of Diamonds")
abline(v = mean(price), col = "blue", lwd = 2, lty = 2)
abline(v = median(price), col = "darkgreen", lwd = 2, lty = 2)
legend("topright", legend = c(paste("Mean =", round(mean(price), 2)), 
                              paste("Median =", round(median(price), 2))), 
       col = c("blue", "darkgreen"), lty = 2, lwd = 2)

