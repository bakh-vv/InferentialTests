# The predictor variable
money<- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# The response variable
liking <- c(2.2, 2.8, 4.5, 3.1, 8.7, 5.0, 4.5, 8.8, 9.0, 9.2)


# Vector containing the amount of money you gave participants (predictor)
money  <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Vector containing the amount the participants liked you (response)
liking <- c(2.2, 2.8, 4.5, 3.1, 8.7, 5.0, 4.5, 8.8, 9.0, 9.2)

# Correlation between money and liking
cxy <- cor(money, liking)

# Standard deviation of money
sx <- sd(money)

# Standard deviation of liking
sy <- sd(liking)

# Calculate the the regression slope using cxy, sx and sy
slope <- cxy * sy/sx



# Calculate the intercept
intercept <- mean(liking) - slope * mean(money)

# Print the value of the intercept
intercept

lm(liking~money)


# Calculate the R squared of our regression model using cor()
cor(money, liking)^2

# Assign the summary of lm(liking ~ money) to 'sum'
sum <- summary(lm(liking ~ money))

# Print sum
sum


nh <- "There will be no relationship between money and liking"

ah <- "More money will be related to more liking"


nh <- "There will be no relationship between money and liking"

ah <- "More money will be related to more liking"


## confidence interval for the slope

# Calculate the upper confidence interval
upper <- 0.7782 + 2.306 * 0.1847

# Calculate the lower confidence interval
lower <- 0.7782 - 2.306 * 0.1847

# Print the upper confidence interval
upper

# Print the lower confidence interval
lower


# Assign regression model to variable "mod1"
mod1 <- lm(liking ~ money)

# Obtain the residuals from mod1 using $, assign to "resmod1"
resmod1 <- mod1$residuals

# Print the residuals
resmod1


# plot the residuals on the y-axis, and liking on the x-axis
plot(liking, resmod1)

# plot the residuals on the y-axis, and money on the x-axis
plot(money, resmod1)



# You can see from the plots that while the residuals seem fairly even on the 'money' plot, there is a slight pattern in the 'liking' plot, such that residuals seem to be larger at higher levels of liking, and lower at lower levels of liking. This pattern suggests that our data might not entirely meet the assumption of linearity or homoscedasticity.


# Make a histogram of your residuals
hist(resmod1)


# Value for which you would like a prediction
nd <- data.frame( money=3)

# Find the prediction interval
predict(mod1, nd, level = 0.95, interval = "predict")


# Find the confidence interval
predict(mod1, nd, level = 0.95, interval = "confidence")












