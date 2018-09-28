### Comparing two proportion

# calculate the difference in sample proportions and store it in a variable called difference
difference <-  0.6 - 0.42

# calculate the pooled estimate and store it in a variable called pooled
pooled <-  (0.6*100 + 0.42*150)/(100 + 150)


# calculate the standard error and store it in a variable called se
se <- sqrt(pooled * (1-pooled) * ((1/100) + (1/150)))


# calculate the z value and store it in a variable called Z_value
z_value <- difference / se


# calculate the associated p_value and store it in a variable called p_value
p_value <-  pnorm(z_value, lower.tail = FALSE) * 2

# decide what your decision is and put it in a variable called conclusion
conclusion <-  "rejected"



# Constructing the matrix
vote_behavior <- matrix(c(60, 63, 40, 87), ncol=2)
colnames(vote_behavior) <- c('left','no left')
rownames(vote_behavior) <- c('male','female')

# Call the prop.test function on the matrix vote_behavior
prop.test(vote_behavior, conf.level = 0.99, correct = FALSE)



### Comparing two means

# average difference between male and female sample in hours of sport per week
mean_difference <- 4.2 - 5.8

# standard error of the difference between male and female sample in hours of sport per week
se = sqrt((2.3^2/100)+(3.1^2/150))


# calculate the t score and assign it to the variable t_score
t_score <- mean_difference / se


# calculate the degrees of freedom and store it in a variable called df
df <- 100 + 150 -2

# calculate the p value
pt(t_score, df) * 2

# calculate the 99% confidence interval and print it to the console
lower_value = mean_difference - qt(0.995, df)* se
upper_value = mean_difference + qt(0.995, df)* se
c(lower_value, upper_value)



## assuming same variability

# calculate the pooled standard deviation and put it in a variable called pooled
pooled <- sqrt(((100-1)*2.8^2 + (150-1)*2.8^2)/(100+150 - 2))

# calculate the standard error and put it in a variable called se
se <- pooled * sqrt((1/100) + (1/150))



### Paired samples

## Two proportions
# calculate p1
200/335

# calculate p2
185/335


# perform a mcnemar test on the matrix europe
mcnemar.test(europe)



# calculate the variable x_diff
x_diff <- colMeans(weight)[1] - colMeans(weight)[2]

# calculate the variable std
std <- sd(pre_weight - post_weight)

# calculate the variable se
se <- std / sqrt(nrow(weight))


# calculate the degrees of freedom against which we are testing and store it in df
df = nrow(weight) - 1

# calculate the t value
t_value = x_diff/ se

# calculate the p value
p_value = pt(t_value, df, lower.tail = FALSE)*2
p_value











