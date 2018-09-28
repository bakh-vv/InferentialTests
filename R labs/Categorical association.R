
#paire t-test
t.test(x = pre_weight, y = post_weight, paired = TRUE)


x <- c(3715, 1513)
n <- c(8442, 4321)
prop.test(x, n)


# get conditional probabilities
# Make the new data frame 'datastan' by copying 'data'
datastand <- data
# Make the first row of datastan into the old value of 'data', divided by the sum of the row 'data'
datastand[1,] <- data[1,] / sum(data[1,])
# Make the second row of datastan into the old value of 'data', divided by the sum of the row 'data'
datastand[2,] <- data[2,] / sum(data[2,])
# Make the third row of datastan into the old value of 'data', divided by the sum of the row 'data'
datastand[3,] <- data[3,] / sum(data[3,])
# Print your new 'datastand' table


# Calculate marginal probabilities
margcol <- colSums(data) / sum(data)
margrow <- rowSums(data) / sum(data)

# Empty data frame for holding expected probabilities
expProb <- data.frame()

#Loop to fill in data frame
for (i in 1:3){
  # Makes row 1 and column i into the expected joint probability based on marginal probability
  expProb[1,i] <- (margcol[i] * margrow[1])
  # Makes row 2 and column i into the expected joint probability based on marginal probability
  expProb[2,i] <- (margcol[i] * margrow[2])
  # Add code to makes row 3 and column i into the expected joint probability based on marginal probability
  expProb[3,i] <- (margcol[i] * margrow[3])
}

# Print expected probabilities
expProb

# Print observed probabilities
data/sum(data)


# Empty dataframe
expDat <- data.frame()

# Loop that makes i = 1, then i = 2, then i = 3
for (i in 1:3){
  # Makes row i and column 1 into the expected value
  expDat[i,1] <- (sum(data[i,]) * sum(data[,1])) / sum(data)
  # Makes row i and column 1 into the expected value
  expDat[i,2] <- (sum(data[i,]) * sum(data[,2])) / sum(data)
  #Add a line that calculates the third column values
  expDat[i,3] <- (sum(data[i,]) * sum(data[,3])) / sum(data)
}

# Print expected values
expDat


# Make a new object called "resid" containing your residuals
resid <- data - expDat
# Print resid
resid


# Calculate the Chi-square value for your data
sum(resid^2/expDat)

#df
(nrow(data) - 1)


# Calculate the chi-square value of 'data' using chisq.test()
chisq.test(data)


# Assign the correct value to m
m <- min(nrow(data), ncol(data)) - 1

# Calculate Cramér's V
chi_square <- as.numeric(chisq.test(data)[1])
sqrt(chi_square / sum(data) * m)


## Standardized Residuals
# Assign chi square output to object 'model'
model <- chisq.test(data)

# Print standardised residuals
model$stdres


## goodness of fit

# expected probabilities
probs <- c( 6 / 10, 3 / 10, 1 / 10 )

# run the chi-square goodness of fit test, assign to x
x <- chisq.test(observed, p = probs)

# Print x
x
