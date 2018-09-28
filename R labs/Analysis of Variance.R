# calculate the overall mean and store it in a variable called grand_mean
grand_mean <- round(mean(song_data$duration), digits = 2)

# calculate the mean song duration of the classical songs and store it in a variable
#' called classical_average
classical_average <- round(mean(classical_data$duration), digits = 2)

# calculate the mean song duration of the hip hop songs and store it in a variable
#' called hiphop_average
hiphop_average <- round(mean(hiphop_data$duration), digits = 2)

# calculate the mean song duration of the pop songs and store it in a variable
#' called pop_average
pop_average <- round(mean(pop_data$duration), digits = 2)


# calculate the sample size of the classical genre
sample_classical <- nrow(classical_data)

# calculate the sample size of the hip hop genre
sample_hiphop <- nrow(hiphop_data)

# calculate the sample size of the pop genre
sample_pop <- nrow(pop_data)

# calculate the between group variance
between_group_variance <- (sample_classical*(classical_average - grand_mean)^2 + sample_hiphop*(hiphop_average - grand_mean)^2 + sample_pop *(pop_average - grand_mean)^2) / (3-1)


# calculate sum of squares for the classical genre and put it in a variable sum_squares_classical
sum_squares_classical <- sum((classical_data$duration - classical_average)^2) 

# calculate sum of squares for the hiphop genre and put it in a variable sum_squares_hiphop
sum_squares_hiphop <- sum((hiphop_data$duration - hiphop_average)^2) 

# calculate sum of squares for the pop genre and put in a variable sum_squares_pop
sum_squares_pop <- sum((pop_data$duration - pop_average)^2)

# calculate the within group variance and put it in a variable called within_group_variance
within_group_variance <- round((sum_squares_classical + sum_squares_hiphop + sum_squares_pop) / (nrow(song_data) - 3), 0)


# calculate the F statistic and store it in a variable called f_stat
f_stat <- round(between_group_variance/within_group_variance, 2)

# calculate the degrees of freedom and store it in the variables df1 and df2
df1 <- 3-1
df2 <- 48-3

# calculate the associated p value and store it in a variable called p_value
p_value <- round(pf(f_stat, df1, df2, lower.tail = FALSE), 2)


# check for normality in the classical genre
shapiro.test(classical_data$duration)

# check for normality in the hip hop genre
shapiro.test(hiphop_data$duration)

# check for homogeneity of variances using the bartlett test
bartlett.test(song_data$duration ~ song_data$genre)


# use the aov function and store the result in fit_aov
fit_aov <- aov(song_data$duration ~ song_data$genre)

# use the summary function on the object fit_aov
summary(fit_aov)

# use the lm function and store the result in fit_lm
fit_lm <- lm(song_data$duration ~ song_data$genre)

# use the summary function on the object fit_lm
summary(fit_lm)


#' follow up your anova with pairwise t tests using a bonferroni corection 
#' and print the output to the console
pairwise.t.test(song_data$duration, g = song_data$genre, p.adj = "bonf")

# do a Tukey test and print the output to the console.
TukeyHSD(fit_aov, which = 'song_data$genre')


# run a two-way anove and store it in the object two_way_fit
two_way_fit <- aov(song_data$duration ~ song_data$genre + song_data$continent)

# call the summary function on the object two_way_fit
summary(two_way_fit)


#' conduct a two-way anova with an interaction term of genre and continent
#' store the model in the object two_way_fit
two_way_fit <- aov(song_data$duration ~ song_data$genre + song_data$continent + song_data$genre:song_data$continent)

# call the summary function on the object two_way_fit and print the output to the console
summary(two_way_fit)






































































