#' test the hypothesis that there is an equal number of american citizens voting for 
#' Hillary Clinton and Bernie Sanders
binom.test(0.6*350, 350, 0.5, alternative = "two.sided")

#' State your conclusion in the variable conclusion. Assign it either the value of "rejected"
#' or the value of "accepted"
conclusion <- 'rejected'


# do a Wilcoxon test
wilcox.test(score_ipa, score_wheat, paired = TRUE, alternative = 'greater')

# assign your conclusion to the variable conclusion
conclusion <- 'rejected'


# do a Wilcoxon rank-sum test
wilcox.test(beer_data$rating ~ beer_data$group, alternative = 'greater')

# assign your conclusion to the variable conclusion
conclusion <- 'rejected'


# do a Kruskal-Wallis test
kruskal.test(rating ~ group, data = beer_data)


# assign your conclusion to the variable conclusion
conclusion <- 'not'




















































