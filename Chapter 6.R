# Comparing Groups: Statistical Tests
# Data for comparing groups
seg.df <- read.csv('http://goo.gl/qw303p')
summary(seg.df)

# Testing group frequencies
# One of the simplest statistical tests is the chi-square test, which is used 
# with frequency counts such as those produced by table. A chi-square test 
# determines whether the frequencies in cells are significantly different from
# what one would expect on the basis of their total counts.

# chi square test of goodness of fit: is the variation in data due to 
# randomness or because one of the variables we are measuring
# chi square statistic is higher than critical value, null hypothesis is 
# rejected. Chi square statistics will always be positive
# If the null hypothesis is rejected (p>0.05), it means that the differences between
# expected values and observed values are statistically important
# always expected frequencies have to be <5
# You use table() to find out the counts of data points for each categorical 
# variable
tmp.tab <- table(rep(c(1:4), times = c(25, 25, 25, 20)))  # goodness of fit test
chisq.test(tmp.tab)

tmb.tab <- table(rep(c(1:4), times = c(10, 17, 13, 29))) 
chisq.test(tmb.tab)

# Are there differences in segment sizes? Use chisq.test to find out
chisq.test(table(seg.df$Segment))   # goodness of fit test

# From the result (p = 0.0006035) you can assert that there are statistically
# significant differences in segment size

# Is subscription status independent from home ownership? (Null hypothesis = 
# both variables are unrelated)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

# Given p = 0.9187 we cannot reject the null hypothesis, data shows that home 
# ownership and subscription status are unrelated

# We should note two options for chisq.test(). First, for 2 × 2 tables, 
# chi-square statistic in light of the fact that the assumption of continuous 
# data is imperfect when data comes from a lumpy binomial distribution. If you 
# want the results to match traditional values such as calculation by hand or 
# spreadsheet, turn that off with correct=FALSE
chisq.test(table(seg.df$subscribe, seg.df$ownHome), correct = FALSE)

# Second, chisq.test() can calculate confidence intervals using a simulation 
# method, where it compares the observed table to thousands of simulated tables 
# with the same marginal counts. The p-value indicates the proportion of those
# simulations with differences between the cell counts and marginal proportions 
# at least as large as the ones in the observed table.
chisq.test(table(seg.df$subscribe, seg.df$ownHome), sim = TRUE, B = 100000)

# Testing observed proportions: binom.test()
# Check if observed binomial values is significantly different from expected 
# binomial values (it checks versus binomial probabilities)
# Null hypothesis is both observations are different ins a significant way
# p-value < 0.05 rejects null hypothesis
binom.test(12, 20, p = 0.5)   
# 12 observations of a sample size of 20 with a success probability of 50%

# About confidence intervals
# Our definition of a 95% confidence interval is this: it is the range of 
# possible estimates that we would expect to see 95% of the time if we 
# repeatedly estimate a statistic using random samples of the same sample size 
# under the assumption that the true value in an infinite or very large 
# population is the same as our current estimate. In other words, it is the 
# best guess of the range of possible answers we would expect with repeated 
# random samples. When the confidence interval excludes the null hypothesis 
# (such as a probability of 0.5 for equal chances, or a mean difference of 0 
# for no difference between groups), then the result is said to be 
# statistically significant.

# Statistical significance does not imply practical importance or the 
# meaningfulness of a result; a tiny difference can be statistically 
# significant with a large sample even when it is not actionable or 
# interpretable as a business matter. We should measure effect sizes.

# In practice, we suggest that before interpreting a result, make sure it is 
# statistically significant for some level of confidence interval (95%, or 
# possibly 90% or 99% depending on how sensitive the matter is). If it is not 
# significant, then your evidence for the result is weak, and you should not 
# interpret it. In that case, either say that, ignore the result, or collect 
# more data. If the result is significant, then proceed with your 
# interpretation and reporting (taking care with how you describe “confidence”) 
# Interpret results in light of their importance, not their statistical 
# significance (once it has been established). We recommend to report—and when
# appropriate, to chart—confidence intervals whenever feasible rather than 
# reporting single point estimates. By reporting CIs, one presents a more 
# complete and accurate description to stakeholders.

binom.test(120, 200, 0.5)
# If our expected probability falls outside our CI, it is very likely null
# hypothesis (observed values do not correspond to expected probability) cannot
# be rejected 

# With R, we can ask much more about the distribution. For example, what are 
# the odds that we would observe 8–12 Seattle fans out of 20, if the true rate 
# is 50%? We use the density estimate for a binomial distribution across the 
# range of interest and sum the point probabilities:
sum(dbinom(8:12, 20, 0.5))

# If we observe 20 fans, and the true split is 50%, there is a 73.6% chance 
# that we would observe between 8 and 12 fans (and thus a 1 − p or 27.3% chance
# of observing fewer than 8 or more than 12).

# An “exact” binomial test (the classical method) may be overly conservative in
# its estimation of confidence intervals [2]. One alternative method is to use
# binom.confint(, method="agresti-coull"), available in the binom package (you 
# may need to install that package):
library(binom)
binom.confint(12, 20, methods = 'ac')   # same a agresti-coull

# Chris also observed that among the 20 groups, 0 had a mixture of Seattle and
# Denver fans (as inferred from their team clothing). Based on that 
# observation, what should we conclude is the most likely proportion of groups
# that comprise mixed fans? We use the Agresti–Coull method because exact tests
# have no confidence interval for 0% or 100% observations”
binom.confint(0, 20, methods = 'ac')   

# expected probability of seeing groups containing fans of both groups based 
# on the observed value of 0 mixed groups on a sample of 20 groups

# Testing group means
# Before applying any statistical test or model, it is important to examine 
# the data and check for skew, discontinuities, and outliers. Many statistical
# tests assume that the data follows a normal distribution or some other smooth
# continuous distribution; skewness or outliers violate those assumptions and 
# might lead to an inaccurate test.
hist(seg.df$income)
with(seg.df, hist(income[ownHome == 'ownYes']))
with(seg.df, hist(income[ownHome == 'ownNo']))

# Is home ownership affected to differences in income?
# income is the response variable
# Null hypothesis: there is no difference between groups when it comes to home
# ownership
t.test(income ~ ownHome, data = seg.df)

# Data shows that people who own their homes have higher income. Null 
# hypothesis has been rejected
# Values of CI show that with 95% confidence, the difference in income between
# these two groups fall within those uopper and lower bounds

# How about the Travelers segment? Do home ownership is affected by differences
# in income?
t.test(income ~ ownHome, data = subset(seg.df, Segment == 'Travelers'))

# Rule of thumb: if 0 is within bounds of CI, it is very likely null hypothesis
# can't be rejected. Also p-value > 0.05. So there is not significant 
# difference in mean income betweer Traveles who own a house or not.

# Testing multiple group means: ANOVA
# ANOVA compares means of multiple groups: you can think of it as testing for 
# difference among multiple means, assuming that the groups have similar 
# variance.

# Which factors are related to differences in mean income in segment data?
seg.aov.own <- aov(income ~ ownHome, data = seg.df)
anova(seg.aov.own)

# The value of Pr(>F) for ownHome is the p-value and reflects that there is 
# significant variation in income between those who do and do not own their own
# homes.

seg.aov.seg <- aov(income ~ Segment, data = seg.df)
anova(seg.aov.seg)
# The value of Pr(>F) is very close to zero, confirming that income varies 
# significantly by segment.

# If income varies by both home ownership and segment, does that mean that a 
# more complete model should include both?
anova(aov(income ~ Segment + ownHome, data = seg.df))
# The results indicate that when we try to explain income differences in income
# by both Segment and ownHome, segment is a significant predictor (p≪0.01) but 
# home ownership is not a significant predictor. Yet the previous results said 
# that it was significant. What’s the difference? What is happening is that 
# segment and home ownership are not independent, and the effect is captured 
# sufficiently by segment membership alone. Home ownership accounts for little
# more over and above what can be explained by Segment.

# Could it be that home ownership is related to income in some segments but not
# in others? This would be represented in our model by an interaction effect. 
# In a model formula, “+” indicates that variables should be modeled for main 
# effects only. We can instead write “:” for an interaction or “∗” for both 
# main effect and interaction. We test main effects and interaction of home 
# ownership and segment:
anova(aov(income ~ Segment * ownHome, data = seg.df))
# Again, segment is a significant predictor, while home ownership and the 
# interaction of segment with home ownership are not significant. In other 
# words, segment membership is again the best predictor on its own.

# Model comparison: ANOVA
# We can compare models for better fit
anova(aov(income ~ Segment, data = seg.df),
      aov(income ~ Segment + ownHome, data = seg.df), data = seg.df)
# This tells us that Model 2 —which includes both segment and home ownership— 
# is not significantly different in overall fit from Model 1. If it were
# better, the null hypothesis of no difference would be rejected, as shown by a
# p-value (“Pr(>F)”) less than 0.05.
# It is essential to note that model comparison as performed by the anova() 
# command only makes sense in the case of nested models. In this context, a 
# model A is nested within another model B when one or more parameters of B 
# can be fixed or removed to yield model A. In the present case, income ∼ 
# Segment is nested within income ∼ Segment + ownHome because we can remove
# ownHome and arrive at the former model. Because they are nested, the two
# models may be compared by anova() or other functions that perform likelihood 
# comparisons.
# If we want to compare models that are not nested, we would have to review
# the literature on Akaike information criterion (AIC) and Bayesiean 
# information criterion (BIC)

# Visualizing group confidence intervals
library(multcomp)
seg.sov <- aov(income ~ -1 + Segment, data = seg.df) 
# we remove the intercept with -1
glht(seg.sov)
par(mar = c(6, 10, 2, 2))
plot(glht(seg.sov), xlab = 'Income', 
     main = 'Average Income by Segment (95% CI)')

# Variable selection ANOVA: Stepwise modeling
# Building models iteratively by adding and removing variables is a common task
# that can be automated with the step(model) command. This performs stepwise 
# model selection by testing models one at time while changing the variables in
# the model to see whether the change improves the model.
# The formula is expressed by the response variable (Income) and all the other
# variables (~ .). If we want to use some data transformation, we can use .^2
# based on the type of independent variable we are using. (Not recommended)
seg.aov.step <- step(aov(income ~ ., data = seg.df))
# Segment is the variable that best predicts income 

anova(seg.aov.step)
# Variable selection is better informed by lasso or random forest (Chapter 11)

# Bayesian ANOVA
set.seed(96761)
library(BayesFactor)
# We set a pseudorandom number seed because this function will take draws from 
# the posterior distribution. What does that mean? Briefly, a common way to 
# estimate a Bayesian model is to do repeated assessments of how well a 
# proposed model fits the data.

seg.bfg1 <- lmBF(income ~ Segment, data = seg.df)
# The end result is a large sample of possible parameters and their 
# likelihoods, or in other words, an outline of the most likely parameters for 
# a given model.

# Compare original model with a second one
seg.bfg2 <- lmBF(income ~ Segment + ownHome, data = seg.df)

# Model comparison in BayesFactor is performed by using the “/” operator to 
# find the ratio of the models’ Bayes Factors. We have the first model seg.bf1
# from above, and now fit the second model with two factors that we wish to 
# compare
seg.bfg1/seg.bfg2

# This tells us that the ratio of Bayes Factors for model 1 ( ∼ Segment) vs.
# model 2 ( ∼ Segment + ownHome) is 6.58. This means that model 1 is the 
# preferable model by a factor of 6.5.

# To find the model parameters and their credible ranges, we use the posterior 
# (model, index, draws) command to draw 10,000 samples of the possible 
# parameters from model 1
seg.df.chain <- posterior(seg.bfg1, index = 1, iterations = 100000)
# The draws are known as a chain because they are estimated by a Markov chain
# process

# Before we examine the estimates, we should inspect whether the draws 
# converged to stable values such that the estimates are reliable.
plot(seg.df.chain[, 1:6])

# We interpret the charts as follows. On the left, we see the estimated 
# parameter values (Y axis) plotted against the draw sequence (X axis). These
# form a fat but straight line, which means the estimates varied around a 
# stable central point; thus, they converged. (If they had not converged, the 
# plot would show erratic variations up or down, or would spread out 
# increasingly rather than being straight.)
# On the right, we see a density plot of the values. The density shape is 
# approximately normal, which matches the assumption of the regression model. 
# Thus, the charts confirm that the model was stable and converged (note that 
# these don’t mean the model is useful, only that it achieved a stable 
# estimate).

# Inspecting the Posterior Draws
# A simple summary() of the chain shows us the estimates:
summary(seg.df.chain)
# The first section of the summary (“1. Empirical mean and...”) gives 
# arithmetic central tendency estimates for the 10,000 draws of each of the 
# parameters in the chain: the mean of each parameter, the standard deviation 
# of that estimate across the 10,000 draws, and so forth. The second result 
# (“Quantiles...”) is what we prefer to use instead; it reports the actual 
# observed quantiles for each of the parameters.

# Note that the model estimates an overall mu that is the best guess for the 
# population mean regardless of segment effects, and then estimates each 
# segment as a deviation from that.

# To estimate the direct values for each segment, we add the population value 
# (mu) to the deviations for each segment. However, we cannot simply do that 
# with the aggregate numbers here by adding the mu row to each of the other 
# rows. Why not? Because the best estimates of segment totals are found within 
# each draw; we need to compute segment values at that level and then summarize
# those estimates. Luckily that is easy to do in R.

head(seg.df.chain)
seg.df.chain[1:4, 1:5]  
# We split the dataframe in the average (mu) and the segments

seg.df.chain[1:4, 1:5] + seg.df.chain[1:4, 1] # We add up mu to segments
seg.bf.chain.total <- seg.df.chain[, 2:5] + seg.df.chain[, 1]   
# We add up mu to all segments in this df

seg.bf.ci <- t(apply(seg.bf.chain.total, 2, 
                     quantile, pr = c(0.025, 0.5, 0.975)))
# In the apply() command, we applied the quantile() function to the columns 
# with the probabilities that we wanted for a 95% credible interval. Then we 
# transposed the result with t() to be more readable (treating the segments as 
# “cases”).

seg.bf.ci
# Those values are the best estimates of the 95% credible range for the 
# estimate of average income as modeled by segment, under the assumptions of 
# our model.

# Plotting the Bayesian Credible Intervals
library(ggplot2)
seg.bf.df <- data.frame(seg.bf.ci)
seg.bf.df$Segment <- rownames(seg.bf.df)
p <- ggplot(seg.bf.df, aes(x = Segment, y = X50., ymax = X97.5., ymin = X2.5.))
p <- p + geom_point(size = 4) + geom_errorbar(width = 0.2) + ylab('Income')
p + ggtitle('95% CI for Mean Income by Segment') + coord_flip()
