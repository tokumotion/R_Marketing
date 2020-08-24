# An important thing to understand is that driver does not imply causation. A 
# linear model only assumes an association among variables. Consider a survey 
# of automobile purchasers that finds a positive association between 
# satisfaction and price paid. If a brand manager wants customers to be more 
# satisfied, does this imply that she should raise prices? Probably not. It is 
# more likely that price is associated with higher quality, which then leads to
# higher satisfaction. Results should be interpreted cautiously and considered
# in the context of domain knowledge.
# Linear models are a core tool in statistics, and R provides an excellent set
# of functions for estimating them. As in other chapters, we review the basics
# and demonstrate how to conduct linear modeling in R, yet the chapter does not
# review everything that one would wish to know in practice. We encourage 
# readers who are unfamiliar with linear modeling to supplement this chapter 
# with a review of linear modeling in a statistics or marketing research 
# textbook, where it might appear under a name such as regression analysis, 
# linear regression, or least-squares fitting.

# Amusement Data
set.seed(08226)
nresp <- 500  # number of survey respondents

# Our hypothetical survey includes four questions about a customer’s 
# satisfaction with different dimensions of a visit to the amusement park: 
# satisfaction with rides (rides), games (games), waiting times (wait), and 
# cleanliness (clean), along with a rating of overall satisfaction (overall). 
# In such surveys, respondents often answer similarly on all satisfaction 
# questions; this is known as the halo effect.
# We simulate a satisfaction halo with a random variable for each customer, 
# halo, that does not appear in the final data but is used to influence the 
# other ratings:
halo <- rnorm(n = nresp, mean = 0, sd = 5)  # Simulate satisfaction halo
<<<<<<< HEAD

# We generate responses for the satisfaction ratings by adding each 
# respondent’s halo to the value of another random variable that is specific to
# the survey item (satisfaction with rides, cleanliness, and so forth).

# We add a constant just to adjust the range slightly, and convert the 
# continuous values to integers using floor(). This gives us a final value for 
# each satisfaction item on a 100-point scale. Although scales rating 1–5, 1–7,
# or 1–11 may be more common in practice, such discrete scales introduce 
# complications that we discuss in 7.9; those would detract from our 
# presentation here. So we assume that the data comes from a 100-point scale. 
# Such near-continuous values might be obtained by measuring where respondents 
# mark levels of satisfaction along a line on paper or by touching a screen.
rides <- floor(halo + rnorm(n = nresp, mean = 80, sd = 3) + 1)
games <- floor(halo + rnorm(n = nresp, mean = 70, sd = 7) + 5)
wait <- floor(halo + rnorm(n = nresp, mean = 65, sd = 10) + 9)
clean <- floor(halo + rnorm(n = nresp, mean = 85, sd = 2) + 1)

# We generate this data using two functions: rlnorm(n, meanlog, sdlog) to 
# sample a lognormal distribution for distance, and sample(x, size, replace) to
# sample discrete distributions for weekend and number of children (num.child):
distance <- rlnorm(n = nresp, meanlog = 3, sdlog = 1)
num.child <- sample(x = 0:5, size = nresp, replace = TRUE, 
                    prob = c(0.3, 0.15, 0.25, 0.15, 0.1, 0.05))
weekend <- as.factor(sample(x = c('yes', 'no'), size = nresp, replace = TRUE, 
                            prob = c(0.5, 0.5)))

# We create the overall satisfaction rating as a function of ratings for the 
# various aspects of the visit (satisfaction with rides, cleanliness, and so 
# forth), distance traveled, and the number of children:
overall <- floor(halo + 0.5*rides + 0.1*games + 0.3*wait + 0.2*clean + 
                   0.03*distance + 5*(num.child==0) + 0.3*wait*(num.child > 0) +
                   rnorm(n = nresp, mean = 0, sd = 7) - 51)

# When a variable like overall is a linear combination of other variables plus
# random noise, we say that it follows a linear model. Although these ratings 
# are not a model of real amusement parks, the structure exemplifies the kind 
# of linear model one might propose. With real data, one would wish to discover
# the contributions from the various elements, which are the weights associated
# with the various predictors. In the next section, we examine how to fit such 
# a linear model.
sat.df <- data.frame(weekend, num.child, distance, rides, games, wait, clean, 
                     overall)
rm(nresp, weekend, distance, num.child, halo, rides, games, wait, clean, 
   overall)
summary(sat.df)

# Preliminary data inspection
gpairs::gpairs(sat.df)

# Convert distance to a normal distribution using log()
sat.df$logdist <- log(sat.df$distance)

# The pairwise scatterplots of our continuous measures are generally elliptical
# in shape, which is a good indication that they are appropriate to use in a 
# linear model.

# A common issue with marketing data and especially satisfaction surveys is 
# that variables may be highly correlated with one another. Although we as 
# marketers care about individual elements of customers’ experiences such as
# their amusement park experience with rides and games, when completing a 
# survey, the respondents might not give independent ratings to each of those
# items. They may instead form an overall halo rating and rate individual 
# elements of the experience in light of that overall feeling.

corrplot::corrplot.mixed(cor(sat.df[, c(2, 4:9)]), upper = 'ellipse')

# For example, to what extent is satisfaction with the park’s rides related
# to overall experience? Is the relationship strong or weak? One way to assess
# this is to plot those two variables against each other

plot(overall~rides, data = sat.df,
     xlab = 'Satifsfaction with rides', ylab = 'Overall Satisfaction')

# Linear Model with Single Predictor

m1 <- lm(overall~rides, data = sat.df)
plot(overall~rides, data = sat.df,
     xlab = 'Satifsfaction with rides', ylab = 'Overall Satisfaction')
abline(m1, col = 'blue')
summary(m1)

# The Std. Error column indicates uncertainty in the coefficient estimate, 
# under the assumption that the data are a random sample of a larger population

# The “t value”, p-value (“Pr(>|t|)”), and significance codes indicate a Wald
# test, which assesses whether the coefficient is significantly different 
# than zero. A traditional estimate of a 95% confidence interval for the 
# coefficient estimate is that it will fall within ± 1.96 × std.error. In this
# case, 1.7033 ± 1.96 × 0.1055=(1.495,1.910), rides estimate is 1.7152. So we 
# are confident—assuming the model is appropriate and the data are 
# representative—that the coefficient for ride is 1.495–1.910.

confint(m1)  # Confidence Interval

# The Residuals section in the summary(m1) output tells us how closely the 
# data follow the best fit line. A residual is the difference between the 
# model-predicted value of a point and its actual value. This is the vertical
# distance between a plotted point (actual value) and the blue line (predicted
# value).

# In the summary of m1, we see that the residuals are quite wide, ranging from
# − 33.597 to 34.699, which means our predictions can be quite a bit off for 
# any given data point (more than 30 points on the rating scale). The quartiles
# of the residuals suggest that they are fairly symmetric around 0. That is a 
# good sign that the model is unbiased (although perhaps imprecise).

# R-squared, a measure of how much variation in the dependent variable is 
# captured by the model. In this case, the R-squared is 0.3434, indicating 
# that about a third of the variation in overall satisfaction is explained by 
# variation in satisfaction with rides. When a model includes only a single 
# predictor, R-squared is equal to the square of the correlation coefficient r
# between the predictor and the outcome.

# F-statistic: provides a statistical test of whether the model predicts the 
# data better than simply taking the average of the outcome variable and using 
# that as the single prediction for all the observations. In essence, this test
# tells whether our model is better than a model that predicts overall 
# satisfaction using no predictors. In the present case, the F-statistic shows 
# a p-value < .05, so we reject the null hypothesis that a model without 
# predictors performs as well as model m1.

# Checking model fit

# The first is that the relationship between the predictors and the outcomes is
# linear. If the relationship is not linear, then the model will make 
# systematic errors. For example, if we generate data where y is a function of
# the square of x and then fit a linear model y ∼ x, this will draw a straight 
# line through a cloud of points that is curved.

x <- rnorm(500)
y <- x^2 + rnorm(500)
toy.model <- lm(y~x)
summary(toy.model)
plot(y~x)
abline(toy.model)

# This results in this plot and you can see from the plot that there is a 
# clear pattern in the residuals: our model under-predicts the value of y near
# zero and over-predicts far from zero.

# Another assumption of a linear model is that prediction errors—the parts of 
# the data that do not exactly fit the model—are normally distributed and look 
# like random noise with no pattern. One way to examine this is to plot the 
# model’s fitted values (the predictions) versus the residuals (the prediction
# errors).

plot(toy.model$fitted.values, toy.model$residuals)

# You can see from the plot that there is a clear pattern in the residuals: our
# model under-predicts the value of y near zero and over-predicts far from zero.
# The more points near 0, the more it underpredicts.

# R suggests four specific plots to assess the fit of linear model objects and 
# you can look at all four simply by using plot() with any lm object. To see 
# all four plots at once, we type par(mfrow=c(2,2))

par(mfrow=c(2,2))
plot(m1)

# The first plot (in the upper left corner) shows the fitted values versus 
# residuals for m1, just as we produced manually for our toy y ∼ x model. There
# is no obvious pattern between the fitted values for overall satisfaction and 
# the residuals; this is consistent with the idea that the residuals are due to
# random error, and supports the notion that the model is adequate.

# The second plot in the lower left is similar to the first, except that 
# instead of plotting the raw residual value, it plots the square root of the
# standardized residual. Again, there should be no clear pattern; if there were
# it might indicate a nonlinear relationship. Observations with high residuals 
# are flagged as potential outliers, and R labels them with row numbers in case 
# we wish to inspect them in the data frame.

# A common pattern in residual plots is a cone or funnel, where the range of 
# errors gets progressively larger for larger fitted values. This is called 
# heteroskedasticity and is a violation of linear model assumptions. A linear
# model tries to maximize fit to the line; when values in one part of the range 
# have a much larger spread than those in another area, they have undue 
# influence on the estimation of the line. Sometimes a transformation of the 
# predictor or outcome variable will resolve heteroskedasticity.

# The third result of plot() for lm objects is a Normal QQ plot, as in the 
# upper right. A QQ plot helps you see whether the residuals follow a normal 
# distribution, another key assumption (see Sect. 3.4.3). It compares the 
# values that residuals would be expected to take if they are normally 
# distributed, versus their actual values. When the model is appropriate, these
# points are similar and fall close to a diagonal line; when the relationship 
# between the variables is nonlinear or otherwise does not match the 
# assumption, the points deviate from the diagonal line. In the present case, 
# the QQ plot suggests that the data fits the assumption of the model.

# We do not want one or a very few observations to have a large effect on the
# coefficients. The lower right plot plots the leverage of each point, a 
# measure of how much influence the point has on the model coefficients. When
# a point has a high residual and high leverage, it indicates that the point 
# has both a different pattern (residual) and undue influence (leverage).

# One measure of the leverage of a data point is Cook’s distance, an estimate 
# of how much predicted (y) values would change if the model were re-estimated
# with that point eliminated from the data. If you have observations with high 
# Cook’s distance, this chart would show dotted lines for the distances; in the
# present case, there are none.

# Fitting Linear Models with Multiple Predictors

# Looking first at the model fit statistics at the bottom of the output, we 
# see that our prediction was improved by including all the satisfaction 
# items in the model. The R-squared increased to 0.5586, meaning that about 
# half of the variation in overall ratings is explained by the ratings for 
# specific features. The residual standard error is now 10.59, meaning that 
# the predictions are more accurate. Our residuals also appear to be symmetric. 
# As noted above, we recommend also to inspect the model using plot() to 
# confirm that there are no patterns in the residuals indicative of 
# nonlinearity or outliers, although we omit that step here.
m2 <- lm(overall ~ rides + games + wait + clean, data = sat.df)
summary(m2)

par(mfrow = c(2, 2))
plot(m2)
# In Fig.7.5, the first plot (in the upper left corner) shows the fitted 
# values versus residuals for m1, just as we produced manually for our toy y∼x 
# model. In Fig.7.5 there is no obvious pattern between the fitted values for 
# overall satisfaction and the residuals; this is consistent with the idea that 
# the residuals are due to random error, and supports the notion that the model
# is adequate.
# The second plot in the lower left of Fig.7.5 is similar to the first, except 
# that instead of plotting the raw residual value, it plots the square root of 
# the standardized residual. Again, there should be no clear pattern; if there
# were it might indicate a nonlinear relationship. Observations with high 
# residuals are flagged as potential outliers, and R labels them with row
# numbers in case we wish to inspect them in the data frame.
# A common pattern in residual plots is a cone or funnel, where the range of
# errors gets progressively larger for larger fitted values. This is called
# heteroskedasticity and is a violation of linear model assumptions. A linear 
# model tries to maximize fit to the line; “when values in one part of the 
# range have a much larger spread than those in another area, they have undue 
# influence on the estimation of the line. Sometimes a transformation of the 
# predictor or outcome variable will resolve heteroskedasticity
# “The third result of plot() for lm objects is a Normal QQ plot, as in the 
# upper right of Fig.7.5. A QQ plot helps you see whether the residuals follow 
# a normal distribution, another key assumption (see Sect.3.4.3). It compares 
# the values that residuals would be expected to take if they are normally 
# distributed, versus their actual values. When the model is appropriate, these
# points are similar and fall close to a diagonal line; when the relationship 
# between the variables is nonlinear or otherwise does not match the 
# assumption, the points deviate from the diagonal line. In the present case, 
# the QQ plot suggests that the data fits the assumption of the model.
# The final plot in the lower right panel of Fig.7.5 again helps to identify 
# potential outliers, observations that may come from a different distribution 
# than the others. Outliers are a problem because, if they are far from other
# points, they unduly influence the fitted line. We do not want one or a very 
# few observations to have a large effect on the coefficients. The lower right 
# plot in Fig.7.5 plots the leverage of each point, a measure of how much 
# influence the point has on the model coefficients. When a point has a high 
# residual and high leverage, it indicates that the point has both a different 
# pattern (residual) and undue influence (leverage). One measure of the 
# leverage of a data point is Cook’s distance, an estimate of how much
# predicted (y) values would change if the model were re-estimated with that 
# point eliminated from the data. If you have observations with high Cook’s 
# distance, this chart would show dotted lines for the distances; in the 
# present case, there are none.
