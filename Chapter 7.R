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
