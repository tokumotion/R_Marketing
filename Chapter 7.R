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
