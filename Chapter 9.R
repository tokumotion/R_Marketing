#### Additional Linear Modeling Topics ####
# data
cust.df <- read.csv('http://goo.gl/PmPkaG', stringsAsFactors = TRUE)
# need to use arg stringsAsFactors = TRUE in order for scatterplotMatrix() works

#### Handling Highly Correlated Variables ####

# first linear model
spend.m1 <- lm(online.spend ~ ., data = subset(cust.df[, -1], online.spend > 0))
summary(spend.m1)

# we visualize the correlation to find identify possible problems with 
# multicollinearity
library(car)
scatterplotMatrix(cust.df)

# scatterplotMatrix() shows skewed data and pairs of data highly correlated
# to fix this we transform the data with BoxCox.lambda() function
autoTransform <- function (x){
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

# we get the complete observations and drop first column of data.frame
cust.df.bc <- cust.df[complete.cases(cust.df), -1]
# then we take all obs with positive online.spend
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
# we drop the email column as it is not a predictor
numcols <- which(colnames(cust.df.bc) != 'email')
# run the transformation with the custom function
cust.df.bc[, numcols] <- lapply(cust.df.bc[, numcols], autoTransform)

# visualize tranformed data
scatterplotMatrix(cust.df.bc)

# refit the model with transformed data
spend.m2 <- lm(online.spend ~ ., data = cust.df.bc)
summary(spend.m2)

# spend.m2 still shows problems of correlated vars (online.spend and 
# online.transactions)
# we run spend.m3 to compare spend.m2 with a model which only predicts 
# transaccions)
spend.m3 <- lm(online.spend ~ online.trans, data = cust.df.bc)
anova(spend.m2, spend.m3)
# Pr(>F) which is the p-value shows that the difference between models is not
# random, therefore they are pretty much the same model which is a problem for
# predicting how the other vars work.

# fixing collinearity
vif(spend.m2)
# good rule of thumb, drop all vars with VIF > 5

# strategy 1: drop highly correlated variables
spend.m4 <- lm(online.spend ~ . - store.trans - online.trans, data = cust.df.bc)
# check for collinearity
vif(spend.m4)
# remaining vars show VIF < 5, we have now a stable linear model.
summary(spend.m4)
# standard errors of all vars are much lower. R2 is still very high (0.8791) but
# we can reliably say that online.spend is the best predictor of the model

# strategy 2: run PCA
# remember, PC are uncorrelated (orthogonal) so can be used in linear models
# we will extract 2 components, one for the online variables and one for the 
# store vars
pc.online <- prcomp(cust.df.bc[, c('online.visits', 'online.trans')])
cust.df.bc$online <- pc.online$x[, 1]
pc.store <- prcomp(cust.df.bc[, c('store.trans', 'store.spend')])
cust.df.bc$store <- pc.store$x[,1]
# we a4e creating new variables (components) to which we will try to find 
# correlations with the explanatory variable (online.spend). because these 
# components are orthogonal, they can be safely used in linear models

# then we fit a new model
spend.m5 <- lm(online.spend ~ email + age + credit.score + distance.to.store +
                 sat.service + sat.selection + online + store, data = cust.df.bc)
summary(spend.m5)
# positive or negative coefficients in PCs mean nothing
vif(spend.m5)
# all VIFs below 5

#### Linear Models for Binary Outcomes: Logistic Regression ####
# we want to know if customers are more likely to purchase the season pass 
# when it is offered in the bundle (with free parking), or not?

# get the data
pass.df <- read.csv('http://goo.gl/J8MH6A')
pass.df$Promo <- factor(pass.df$Promo, levels = c('NoBundle', 'Bundle'))
pass.df$Pass <- factor(pass.df$Pass, levels = c('NoPass', 'YesPass'))
# positive and negative values of factors need to be in the same direction
summary(pass.df)

# fitting a logistic regression model
pass.m1 <- glm(Pass ~ Promo, data = pass.df, family = binomial)
# for the model to run, all character columns need to be changed into factors

summary(pass.m1)
# there is a positive coeff with statistical significance

# we calculate the association of pass sales to the promotion bundle factor by
# examinig the ratio of success (plogis(coeff)) to non-success ((1- plogis(coeff)))
# manual way
plogis(0.38879)/(1 - plogis(0.38879))
# this shows that the effect of Bundle is an estimated odds ratio of 1.475.
# customers are 1.475 times more likely to purchase the pass when it is offered
# in the bundle.
# another way to think about it is bundle increases the purchase likelyhood 
# by 47.5%

# other way to calculate the effect
exp(0.38879)

# or directly from the model
exp(coef(pass.m1))

# we find the confidence intervals
exp(confint(pass.m1))
# the odds ratio for the promotion bundle is estimated at 1.28 to 1.70, which
# is a significant effect
# the effect of increased likelyhood of event occurring because of explanatory 
# variable will never be more than 171% because the coeff of the model lays 
# between [0, 1] (exp(1) - 1 = 171% while exp(0) - 1 = 0%)

# reconsidering the model
# we create a mosaic plot to find hidden relationships
library(vcd)
doubledecker(table(pass.df))
# we see that park is the most effective channel to sell tickets regardless of
# the promo.
# the 3 channels have different effects, bundles do not sell tickets via mail
# but they do via email. no bundle sales (season passes) are very successful in
# the park but fail at email
# to find out more, we model by channel
pass.m2 <- glm(Pass ~ Promo + Channel, data = pass.df, family = binomial)
summary(pass.m2)
# given the new model, we find that the promo has a substantial negative effect
# on sales. 
# compute odds ratios and confidence intervals
exp(coef(pass.m2))
exp(confint(pass.m2))
# in this model, promotion is linked with 32% - 53% less chances of purchasing
# the season pass, while purchasing by mail or at the park increases chances of
# buying the season pass by 30-56x.

# let's see a model that includes the interaction of promo and channel
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, data = pass.df,
               family = binomial)
summary(pass.m3)
# here we can see that the interaction between promo and channel in mail and 
# park is strongly negative while the independent effect of promo is positive
# we can infer that bundles work good in emails, but not in mail or park
exp(confint(pass.m3))
# here we can say that promobundle makes sales of season pass 5 to 14x more 
# likely via email, while only 2% to 11% in the other channels

#### Hierarchical Models ####
# get the data
conjoint.df <- read.csv('http://goo.gl/G8knGV')
conjoint.df$speed <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
conjoint.df$const <- factor(conjoint.df$const)
conjoint.df$theme <- factor(conjoint.df$theme)
# we convert explanatory variables to factors

summary(conjoint.df)

# to measure the mean rating of each height factor we can use by()
by(conjoint.df$rating, conjoint.df$height, mean)
# from this results, we can see that the most popular height is 300 with 7.25
# the least liked is 200 with 3.65 points

# we model the customers preferences with a linear model
ride.lm <- lm(rating ~ speed + height + const + theme, data = conjoint.df)
summary(ride.lm)
# we could now sum the and the intercept coefficients between the 4 explanatory 
# variables speed70 + height300 + intercept = 3.07 + 4.48 + 2.94 = 10.46
# this would lead us to a wrong conclusion, rating is capped at 10
# you would add too themeDragon and constSteel, as they are positive (their
# counterparts are negative) and the result would still be > 10.
# Lesson: you cannot simply add up the coeff and intercept to find the best mix
# of variables that would hit on the preferences of the public

# After we analyse the overall model, we need to go the hierarchical model to 
# find individual preferences and find out more about preferences and get a 
# clearer view
library(lme4)
ride.hlm1 <- lmer(rating ~ speed + height + const + theme + (1 | resp.id),
                  data = conjoint.df)
summary(ride.hlm1)

# to find fixed effects
fixef(ride.hlm1)

# to find first 6 random effects
head(ranef(ride.hlm1)$resp.id)

# to find the complete effect for each respondent
# remember, the fixed effects are the same for each observation, the random
# variable is the intercept
head(coef(ride.hlm1)$resp.id)

# Complete Hierarchical Models

# this new model can take a few mins to run as it takes in more parameters
ride.hlm2 <- lmer(rating ~ speed + height + const + theme + 
                    (speed + height +const + theme | resp.id), 
                  data = conjoint.df,
                  control = lmerControl(optCtrl = list(maxfun = 100000)))

# error message: boundary (singular) fit: see ?isSingular
isSingular(ride.hlm2, tol = 1e-4)
# isSingular error: Your model did fit, but it generated that warning because 
# your random effects are very small. You can read more about this in this post
# or the help page. (Overfitting)

# fixed effects
fixef(ride.hlm2)
# same results as previous models

# random effects now include an estimate FOR EACH parameter for each respondent
# we group by $resp.id to see all random effects
head(ranef(ride.hlm2)$resp.id)
# note that also intercept is unique to each respondent

# check coefs
head(coef(ride.hlm2)$resp.id)
# again, each respondent has its own coefficients

# final check, coef = fixef + ranef in a random respondent
fixef(ride.hlm2) + ranef(ride.hlm2)$resp.id[196, ]
coef(ride.hlm2)$resp.id[196, ]
# they are the same

#### Bayesian Hierarchical Lineal Models ####

# get the data
conjoint.df <- read.csv('http://goo.gl/G8knGV')
conjoint.df$speed <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
conjoint.df$const <- factor(conjoint.df$const)
conjoint.df$theme <- factor(conjoint.df$theme)
# we convert explanatory variables to factors

# we run a initial non=hierarchical model which will be the base estimation for
# further models
library(MCMCpack)
set.seed(97439)
ride.mc1 <- MCMCregress(rating ~ speed + height + const + theme, 
                        data = conjoint.df)
summary(ride.mc1)

# we add hierarchical component to the model
set.seed(97439)
ride.mc2 <- MCMChregress(fixed = rating ~ speed + height + const + theme,
                         random = ~ speed + height + const + theme,
                         group = 'resp.id', data = conjoint.df, r = 8, 
                         R = diag(8))
# args "r =" is equal to the number of parameters  and "R=" is equal 
# to diag(number of parameters)
summary(ride.mc2$mcmc[, 1:8])

# fixed effects in bayesian model are similar to normal hierarchical model
summary(ride.mc2$mcmc[, grepl(".196", colnames(ride.mc2$mcmc), fixed = TRUE)])

# we check response data from observation 196 as in the previous model
# we see that respondent 196 likes wood coasters by 2.57 points over the avg
# in our 10 point scale. Dislikes Eagle theme -1.41 below avg.
# random (individual) effects are read as above or under the avg of such variable

# Inspecting distribution of preference
# we get each draw's individual and population level estimates, and the add 
# them together (that's why we have a + sign in the summary() command)
ride.constWood <- summary(ride.mc2$mcmc[, grepl("b.constWood", 
                                                colnames(ride.mc2$mcmc))] +
                            ride.mc2$mcmc[, "beta.constWood"])

ride.constWood

# now we plot
hist(ride.constWood$statistics[, 1],
     main = "Preference for Wood vs. Steel",
     xlab = "Rating points", ylab = "Count of Respondents", xlim= c(-4, 4))

# now we make the same analysis on speeds (be careful, this only applies when 
# you compare a variable shown in the MCMC results vs the variable that is not
# shown in the MCMC result - because this is a linear regression one var doesn't show)
ride.speed60 <- summary(ride.mc2$mcmc[, grepl("b.speed60",
                                              colnames(ride.mc2$mcmc))] +
                          ride.mc2$mcmc[, "beta.speed60"])
hist(ride.speed60$statistics[, 1],
     main = "Preference 60mph vs 40 mph",
     xlab = "Rating points", ylab = "Count of Respondents", xlim= c(-4, 4))
# we can see that respondents overwhelmingly prefer a ride with a 60mph ride
# than a 40mph ride

# we want to know the variance of the population estimate of preference
# this data is also included in the MCMCregress() model
summary(ride.mc2$mcmc[, c("beta.constWood", "VCV.constWood.constWood",
                          "beta.speed60", "VCV.speed60.speed60")])
# we see that variance of constWood is quit wide (2.34) so preferences are mixed
# variance of speed60 is much tighter (0.57) which comes to show that the preference
# for 60 mph rides are much more pronounced (as seen in the histogram)

# 
#### A Quick Comparison of the Effects ####
# we fix and compare the population estimates between ride.hlm2 (fixef()) and 
# ride.mc2 (colMeans(model$mcmc[, 1:8]))
fix.hlm <- fixef(ride.hlm2)
fix.mc <- colMeans(ride.mc2$mcmc[, 1:8])
plot(fix.hlm, fix.mc)
abline(0, 1)
# if they are on the 45 degree line, the estimates are very close to each other

# random effects have to be compared within respondent
ranef(ride.hlm2)$resp.id[196, ]
colMeans(ride.mc2$mcmc[, grepl(".196", colnames(ride.mc2$mcmc), fixed = TRUE)])

# we plot random effects
par(mfrow = c(2, 2))
plot.xlim <- c(-3, 3)
for(i in 2:5){
  mcmc.col <- which(grepl(".196", colnames(ride.mc2$mcmc), fixed = TRUE))[i]
  plot(density(ride.mc2$mcmc[, mcmc.col]), xlab = "", ylim = c(0, 1.4),
       xlim = plot.xlim,
       main = paste("HB & lmer density:", colnames(ride.mc2$mcmc)[mcmc.col]))
  hlm2.est <- ranef(ride.hlm2)$resp.id[196, i]
  # take the variance from the lmer model
  hlm2.sd <- sqrt(attr(ranef(ride.hlm2, condVar = TRUE)$resp.id, 
                       "postVar")[, , 196][i, i])
  seq.pts <- seq(from = plot.xlim[1], to = plot.xlim[2], length.out = 1000)
  points(seq.pts, dnorm(seq.pts, mean = hlm2.est, sd = hlm2.sd), col = "red",
         pch = 20, cex = 0.05)
  legend("topright", legend = c("red = lmer", "black = HB"), 
         text.col = c("red", "black"))
}
# we find that central tendency parameters between both models largely coincide
# main difference is variance in bayesian model is significantly higher because 
# of posterior draws