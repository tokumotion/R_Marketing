library(MCMCpack); library(tidyverse); library(lme4); library(car); library(vcd)

sales.data.raw <- read.csv('https://goo.gl/4Akgkt', stringsAsFactors = TRUE)
summary(sales.data.raw)

# acctAge = Tenure of customer in months
# spendToDate = Customer's total lifetime spending
# satSite = Rating for satisfaction with prices
# region = US geographic region
# purchase = Whether they purchased the promoted product (with or without coupon)
# visitsMonth = Visits to the website in the most recent month
# spendMonth = Spending, most recent month
# satQuality = Rating for satisfaction with product quality
# satOverall = Overall satisfaction rating
# coupon = Whether coupon was sent to them for a particular promoted product

# 1. In the sales data, predict the recent month's spending (spendMonth) on the
# basis of the other variables using a linear model. Are there any concerns with
# the model? If so, fix them and try the prediction again.
spend.m1 <- lm(spendMonth ~ ., data = sales.data.raw)
summary(spend.m1)
scatterplotMatrix(sales.data.raw)

vif(spend.m1)
spend.m2 <- lm(spendMonth ~ . - satSite - satPrice, data = sales.data.raw) 
vif(spend.m2)
summary(spend.m2)
# Collinearity in satSite and satPrice, we take those variables off the model
# all VIF are below 5

# 2. How does the prediction of the recent month's sales change when the 
# variables are optimally transformed? Which model -- transformed or not --
# is more interpretable?
autoTransform <- function (x){
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

sales.data <- sales.data.raw
sales.data[, -9] <- lapply(sales.data[, -9], autoTransform) 
# why transform binary data? if you run a transform you have to run it to the 
# entire dataset
spend.m3 <- lm(spendMonth ~ . - satSite - satPrice, data = sales.data) 
vif(spend.m3)
summary(spend.m3)
# 3. Fit the linear model again, using a principal component extraction for 
# satisfaction. What is the primary difference in the estimates?
PCAspend <- prcomp(sales.data[, 5:8])
summary(PCAspend)
# PCA1 explains most of the variance and same direction
sales.data$PCA <- PCAspend$x[, 1]
# now we take away troublesome variables and put PCA into the model
spend.m4 <- lm(spendMonth ~ ., data = sales.data[, -c(5:8)])
summary(spend.m4)
vif(spend.m4)

# 4. [thought exercise without code]. When the model is fit with region as a
# predictor, it may show the West region with a large, possibly even the
# largest, effect. Yet it is not statistically significant whereas smaller
# effects are. Why could that be?
# Type of data.

# 5. Using logistic regression, what is the relationship between the coupon
# being sent to some customers and whether the purchased the promoted product?
# first we convert back purchase to binary
sales.data$purchase <- sales.data.raw$purchase
purchase.m1 <- glm(purchase ~ coupon, data = sales.data, family = binomial)
summary(purchase.m1)
# we see a positive correlation between coupon and purchase

# 6. How does that model change if region, satisfaction, and total spending are
# added to the model?
purchase.m2 <- glm(purchase ~ coupon + region + PCA + spendMonth, data = sales.data,
                   family = binomial)
summary(purchase.m2)
# we now see that principal component of satisfaction and monthly spend are 
# better predictors than coupons

# 7. Is there an interaction between the coupon and satisfaction, in their 
# relationship to purchase of the promoted product? 
# here we need to show the interaction between sat and coupon var
purchase.m3 <- glm(purchase ~ coupon * PCA, data = sales.data, family = binomial)
summary(purchase.m3)
# satisfaction remains negatively correlated with purchase and is the only
# statistically significant variable

# 8. What is the best estimate for how much a coupon is related to increased purchase, as
# an odds ratio?
plogis(purchase.m3$coefficients["coupon"])/(1 - plogis(purchase.m3$coefficients["coupon"]))
exp(purchase.m3$coefficients["coupon"])
# increased likelyhood of purchasing of 47.51%

# 9. What is the change in purchase likelihood, in relation to a change of 1 
# unit of satisfaction? (Hint: what is a unit of satisfaction?) Approximately 
# how many points would ``1 unit'' be, on the actual 1-10 rating scales?)

# 1 units is 1 sd
library(psych)
describe(sales.data.raw[, 5:8])

# 10. [thought exercise] For product strategy, what questions are suggested by 
# this relationship between satisfaction and purchase? What possible 
# explanations are there, or what else would you wish to know?
# why do increase in satisfaction decreases the chances of purchasing the product?
# is the product considered below par for our brand?

# 11. Using the handbag data, estimate the likelihood to purchase as a 
# function of the handbags' attributes, using a simple linear model
conjoint.df <- read.csv("https://goo.gl/gEKSQt", stringsAsFactors = TRUE)
summary(conjoint.df)

bags.m1 <- lm(rating ~ price + color + zipper + finish, data = conjoint.df)
summary(bags.m1)

# 12. Now fit the same model as a classical hierarchical model, with 
# individual level estimates for each attribute's utility.
# this model only has intercept as random effect
bags.hlm1 <- lmer(rating ~ price + color + zipper + finish + 
                    (price + color + zipper + finish|resp.id)
                  , data = conjoint.df,
                  control = lmerControl(optCtrl = list(maxfun = 100000)))
summary(bags.hlm1)
fixef(bags.hlm1)

# 13. What is the estimated rating for a black bag with matte finish and a
# gold zipper, priced at $15?
fixef(bags.hlm1)
sum(fixef(bags.hlm1)[c(1:6)])
# doesn't make sense, we don't see the estimates we want.

# 14. Which respondents are most and least interested in a navy handbag?
str(ranef(bags.hlm1))
# cutoffs for top and bottom 5%
quantile(x = ranef(bags.hlm1)$resp.id$colornavy, pr=0:20/20)
# pr = 0:20/20 means we get 5% cutoffs

which(ranef(bags.hlm1)$resp.id$colornavy > 1.13119476 | 
        ranef(bags.hlm1)$resp.id$colornavy < -1.12534598)
# this gives us the resp.ids for those who match the condition

# 15. Fit the hierarchical model again, using a Bayesian MCMC approach. How do
# the upper level estimates compare with those from the classical model?
set.seed(97439)
bags.hlm2 <- MCMChregress(fixed = rating ~ price + color + zipper + finish, 
                       random = ~ price + color + zipper + finish, 
                       group="resp.id", data=conjoint.df, r=6, R=diag(6))
# find where the coef are
str(bags.hlm2)
summary(bags.hlm2$mcmc[ ,1:6])
# Very similar estimates

# 16. In the MCMC results, what is the distribution of preference for a navy bag?
# estimates for Navy color
bag.navy <- summary(bags.hlm2$mcmc[, grepl("b.colornavy",
                                            colnames(bags.hlm2$mcmc))] + 
                       bags.hlm2$mcmc[, "beta.colornavy"])
bag.navy
hist(bag.navy$statistics[, 1],
     main = "Preference Navy Bag vs Black Bags",
     xlab = "Rating Points", ylab = "Count of Respondents", xlim = c(-10, 10))
# People strongly prefer Black Bags