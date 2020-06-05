library(car); library(tidyverse); library(forecast); library(patchwork)
library(GGally); library(magrittr)

# Handling Highly Correlated Variables
cust.df <- read.csv('http://goo.gl/PmPkaG')
spend.m1 <- lm(formula = online.spend ~ ., 
               data = subset(cust.df[, -1], online.spend > 0))
summary(spend.m1)
# First, online spend is closely related to the number of online transactions 
# (coefficient=20.6) but not to the number of online visits. That is puzzling.
# Second, the model accounts for almost all the available variance, R2=0.98. 
# These results should cause concern. Because online transactions are dependent
# on visits, shouldn’t those two variables show a similar pattern? How could we
# be so lucky as to fit a model that nearly perfectly predicts online spending 
# (insofar as it is assessed by R 2)? And notice that the standard error on 
# store.trans is quite large, showing that its estimate is very uncertain.

ggpairs(spend.m1)
# we see data is not normally distributed, write a function (Box-Cox 
# transformation) 

autoTransform <- function(x){
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

# we drop var email (character) and run the function
cust.df.bc <- cust.df[complete.cases(cust.df), -1] %>% 
  filter(online.spend > 0) %>% 
  select(-email) %>% 
  sapply(.,autoTransform) %>% 
  data.frame() %>% 
  add_column(email = cust.df[which(cust.df$online.spend > 0 & 
                                     complete.cases(cust.df)), -1]$email)

ggpairs(cust.df.bc)

spend.m2 <- lm(online.spend ~ ., data = cust.df.bc)
summary(spend.m2)

# still having a huge estimate of online.trans (possible multicollinearity)
# compare models online.spend ~ . vs online.spend ~ online.trans
spend.m3 <- lm(online.spend ~ online.trans, data = cust.df.bc)
anova(spend.m2, spend.m3)  # compare models
# Differene between models is not significant (p = 0.8129 > p = 0.05)

# Dealing with collinearity
vif(spend.m2)  # vif > 5, collinearity
# There are three general strategies for mitigating collinearity:
# 1. Omit variables that are highly correlated.
# 2. Eliminate correlation by extracting principal components or factors for 
# sets of highly correlated predictors (see Chap.8).
# 3. Use a method that is robust to collinearity, i.e., something other than 
# traditional linear modeling. There are too many options to consider this 
# possibility exhaustively, but one method to consider would be a random forest 
# approach, which only uses a subset of variables at a time (see Sect.11.4.2).

# we find 2 pairs of highly correlated variables, we remove one variable of 
# each pair
spend.m4 <- lm(online.spend ~ . -store.trans -online.trans, data = cust.df.bc)
vif(spend.m4)
summary(spend.m4)

# we try principal component analysis
# Principal components are uncorrelated (orthogonal). Thus, PCA provides a way
# to extract composite variables that are guaranteed to be free of collinearity 
# with other variables that are included in the same PCA
pc.online <- prcomp(cust.df.bc[, c('online.visits', 'online.trans')])
cust.df.bc$online <- pc.online$x[, 1]
pc.store <- prcomp(cust.df.bc[, c('store.trans', 'store.spend')])
cust.df.bc$store <- pc.store$x[, 1]
# pc.online and pc.store are indicators of activity in store and online 
# respectively

spend.m5 <- lm(online.spend ~ email + age + credit.score + distance.to.store +
                 sat.service + sat.selection + online + store, data = cust.df.bc)
summary(spend.m5)
# directionality of PCA var does not hold any predictive power. 
vif(spend.m5)
# All VIF < 5 so no problem here