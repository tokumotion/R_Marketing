library(tidyverse); library(data.table);library(vcdExtra)

pass.df <- read.csv('https://goo.gl/J8MH6A')
# convertir variable Promo en factor
pass.df$Promo <- factor(pass.df$Promo, levels = c('NoBundle', "Bundle"))
summary(pass.df)
pass.df %>% 
  group_by(Pass, Channel, Promo) %>% 
  dplyr::summarise(count = n()) %>% 
  spread(Promo, count)

pass.tab <- c(359, 284, 27, 242, 639, 38, 278, 49, 485, 449, 223, 83)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- 'table'
dimnames(pass.tab) <- list(Channel = c('Mail', ' Park', 'Email'),
                          Promo = c('NoBundle', 'Bundle'),
                          Pass = c('YesPass', 'NoPass'))
pass.df <- expand.dft(pass.tab)
# set the order of the Bundle var values
pass.df$Promo <- factor(pass.df$Promo, levels = c('NoBundle', 'Bundle'))
table(pass.df$Pass, pass.df$Promo)

# run the model
summary(glm(Pass ~ Promo, family = binomial, data = pass.df))
# positive coefficient for bundle and significant

# calculate the odds ratio of bundle
exp(coef(glm(Pass ~ Promo, family = binomial, data = pass.df)))
# the bundle increses the likelyhood of purchase by 47.5%

# calculate confidence intervals of odds ratio
exp(confint(glm(Pass ~ Promo, family = binomial, data = pass.df)))

# how do we know in which channel the promo is effective?
summary(glm(Pass ~ Promo + Channel + Promo:Channel, 
            family = binomial, data = pass.df))
# we add the interacction between promo and channel

# we see from the results that the promo is effective in the Email channel
exp(coef(glm(Pass ~ Promo + Channel + Promo:Channel, 
                family = binomial, data = pass.df)))
# always substract 1 to the coef to see the odds, even if negative
exp(confint(glm(Pass ~ Promo + Channel + Promo:Channel, 
        family = binomial, data = pass.df)))