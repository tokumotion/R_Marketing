install.packages(c('lavaan', 'semPlot', 'corrplot', 'multcomp'))

satData <- read.csv('http://goo.gl/UDv12g')
satData$Segment <- factor(satData$Segment)
head(satData)
summary(satData)

# This is an example of how R works

library(corrplot)
corrplot.mixed(cor(satData[,-3]))

# “A plot visualizing correlation between satisfaction and likelihood to 
# recommend variables in a simulated consumer data set, N = 500. All items 
# are positively correlated with one another, and the two satisfaction items 
# are especially strongly correlated with one another, as are the two 
# recommendation items. Chapter 4 discusses correlation analysis in detail.”

# Does product satisfaction differ by segment?

aggregate(iProdSAT ~ Segment, satData, mean)

# Are the differences between means statistically significant?

sat.anova <- aov(iProdSAT ~ -1 + Segment, satData)
summary(sat.anova)

# Plot the ANOVA

library(multcomp)
par(mar = c(4, 8, 4, 2))
plot(glht(sat.anova))

# Fit a structural equation model to the satisfaction data.

satModel <- 'SAT =~ iProdSAT + iSalesSAT 
             REC =~ iProdREC + iSalesREC 
             REC ~ SAT'

library(lavaan)
sat.fit <- cfa(satModel, data = satData)
summary(sat.fit, fit.m = TRUE)

# Graph it

library(semPlot)
semPaths(sat.fit, what = 'est', residuals = FALSE, intercepts = FALSE,
         nCharNodes = 9)

# “A structural model with path loadings for a model of product satisfaction 
# and likelihood-to-recommend, using the lavaan and semPlot packages. 
# Satisfaction has a strong relationship to likelihood-to-recommend 
# (coefficient = 0.76) in the simulated consumer data.”

# New dataset for practicing
store.num <- factor(c(3, 14, 21, 32, 54)) # Store ID
store.rev <- c(543, 654, 345, 678, 234) # store revenue, $1000
store.visits <- c(45, 78, 32, 56, 34) # visits, 1000s
store.manager <- c('Annie', 'Bert', 'Carla', 'Dave', 'Ella')
store.df <- data.frame(store.num, store.num, store.visits, store.manager, 
                       stringsAsFactors = F)

# How to create a function
# 4 conventions
# 1. Put braces {} along the body
# 2. Create temporary values to hold results along the way inside the function
# 3. Comment the function profusely
# 4. Use the keyword return() to show the explicit value returned by the 
# function
se <- function(x){
  # computes standard error of the mean
  tmp.sd <- sd(x)               # standard deviation
  tmp.N <- length(x)            # sample size
  tmp.se <- tmp.sd/sqrt(tmp.N)  # std error of the mean
  return(tmp.se)
  }
se(store.df$store.visits)

˜# Compute upper-bound 95% confidence interval as the mean + 1.96 se
mean(store.df$store.visits) + 1.96 * se(store.df$store.visits)

# 