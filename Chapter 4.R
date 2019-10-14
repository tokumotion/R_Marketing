# Relationships between continuous variables
# Retailer data
cust.df <- read.csv('http://goo.gl/PmPkaG')

# Simulate the retailer data
set.seed(21821)
ncust <- 1000
cust.df <- data.frame(cust.id = as.factor(c(1:ncust)))
cust.df$age <- rnorm(n = ncust, mean = 35, sd = 5)
cust.df$credit.score <- rnorm(n = ncust, 
                              mean = 3*cust.df$age + 620, sd = 50)
cust.df$email <- factor(sample(c('yes', 'no'), size = ncust, replace = TRUE, 
                               prob = c(0.8, 0.2)))
# Our final variable for the basic CRM data is distance.to.store, which we 
# assume follows the exponential of the normal distribution.
cust.df$distance.to.store <- exp(rnorm(n = ncust, mean = 2, sd = 1.2))
summary(cust.df)

# Simulate Online and In-Store Sales Data
# We simulate the number of visits with a negative binomial distribution, a 
# discrete distribution often used to model counts of events over time. Like 
# the lognormal distribution, the negative binomial distribution generates 
# positive values and has a long right-hand tail, meaning that in our data 
# most customers make relatively few visits and a few customers make many visits.

# We model the mean (mu) of the negative binomial with a baseline value of 15. 
# The size argument sets the degree of dispersion (variation) for the samples. 
# We add an average 15 online visits for customers who have an email on file, 
# using ifelse() to generate a vector of 0 or 15 as appropriate. Finally, we 
# add or subtract visits from the target mean based on the customer’s age 
# relative to the sample median; customers who are younger are simulated to 
# make more online visits.
cust.df$online.visits <- rnbinom(ncust, size = 0.3, 
                                 mu = 15 + ifelse(cust.df$email=='yes', 15, 0) 
                                 - 0.7 * (cust.df$age - median(cust.df$age)))

# We assume that for each online visit that a customer makes, we asssume there
# is a 30% chance of placing an order and use rbinom() to create the variable
# online.trans. We assume spent in those orders (the variable online.spend)
# are lognormally distributed.
cust.df$online.trans <- rbinom(ncust, size = cust.df$online.visits, prob = 0.3)
cust.df$online.spend <- exp(rnorm(ncust, mean = 3, sd = 0.1)) * 
                              cust.df$online.trans

# We assume that transactions follow a negative binomial distribution 
# (rnbinom()), with lower average numbers of visits for customers who live 
# farther away. We model in-store spending as a lognormally distributed 
# variable simply multiplied by the number of transactions
cust.df$store.trans <- rnbinom(ncust, size = 5, 
                               mu = 3/sqrt(cust.df$distance.to.store))
cust.df$store.spend <- exp(rnorm(ncust, mean = 3.5, sd = 0.4)) * 
  cust.df$store.trans
summary(cust.df)

# Simulating Satisfaction Survey Repsonses
sat.overall <- rnorm(ncust, mean = 3.1, sd = 0.7)
summary(sat.overall)

# To create such a score from a halo variable, we add sat.overall (the halo) to
# a random value specific to the item, drawn using rnorm(). Because survey 
# responses are typically given on a discrete, ordinal scale (i.e., “very 
# unsatisfied”, “unsatisfied”, etc.), we convert our continuous random values 
# to discrete integers using the floor() function.
sat.service <- floor(sat.overall + rnorm(ncust, mean = 0.5, sd = 0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean = -0.2, sd = 0.6))
summary(cbind(sat.service, sat.selection))

# The summary shows that our data now ranges from −1 to 6. However, a typical 
# satisfaction item might be given on a 5-point scale. To fit that, we replace
# values that are greater than 5 with 5, and values that are less than 1 with 1
sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1
summary(cbind(sat.service, sat.selection))

# Simulating Non-Response Data
# Modelling non-response data
no.response <- as.logical(rbinom(ncust, size = 1, prob = cust.df$age/100))
sat.service[no.response] <-  NA
sat.selection[no.response] <- NA
summary(cbind(sat.service, sat.selection))

cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection
summary(cust.df)
rm(ncust, sat.selection, sat.service, sat.overall, no.response)

# Exploring associations between variables with scatterplots
str(cust.df)
# Each row represents a different customer
plot(x = cust.df$age, y = cust.df$credit.score,
     col = "blue", xlim = c(15, 55), ylim = c(500, 900),
     main = 'Active Customers as of June 2014',
     xlab = 'Customer Age (years)', ylab = 'Customer Credit Score')
abline(h = mean(cust.df$credit.score), col = 'dark blue', lty = 'dotted')
abline(v = mean(cust.df$age), col = 'dark blue', lty = 'dotted')
abline(h = median(cust.df$credit.score), col = 'dark red', lty = 'dotted')
abline(v = median(cust.df$age), col = 'dark red', lty = 'dotted')
abline(h = getmode(cust.df$credit.score), col = 'dark green', lty = 'dotted')
abline(v = getmode(cust.df$age), col = 'dark green', lty = 'dotted')

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Do customers who buy online buy less in-store?
plot(cust.df$store.spend, cust.df$online.spend,
     main = 'Customers as of June 2014',
     xlab = 'Prior 12 months in-store sales ($)',
     ylab = 'Prior 12 months online sales ($)',
     cex = 0.7)
# The resulting plot is typical of the skewed distributions that are common in 
# behavioral data such as sales or transaction counts; most customers purchase 
# rarely so the data is dense near zero

hist(cust.df$store.spend, 
     breaks = (0:ceiling(max(cust.df$store.spend)/10))*10,
     main = 'Customers as of June 2014',
     xlab = 'Prior 12 months online sales ($)',
     ylab = 'Count of customers')
# The distribution of sales among those who do buy has a mode around $20 and a
# long right-hand tail with a few customers whose 12-month spending was high. 
# Such distributions are typical of spending and transaction counts in customer 
# data.

# Color-coding points in scatterplot
my.col <- c('black', 'green3')
my.pch <- c(1, 19)  # R's symbols for solid and open circles

head(cust.df$email)
as.numeric(head(cust.df$email))
my.col[as.numeric(head(cust.df$email))]   # Color coding the data
plot(cust.df$store.spend, cust.df$online.spend,
     cex = 0.7, col = my.col[cust.df$email], pch = my.pch[cust.df$email],
     main = 'Customers as of June 2014',
     xlab = 'Prior 12 months in-store sales ($)',
     ylab = 'Prior 12 months online sales ($')

# Adding a legend to a plot
legend(x = 'topright', legend = paste('email on file:', levels(cust.df$email)),
       col = my.col, pch = my.pch)

# Plotting on a log scale
plot(cust.df$store.spend + 1, cust.df$online.spend + 1, log = 'xy', cex = 0.7,
     col = my.col[cust.df$email], pch = my.pch[cust.df$email],
     main = 'Customers as of June 2014',
     xlab = 'Prior 12 months in-store sales ($)',
     ylab = 'Prior 12 months online sales ($')
legend(x = 'topright', legend = paste('email on file:', levels(cust.df$email)),
       col = my.col, pch = my.pch)
# In this code, spend + 1 to avoid an error due to the fact that log(0) is not
# defined.
# It now appears that there is little or no association between online and 
# in-store sales; the scatterplot among customers who purchase in both channels
# shows no pattern. Thus, there is no evidence here to suggest that online 
# sales have cannibalized in-store sales (a formal test of that would be 
# complex, but the present data do not argue for such an effect in any obvious
# way). We also see that customers with no email address on file show slightly
# lower online sales than those with addresses; there are somewhat more black 
# circles in the lower half of the plot than the upper half. If we have been 
# sending email promotions to customers, then this suggests that the promotions 
# might be working. An experiment to confirm that hypothesis could be an 
# appropriate next step.

# Combining plots in a single graphics object
par(mfrow = c(2, 2))  # Graphics show a matrix of 2x2
plot(cust.df$distance.to.store, cust.df$store.spend, main = 'store')
plot(cust.df$distance.to.store, cust.df$online.spend, main = 'online')
plot(cust.df$distance.to.store, cust.df$store.spend, log = 'xy', 
     main = 'store, log')
plot(cust.df$distance.to.store +1, cust.df$online.spend + 1, log = 'xy',
     main = 'online, log')
par(mfrow = c(1, 1))

# Scatterplot matrices
# pairs()
pairs(formula = ~ age + credit.score + email + distance.to.store + 
      online.visits + online.trans + online.spend + store.trans + store.spend,
      data = cust.df)
# We can see relationships between variables quickly in a scatterplot matrix. 
# In the fifth row and sixth column we see a strong linear association between 
# online.visits and online.trans; customers who visit the website more 
# frequently make more online transactions. Looking quickly over the plot, we 
# also see that customers with a higher number of online transactions have 
# higher total online spending (not a surprise), and similarly, customers with
# more in-store transactions also spend more in-store. This simple command 
# produced a lot of information to consider.
pairs(cust.df[, c(2: 10)])

# scatterplotMatrix()
library(car)
scatterplotMatrix(formula = ~ age + credit.score + email + distance.to.store +
                    online.visits + online.trans + online.spend + store.trans +
                    store.spend, data = cust.df, diagonal = 'histogram',
                  legend = TRUE)
?scatterplotMatrix
library(gpairs)
gpairs(cust.df[, c(2:10)])

# Correlation coefficients
# We often use Cohen’s Rules of Thumb, which come out of the psychology 
# tradition. Cohen proposed that for correlations between variables describing 
# people, r=0.1 should be considered a small or weak association, r=0.3 might
# be considered to be medium in strength, and r=0.5 or higher could be 
# considered to be large or strong. Cohen’s interpretation of a large effect 
# was that such an association would be easily noticed by casual observers. A 
# small effect would require careful measurement to detect yet might be 
# important to our understanding and to statistical models.

# Importantly, interpretation of r according to Cohen’s rules of thumb depends 
# on the assumption that the variables are normally distributed (also known as
# Gaussian) or are approximately so. If the variables are not normal, but 
# instead follow a logarithmic or other distribution that is skewed or strongly
# non-normal in shape, then these thresholds do not apply. In those cases, it 
# can be helpful to transform your variables to normal distributions before 
# interpreting

# Correlation tests
cor.test(cust.df$age, cust.df$credit.score)

# Correlation matrices
cor(cust.df[,c(2, 3, 5:12)])
cor(cust.df[, c(2, 3, 5:12)], use = 'complete.obs')

library(corrplot)
library(gplots)
col1 <- colorRampPalette(c('red', 'grey60', 'blue4'))
corrplot.mixed(corr = cor(cust.df[, c(2, 3, 5:12)], use = 'complete.obs'),
               upper = 'ellipse', tl.pos = 'lt', upper.col = col1(50),
               lower.col = col1(50), number.cex = .7, tl.col = 'black', 
               tl.cex = .7)
# Numeric values of r are shown in the lower triangle of the matrix. The upper 
# triangle displays ellipses (because we used the argument upper="ellipse"). 
# These ellipses are tighter, progressively closer to being lines, for larger
# values of r, and are rounder, more like circles for r near zero. They are 
# also shaded blue for positive direction, and red for negative (and show
# corresponding positive or negative slope).

# Tranforming variables before computing correlations
# It is important to consider transforming variables to approximate normality 
# before computing correlations or creating scatterplots; the appropriate 
# transformation may help you to see associations more clearly.

# Unit sales, revenue, household income, price = log(x). Units sold are often
# related to log(price)
# Distance = 1/x, 1/x^2, log(x). Store spend is often associated with 
# 1/distance-to-store^2
# Market or preference share based on a uitility value = e^x/(1+e^x)
# Right-tailed distributions (NBD) = x^1/2 or log(x) (watch out for log(x <= 0))
# Left-tailed distribution (BD) = x^2

# When the above transformations do not work, use Box-Cox transformation
powerTransform(cust.df$distance.to.store)   
# Value of lambda to make distance as similar as possible to normal 
# distribution.
# If you attempt to transform a variable that is already close to normally 
# distributed, powerTransform() will report a value of lambda that is 
# close to 1.

# Extract value of lambda
lambda <- coef(powerTransform(cust.df$distance.to.store))
# Create transformed variable
bcPower(cust.df$distance.to.store, lambda = lambda)

# See how this compares transformed and untransformed data
par(mfrow = c(1, 2))
hist(cust.df$distance.to.store,
     xlab = 'Distance to Nearest Store', ylab = 'Count of Customers')
hist(bcPower(cust.df$distance.to.store, lambda = lambda),
     xlab = 'Box-Cox Transform of Distance', ylab = 'Count of Customers',
     main = 'Transformed Distribution')
par(mfrow = c(1, 1))

# See how the transformed distribution correlates with in-store spend
# Finally, we can compute correlations for the transformed variable. These 
# correlations will often be larger in magnitude than correlations among raw, 
# untransformed data points. We check r between distance and in-store spending, 
# transforming both of them first:
l.dist <- coef(powerTransform(cust.df$distance.to.store))
l.spend <- coef(powerTransform(cust.df$store.spend + 1))
cor(bcPower(cust.df$distance.to.store, lambda = l.dist),
    bcPower(cust.df$store.spend + 1, lambda = l.spend))

# Exploring associations in survey responses
par(mfrow = c(1, 2))
plot(cust.df$sat.service, cust.df$sat.selection,
     xlab = 'Customer Satisfaction with Service',
     ylab = 'Customer Satisfaction with Selection',
     main = 'Customers as of June 2014')
plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection),
     xlab = 'Customer Satisfaction with Service',
     ylab = 'Customer Satisfaction with Selection',
     main = 'Customers as of June 2014')
par(mfrow = c(1, 1))

# Polychloric correlation coefficient is designed for ordinal (Likert) 
# responses
# The concept of a polychoric correlation is that respondents have continuous 
# values in mind when they answer on a rating scale. However, because the 
# scales are limited to a small number of points, respondents must select 
# discrete values and choose points on the scale that are closest to the
# unobserved latent continuous values. The polychoric estimate attempts to 
# recover the correlations between the hypothetical latent (unobserved) 
# continuous variables.
resp <- !is.na(cust.df$sat.service)
cor(cust.df$sat.service[resp], cust.df$sat.selection[resp])
library(psych)
polychoric(cbind(cust.df$sat.service[resp], cust.df$sat.selection[resp]))
