# Simulating data
store.df <- read.csv('http://goo.gl/QPDdMl')

# Setting the structure
k.stores <- 20 # 20 stores, using 'k.' for constant
k.weeks <- 104 # 108 weeks. 2 years of data

# Create a df of empty values
store.df <- data.frame(matrix(NA, ncol = 10, nrow = k.stores*k.weeks))
names(store.df) <- c('storeNum', 'Year', 'Week', 'p1sales', 'p2sales', 
                     'p1price', 'p2price', 'p1prom', 'p2prom', 'country')
dim(store.df)
store.num <- 101:(100 + k.stores)
store.cty <- c(rep('US', 3), rep('DE', 5), rep('GB', 3), rep('BR', 2),
               rep('JP', 4), rep('AU', 1), rep('CN', 2))
length(store.cty)

# Fill storeNum and country columns
store.df$storeNum <- rep(store.num, each = k.weeks)
store.df$country <- rep(store.cty, each = k.weeks)
rm(store.num, store.cty)

# Fill Week and Year columns
store.df$Week <- rep(1:52, times = k.stores*2)
store.df$Year <- rep(rep(1:2, each = k.weeks/2), times = k.stores)

# Redefine storeNum and country as categorical variables
store.df$storeNum <- factor(store.df$storeNum)
store.df$country <- factor(store.df$country)

# Simulating data points
set.seed(98250)

# “We arbitrarily assign a 10% likelihood of promotion for product 1, and 15% 
# likelihood for product 2 and then randomly determine which weeks have promotions.”
store.df$p1prom <- rbinom(n = nrow(store.df), size = 1, p = 0.1) # 10% promoted
store.df$p2prom <- rbinom(n = nrow(store.df), size = 1, p = 0.15) # 15% promoted

# Set a price 
store.df$p1price <- sample(x = c(2.19, 2.29, 2.49, 2.79, 2.99), 
                           size = nrow(store.df), replace = TRUE)
store.df$p2price <- sample(x = c(2.29, 2.49, 2.59, 2.99, 3.19), 
                           size = nrow(store.df), replace = TRUE)

# Simulate weekly sales figures. We calculate sales as a function of the 
# relative prices of the two products along with the promotional status of each
# Item sales are in unit counts, so we use Poisson distribution to generate 
# count data rpois(n, lambda). n = # of draws + lambda = mean value of 
# units of week. 
tmp.sales1 = rpois(nrow(store.df), lambda = 120)
tmp.sales2 = rpois(nrow(store.df), lambda = 100)

# Scale sales according to the ratio of log(price)
# We have assumed that sales vary as the inverse ratio of prices. That is, 
# sales of Product 1 go up to the degree that the log(price) of Product 1 is 
# lower than the log(price) of Product 2.”
tmp.sales1 = tmp.sales1 * log(store.df$p2price)/log(store.df$p1price)
tmp.sales2 = tmp.sales2 * log(store.df$p1price)/log(store.df$p2price)

# final sales get a 30% or 40% lift when promoted
store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom * 0.3))
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom * 0.4))
rm(tmp.sales1, tmp.sales2, k.stores, k.weeks)

# Show a random sample of the data.frame
library(car)
some(store.df, 10)

# Functions to summarize a variable
p1.table <- table(store.df$p1price)
plot(p1.table)

# How many times each product has been promoted
table(store.df$p1price, store.df$p1prom)

# What is the % of times products have been promoted?
p1.table2 <- table(store.df$p1price, store.df$p1prom)
p1.table2[, 2]/(p1.table2[, 1] + p1.table2[, 2])

# Continuous Variables
min(store.df$p1sales)   # Minimum ammount of sales
max(store.df$p1sales)   # Maximum ammount of sales
mean(store.df$p1prom)   # Mean of promotions
mean(store.df$p1sales)  # Mean of sales
var(store.df$p1sales)   # Variance around the mean
sd(store.df$p1sales)    # Standard Deviation
IQR(store.df$p1sales)   # Interquantile range, 75th-25th percentile
mad(store.df$p1sales)   # Median absolute deviation
quantile(store.df$p1sales, probs = c(0.25, 0.5, 0.75))  # Percentiles

# For asymmetric and skewed distributions common in marketing sd() and mean()
# may be misleading (no normality). median() and IQR() are more useful to 
# summarize a distribution
# Find the other quantiles
quantile(store.df$p1sales, probs = 0:10/10)         # Find all other quantiles
quantile(store.df$p1sales, probs = c(0.05, 0.95))   # Central 90% of data

# “Suppose we wanted a summary of the sales for product 1 and product 2 based 
# on their median and interquartile range. We might assemble these summary 
# statistics into a data frame that is easer to read than the 
# one-line-at-a-time output above.”
mysummary.df <- data.frame(matrix(NA, nrow = 2, ncol = 2))
names(mysummary.df) <- c('Median Sales', 'IQR')
rownames(mysummary.df) <- c('Product 1', 'Product 2')
mysummary.df['Product 1', 'Median Sales'] <- median(store.df$p1sales)
mysummary.df['Product 2', 'Median Sales'] <- median(store.df$p2sales)
mysummary.df['Product 1', 'IQR'] <- IQR(store.df$p1sales)
mysummary.df['Product 2', 'IQR'] <- IQR(store.df$p2sales)

# Median Sales of Product 1 is higher as well the variation in sales volume
# While Product 2 has more steady but lower sales

# Summarizing Data Frames
library(psych)
describe(store.df)
# “By comparing the trimmed mean to the overall mean, one might discover 
# when outliers are skewing the mean with extreme values.”

# “describe() is especially recommended for summarizing survey data with 
# discrete values such as 1–7 Likert scale items from surveys (items that use
# a scale with ordered values such as “Strongly disagree (1)” to “Strongly 
# agree (7)” or similar).”

# There are * next to storeNum and country because they are factors

# We don't want to use the 1st, 3rd and 10th row
describe(store.df[, c(2, 4:9)])

# Use apply() to summarize rows
apply(X = store.df[,2:9], MARGIN = 2, FUN = mean)   
# MARGIN = 1 is rows, MARGIN = 2 is columns. To use both MARGIN = c(1, 2)
apply(store.df[, 2:9], 2, function(x) {mean(x) - median(x)})
# “This analysis shows that the mean of p1sales and the mean of p2sales are 
# larger than the median by about four sales per week, which suggests there is
# a right-hand tail to the distribution. That is, there are some weeks with 
# very high sales that pull the mean up. ”

# Single Variable Visualization
hist(x = store.df$p1sales, breaks = 30,
     main = 'Product 1 Weekly Sales Frequencies, All Stores',
     xlab = 'Product 1 Sales (Units)', ylab = 'Count', col = 'lightblue',
     freq = FALSE, xaxt = 'n')
axis(side = 1, at = seq(60, 300, by = 20))
lines(density(store.df$p1sales, bw = 10), type = 'l', col = 'darkred', lwd = 2)

hist(x = store.df$p2sales, breaks = 30,
     main = 'Product 2 Weekly Sales Frequencies, All Stores',
     xlab = 'Product 2 Sales (Units)', ylab = 'Count', col = 'lightblue',
     freq = FALSE, xaxt = 'n')
axis(side = 1, at = seq(40, 240, by = 15))
lines(density(store.df$p2sales, bw = 10), type = 'l', col = 'darkred', lwd = 2)

# Boxplots

# How do different stores compare on sales of product 2?
boxplot(store.df$p2sales ~ store.df$storeNum, 
        ylab = 'Store', xlab = 'Weekly unit sales', las = 1,
        main = 'Weekly Sales of P2 by Stores', horizontal = TRUE)

# How does promotion varies sales
boxplot(p2sales ~ p2prom, data = store.df, horizontal = TRUE, yaxt = 'n',
        ylab = 'P2 promoted in store?', xlab = 'Weekly sales',
        main = 'Weekly sales of P2 with and without promotion')
axis(side = 2, at = c(1, 2), labels = c('No', "Yes"))

# QQ plot to check for normality
# A QQ plot can confirm that the distribution is, in fact, normal by plotting 
# the observed quantiles of your data against the quantiles that would be 
# expected for a normal distribution.
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)  
# Data is not normally distributed. Upward curving shape is typical data with 
# high positive skew

# A QQ plot can confirm that the distribution is, in fact, normal by plotting 
# the observed quantiles of your data against the quantiles that would be 
# expected for a normal distribution.
qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

# Cummulative distribution
plot(ecdf(store.df$p1sales),
     main = 'Cummulative distribution of P1 Weekly Sales',
     ylab = 'Cummulative Proportion',
     xlab = c('P1 weekly sales, All stores', '90% of weeks sold <= 174 units'),
     yaxt = 'n')
axis(side = 2, at = seq(0, 100, by = 0.1), las = 1)
abline(h = 0.9, lty = 3)
abline(v = quantile(store.df$p1sales, pr = 0.9), lty = 3)
# The chart identifies that 90% of weekly sales are lower than or equal to 171 
# units. Other values are easy to read off the chart.
# For instance, roughly 10% of weeks sell less than 100 units, and fewer than 
# 5% sell more than 200 units.

# Break down the data by factors (by storeNum)
by(store.df$p1sales, store.df$storeNum, mean)

# Break down the data by factors (by storeNum and Year)
by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

# Break down the data by factors (by country)
p1sales.sum <- aggregate(store.df$p1sales, 
                         by = list(country = store.df$country), sum)

# How to map this
library(rworldmap)
library(RColorBrewer)
# Associate the aggregated data with specific map regions
p1sales.map <- joinCountryData2Map(p1sales.sum, joinCode = 'ISO2', 
                                   nameJoinColumn = 'country')
# Draw map  using mapcountrydata()
mapCountryData(p1sales.map, nameColumnToPlot = 'x', 
               mapTitle = 'Total P1 sales by country', 
               colourPalette = brewer.pal(7, 'Greens'),
               catMethod = 'fixedWidth', addLegend = FALSE)

# Describe discrete (categorical) data with table() and inspect continuous data
# with describe() from the psych package
