# Comparing Groups: Tables and Visualizations
# Simulating consumer data creating lookup tables
segVars <- c('age', 'gender', 'income', 'kids', 'ownHome', 'subscribe')
segVarType <- c('norm', 'binom', 'norm', 'pois', 'binom', 'binom')
segNames <- c('Suburb mix', 'Urban hip', 'Travelers', 'Moving up')
segSize <- c(100, 50, 80, 70)

# There are four segments and six demographic variables, so we create a 4 × 6
# matrix to hold the mean of each. The first row holds the mean values of each 
# of the six variables for the first segment; the second row holds the mean 
# values for the second segment, and so forth.
segMeans <- matrix(c(40, 0.5, 55000, 2, 0.5, 0.1,
                     24, 0.7, 21000, 1, 0.2, 0.2,
                     58, 0.5, 64000, 0, 0.7, 0.05,
                     36, 0.3, 52000, 2, 0.3, 0.2),
                   ncol = length(segVars), byrow = TRUE)

# For normal variables—in this case, age and income, the first and third 
# variables—we additionally need to specify the variance of the distribution, 
# the degree of dispersion around the mean. So we create a second 4 × 6 
# matrix that defines the standard deviation for the variables that require it:
segSDs <- matrix(c(5, NA, 12000, NA, NA, NA,
                   2, NA, 5000, NA, NA, NA,
                   8, NA, 21000, NA, NA, NA,
                   4, NA, 10000, NA, NA, NA),
                   ncol = length(segVars), byrow = TRUE)

# Populate data.frame
seg.df <- NULL
set.seed(02554)

# Iterate over segments and create data for each
for(i in seq_along(segNames)) {
  cat(i, segNames[i], "\n")
  
  # Empty matrix to hold this particular segment's data
  this.seg <- data.frame(matrix(NA, nrow = segSize[i], ncol = length(segVars)))
  
  # Within segment, iterate over variables and draw appropiate random data
  for(j in seq_along(segVars)) {
    if(segVarType[j] == 'norm') {
      this.seg[, j] <- rnorm(segSize[i], mean = segMeans[i, j], sd = segSDs[i, j])
    } else if(segVarType[j] == 'pois') {
      this.seg[, j] <- rpois(segSize[i], lambda = segMeans[i, j])
    } else if(segVarType[j] == 'binom') {
      this.seg[, j] <- rbinom(segSize[i], size = 1, prob = segMeans[i, j])
    } else {
      stop("Bad segment data type: ", segVarType[j])
    }
  }
  # add this segment to the total dataset
  seg.df <- rbind(seg.df, this.seg)
}

# Finishing data set
names(seg.df) <- segVars
# Add segment membership for each row
seg.df$segment <- factor(rep(segNames, times = segSize))
seg.df$gender <- factor(seg.df$gender, labels = c('Female', 'Male'))
seg.df$ownHome <- factor(seg.df$ownHome, labels = c('onwNo', 'ownYes'))
seg.df$subscribe <- factor(seg.df$subscribe, labels = c('subNo', 'subYes'))

summary(seg.df)
save(seg.df, file='~/Documents/R for Marketing/segdf-Rintro-Ch5.RData')

# Finding descriptives by group
by(data = seg.df$income, INDICES = seg.df$segment, FUN = mean)
by(data = seg.df$income, INDICES = list(seg.df$segment, seg.df$subscribe), 
   FUN = mean)
aggregate(seg.df$income, list(seg.df$segment, seg.df$subscribe), mean)

# Pivot table without using a function. Only sum.
with(seg.df, table(segment, ownHome))
xtabs(kids ~ segment, data = seg.df)

# This is similar
aggregate(kids ~ segment, data = seg.df, sum)

seg.tab <- with(seg.df, table(kids, segment))
apply(seg.tab*0:7, 2, sum)
colSums(seg.tab*0:7)

# Visualization by group: Frequencies and proportions
library(lattice)
histogram(~subscribe | segment, data = seg.df)
histogram(~subscribe | segment, data = seg.df, type = 'count', 
          layout = c(4,1), col = c('burlywood', 'darkolivegreen'))
histogram(~subscribe | segment + ownHome, data = seg.df)

# Proportions table
prop.table(table(seg.df$subscribe, seg.df$segment), margin = 2)
barchart(prop.table(table(seg.df$subscribe, seg.df$segment), margin = 2)[2, ],
         xlab = 'Subscriber proportion by Segment', col = 'darkolivegreen',
         xlim = c(0, 0.25))

# Visualization by Group: Continuous Data
seg.mean <- aggregate(income ~ segment, data = seg.df, mean)
barchart(income ~ segment, data = seg.mean, col = 'grey')

seg.mean.agg <- aggregate(income ~ segment + ownHome, data = seg.df, mean)
barchart(income ~ segment, data = seg.mean.agg, groups = ownHome, 
         auto.key = TRUE, par.settings = simpleTheme(col = terrain.colors(2)),
         xlab = 'Subscriber proportion by Segment and Home Ownership')

# Using boxplots to find out the real spread of incomes within segments
boxplot(income ~ ownHome + segment, data = seg.df, yaxt = 'n', ylab = 'Income ($k)')
ax.seq <- seq(from = 0, to = 120000, by = 20000)
axis(side = 2, at = ax.seq, labels = paste(ax.seq/1000, 'k', sep = ''), las = 1)

# Using lattice
bwplot(segment ~ income, data = seg.df, horizontal = TRUE, xlab = 'Income')
bwplot(segment ~ income | ownHome, data = seg.df, horizontal = TRUE, 
       xlab = 'Income')
