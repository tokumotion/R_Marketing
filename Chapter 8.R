### Data loading ####

brand.ratings <- read.csv('http://goo.gl/IQl8nc')
head(brand.ratings)

summary(brand.ratings)
# vars fom 1 to 9 are in the same range (1 to 10)

#### Principal Components Analysis ####

# rescale by getting the z-scoring of the data
brand.sc <- brand.ratings
brand.sc[, 1:9] <- data.frame(scale(brand.ratings[, 1:9]))
summary(brand.sc)

# use corrplot to find bi-variate relationships
library(corrplot)
corrplot(cor(brand.sc[, 1:9]), order = "hclust")

# aggregate mean ratings by brand
brand.mean <- aggregate(. ~ brand, data = brand.sc, FUN = mean)
rownames(brand.mean) <- brand.mean[, 1]
brand.mean <- brand.mean[, -1]

# we use a heatmap to visualize each brand's position
library(gplots); library(RColorBrewer)
heatmap.2(as.matrix(brand.mean),
          col = brewer.pal(9, "GnBu"), trace = "none", key = FALSE, dend = "none",
                           main = "\n\n\nBrand Attributes")
# green is low values, blue is higher values

# PCA example
# This example is to develop intuition into what PCA is
set.seed(98286)
xvar <- sample(1:10, 100, replace = T)
yvar <- xvar
yvar[sample(1:length(yvar), 50)] <- sample(1:10, 50, replace = T)
zvar <- yvar
zvar[sample(1:length(zvar), 50)] <- sample(1:10, 50, replace = T)
my.vars <- cbind(xvar, yvar, zvar)

# visualize data using arg data = jitter() because a lot of points will be repeated
plot(yvar ~ xvar, data = jitter(my.vars))
# clear linear trend (high positive cor)
cor(my.vars)

# in this data, there is shared variance across all vars. it is expected to 
# pick one var that explains the variance of all the associated remaining vars
my.pca <- prcomp(my.vars)
summary(my.pca)
# we find that 1st component (not var) accounts for 58% of variance

# to understand this we use a rotation matrix
my.pca
my.pca$x

# PCA components are uncorrelated
cor(my.pca$x)

# visualize results
biplot(my.pca)

# aaaaand we go back to brand ratings
brand.pc <- prcomp(brand.sc[, 1:9])
# scree plot which shows the successive proportion of additional variance that
# each components adds
plot(brand.pc, type = "l")
# visualize results
biplot(brand.pc)

# plot is too crowded and doesn't show brands. we use brand.mean data to fix that
brand.mu.scale <- prcomp(brand.mean, scale. = TRUE)
summary(brand.mu.scale)
# the first 2 PCs explain 84.07% of variance

# perceptual map of brands
biplot(brand.mu.scale, main = "Brand positioning", cex = c(1.5, 1))

# managerial implications (we are brand 'e')
# we want to invade 'C' territory
brand.mean['c',] - brand.mean['e',]
# brand c outperforms us in serious and perform, while we outperform them in 
# fun and value. So to move into 'c' territory, we need to dial down fun and
# value, while tuning up serious and perform

# we want out own territory
colMeans(brand.mean[c('b', 'c', 'g', 'f'),]) - brand.mean['e',]
# we want the space between gf and cb clusters, so we aggregate the standarized
# values of all 4 brands and find the differences between them and us.
# we find that we outperform them in fun and latest, we'll dial down these brand
# attributes, while we need to improve the perform attribute

#### Exploratory Factor Analysis ####
library(nFactors)
nScree(brand.sc[, 1:9])
# we use nScree to estimate the number of factors using 4 methods.
# Given the results, we use 3 factors

# Use eigen() to examine eigenvalues in a cor matrix
eigen(cor(brand.sc[, 1:9]))
# check in $values for eigenvalues. First 3 factors got eigenvalue >= 1

# estimate EFA model with 2 factors
factanal(brand.sc[, 1:9], factors = 2)
# loadings are the important values. when reading loadings, the factors that
# are important have the highest positive values, near 0 values means they are
# independent and negative values show a reverse relationship
# in the 2 factors model, factor 1 loads strongly in bargain and value, while 
# factor 2 loads high in leader and serious

#estimate EFA model with 3 factors
factanal(brand.sc[, 1:9], factors = 3)
# model remains consistent with the 2 factor analysis, but in factor 3 we can 
# a strong latest and trendy component. This alligns with the results of the
# scree amd eigen tests.

# EFA rotations
library(GPArotation)
brand.fa.ob <- factanal(brand.sc[, 1:9], factors = 3, rotation = 'oblimin')
brand.fa.ob

# visualize item-to-factor loadings
heatmap.2(brand.fa.ob$loadings, col = brewer.pal(9, "Greens"), trace = "none",
          key = FALSE, dend = "none", Colv = FALSE, cexCol = 1.2,
          main = "\n\n\n\n\n\nFactor loadings for brand adjectives")
# rebuy depends on whether is a value brand or a leader brand. 
# repeat purchase does not occur in lastest or trendy brands.

# other useful graphic is a path diagram
library(semPlot)
semPaths(brand.fa.ob, what = 'est', residuals = FALSE, cut = 0.3,
         posCol = c("white", "darkgreen"), negCol = c("white", "red"),
         edge.label.cex = 0.75, nCharNodes = 7)
# First we will explain the semPaths() call. We plotted the brand.fa.ob model
# as fit above. To draw the loading estimates, we requested what="est". We omit
# the residual estimates for manifest variables (an advanced topic we don’t 
# cover in this book) using residuals=FALSE. Then we cut loadings with absolute
# magnitude<0.3 by adding cut=0.3 and the options posCol=c("white", "darkgreen")
# and negCol=c("white", "red"). The posCol argument says that positive loadings 
# < 0.3 should be colored white (and thus not appear in the output), while 
# loadings > 0.3 should be darkgreen. The negCol argument similarly excludes or
# colors red the loadings < 0. We adjust the loadings’ text size with 
# edge.label.cex, and create room to spell out full variable names with 
# nCharNodes.

# Using Factor Scores on Brands
brand.fa.ob <- factanal(brand.sc[, 1:9], factors = 3, rotation = "oblimin",
                        scores = "Bartlett")
brand.scores <- data.frame(brand.fa.ob$scores)
brand.scores$brand <- brand.sc$brand
# brand.scores results are factor scores for each observation.
# if we want to dig deeper into the data and figure out how each observation
# is related to age, demographics or purchasing behavior, we use this granular
# data.
# can be helpful if we are running regressions or segmentations because the data
# is less complex (3 factors rather than 9 vars) and is more reliable (factor
# scores that reflect several manifest variables)

# to find the overall position of a brand we aggregate() individual scores by 
# brand
brand.fa.mean <- aggregate(. ~ brand, data = brand.scores, mean)
rownames(brand.fa.mean) <- brand.fa.mean[, 1]
brand.fa.mean <- brand.fa.mean[, -1]
names(brand.fa.mean) <- c('Leader', 'Value', 'Latest')

# create a heatmap to visualize the results
heatmap.2(as.matrix(brand.fa.mean), col = brewer.pal(9, "GnBu"), trace = 'none',
          key = FALSE, dend = "none", cexCol = 1.2, 
          main = "\n\n\n\nMean Factor Score by Brand")

#### Multidimensional Scaling ####
# measure distance between observations
brand.dist <- dist(brand.mean)
# find MDS solution
brand.mds <- cmdscale(brand.dist)

# visualize results
plot(brand.mds, type = 'n')
text(brand.mds, rownames(brand.mds), cex = 2)

# Non-metric MDS
# First, convert mean ratings into rankings
brand.rank <- data.frame(lapply(brand.mean, function (x) ordered(rank(x))))

# Find distances between ranks
library(cluster)
brand.dist.r <- daisy(brand.rank, metric = 'gower')

# then we apply non-metric MDS
library(MASS)
brand.mds.r <- isoMDS(brand.dist.r)

# aaannd we plot
plot(brand.mds.r$points, type = 'n')
text(brand.mds.r$points, levels(brand.sc$brand), cex = 2)
