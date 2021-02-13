# exercise data
pstr1 <- read.csv("https://goo.gl/z5P8ce")

# Exercises
# 1. Summarize PRST data. Should it be rescaled?
# yes, data is in likert scale which is near-metric.
brand.pstr <- pstr1
summary(brand.pstr)

# 2. Rescale the PRST data with a "Z-score" procedure and exmine the rescaled 
# data.
# Does this confirm your decision in the previous exercise about whether to
# rescale the data?
brand.pstr[, 1:9] <- data.frame(scale(brand.pstr[, 1:9]))
summary(brand.pstr)
# Yes, it allows us to see that the data is spread between 3 std dev from the 
# mean
 
# 3. Plot a correlation matrix for the adjective ratings. How many factors does
# it suggest?
corrplot(cor(brand.pstr[, 1:9]), order = "hclust")
# 3 factors

# 4. Aggregate the mean of each adjective rating by brand. Plot a heatmap for 
# the mean ratings by brand.
brand.mean <- aggregate(. ~ Brand, data = brand.pstr, FUN = mean)
rownames(brand.mean) <- brand.mean[, 1]
brand.mean <- brand.mean[, -1]

heatmap.2(as.matrix(brand.mean),
          col = brewer.pal(9, "GnBu"), trace = 'none', key = FALSE, dend = 'none',
          main = '\n\n\n\n\nBrand Attributes')

# 5. Extract the PCA in the PRST data. How many components are needed to explain
# the majority of variance in the PRST data? Visualize that.
brand.pc <- prcomp(brand.pstr[, 1:9])
plot(brand.pc, type = "l")
biplot(brand.pc, choices = 1:2)
biplot(brand.pc, choices = 2:3)
biplot(brand.pc, choices = 3:4)
summary(brand.pc)
# We may need 3 -4 PCs to explain the data

# 6. Using PCA for the mean adjective ratings, plot the brands against the first
# two components. How do you interpret that? Now plot against the second and 
# third components. Does this change your interpretation? What does this tell 
# you about interpreting PCA results?
brand.mu.scale <- prcomp(brand.mean, scale. = TRUE)
summary(brand.mu.scale)

?biplot.princomp
biplot(brand.mu.scale, choices = 1:2)
biplot(brand.mu.scale, choices = 2:3)

# First interpretation, Romeo has no differentiation. Papa is strong in 
# intuitiveness, Tango value while Sierra can't be said (not in node).
# Second interpretation, Tango is best value and adaptative, Romeo delightful 
# and Papa Generous. It changes all the analysis.

# 7. Suppose you are the brand manager of Sierra, and you wish to change your 
# position versus the market leader, Tango. What are some strategies suggested
# by the PCA positions?
brand.mean['Sierra',] - brand.mean['Tango',]
colMeans(brand.mean[c('Papa', 'Tango', 'Romeo'),]) - brand.mean['Sierra',]
# I have no leadership in any adjective. I'd boost my CuttingEdge and Best Value
# nerf Delightful and Generous

# Exploratory Factor Analysis
# 8. Consider a EFA for the PRST adjective ratings. How many factors should we
# extract?
nScree(brand.pstr[, 1:9])
eigen(cor(brand.pstr[, 1:9]))
# 3 factors

# 9. Find a EFA solution fopr the PRST data with an appropriate number of factors
# and rotation. What factor rotation did you select and why?
brand.fa.ob <- factanal(brand.pstr[, 1:9], factors = 3, rotation = 'oblimin')
brand.fa.ob
# 3 factors. Oblimin rotation because the items are correlated.

# 10. Draw a heatmap of the EFA factor loadings. Als draw a path diagram for 
# the EFA solution.
heatmap.2(brand.fa.ob$loadings, col = brewer.pal(9, "Greens"), trace = "none",
          key = FALSE, dend = "none", Colv = FALSE, cexCol = 1.2,
          main = "\n\n\n\n\n\nFactor loadings for brand adjectives")

semPaths(brand.fa.ob, what = 'est', residuals = FALSE, cut = 0.3,
         posCol = c("white", "darkgreen"), negCol = c("white", "red"),
         edge.label.cex = 0.75, nCharNodes = 7)

# 11. Find the mean factor scores for each brand and plot a heatmap of them.
brand.fa.ob <- factanal(brand.pstr[, 1:9], factors = 3, rotation = "oblimin",
                        scores = "Bartlett")
brand.scores <- data.frame(brand.fa.ob$scores)
brand.scores$brand <- brand.pstr$Brand

brand.fa.mean <- aggregate(. ~ brand, data = brand.scores, mean)
rownames(brand.fa.mean) <- brand.fa.mean[, 1]
brand.fa.mean <- brand.fa.mean[, -1]
names(brand.fa.mean) <- c('Intuitive', 'Exciting', 'BestValue')

heatmap.2(as.matrix(brand.fa.mean), col = brewer.pal(9, "GnBu"), trace = 'none',
          key = FALSE, dend = "none", cexCol = 1.2, 
          main = "\n\n\n\nMean Factor Score by Brand")

# 12.Compare the factor score heatmap for PRST brands to the PCA interpretations
# in Exercise 6 above. Does the heatmap suggest different directions for the 
# brand strategy for Sierra vs Tango?
# Yes, Sierra can compete with Tango in the Exciting factor or become a leader
# in Best Value factor.

# 13. Plot a multidimensional scaling (MDS) map for the PRST brand using the mean
# adjective ratings. Which brands are most similar and most different?
brand.dist <- dist(brand.mean)
brand.mds <- cmdscale(brand.dist)

plot(brand.mds, type = 'n')
text(brand.mds, rownames(brand.mds), cex = 2)
# Romeo and Sierra are kind of similar. Papa and Tango are very differentiated
# in different territories.

# 14. How does MDS map relate to the PCA and EFA positions in the exercises above?
# What does it suggest for the strategy you considered in Exercise 6 above?
# MDS shows a a similar positioning with PCA, but less dramatic in its findings. 
# EFA confirms that Tango and Papa are well differentiated and Sierra and Rome
# have room to find a territory for themselves.
# Sierra needs to differentiate from Romero, as they both share the same territory
# and may be competing for customers while Tango and Papa, eadh, have their own 
# customer profile.