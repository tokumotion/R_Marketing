library(tidyverse); library(corrplot); library(GGally); library(car)
library(psych)

hotel.df <- read.csv('https://goo.gl/oaWKgt')

summary(hotel.df)
# 7-point Likert scale for the first 18 variables
# visitPurpose & eliteStatus as factor
# rest of variables continuous

# 1. Visualize distributions and transformed as necessary

# First glance of how the data looks like
describe(hotel.df)

#pairs
ggpairs(hotel.df[, 1:10])
# satFrontStaff and satCleanBath have high corr with the other vars

ggpairs(hotel.df[, 11:20])
# distanceTraveled and nightsStayed have log distributions

ggpairs(hotel.df[, 21:27])
# avgFoodSpendPerNight have log dist. avgRoomSpendPerNight and 
# avgWifiSpendPerNight have similar dist which are not normal (2 modes)

# make tranformations
par(mfrow = c(1,2))
hist(hotel.df$distanceTraveled)
hist(log(hotel.df$distanceTraveled))

hist(hotel.df$nightsStayed)
hist(log(hotel.df$nightsStayed))

hist(hotel.df$avgFoodSpendPerNight)
hist(log(hotel.df$avgFoodSpendPerNight + 1))

hotel.df.tr <- hotel.df
hotel.df.tr$distanceTraveled <- log(hotel.df$distanceTraveled)
hotel.df.tr$nightsStayed <- log(hotel.df$nightsStayed)
hotel.df.tr$avgFoodSpendPerNight <- log(hotel.df$avgFoodSpendPerNight + 1)

# 2. briefly summarize correlations
corrplot(cor(hotel.df.tr[, -c(21, 25)]))
# satFrontStaff is positively correlated with a lot sat variables
# satPerks too
# satDiningPrince positively correlated with avgFoodSpendperNight
# satParkingPrice and satValetStaff negatively correlated
# avgRoomSpendperNight negatively correlated with parkingprice and diningprice

# 3. Consider 3 items of cleanliness (satCleanRoom, satCleanBath, 
# satCleanCommon) and find cor coef person's r. Is there a better measure? why?
# Pearson's R won't cut it, because data responds to 7-point likert scale
# polychoric() for likert data
cor(hotel.df.tr[, 1:3])
polychoric(hotel.df.tr[, 1:3]) # makes correlation higher

# 4. Does satisfaction with satPerks predicts satOverall. satPerks is predictor
cor.test(hotel.df.tr$satPerks, hotel.df.tr$satOverall)
#satPerks has a positive correlation with satOverall
summary(lm(satOverall ~ satPerks, data = hotel.df.tr))
# from lm(), satPerks explain 16.18% of avgOverall variance

# 5. add satFrontStaff and satCity into lm()
summary(lm(satOverall ~ satPerks + satCity + satFrontStaff, 
           data = hotel.df.tr))
# satFrontStaff explains better avgOverall than satPerks. the model explains
# 28.72% of the variance in which satCity explains the lowest.

# 6. Suppose we have a business strategy to maximize satisfaction with elite 
# recognition (satRecognition), among our Gold and Platinum elite members. 
# Suppose that we might invest more in the front staff, room cleanliness,
# the points that we give, or the membership perks. Which of those seems best 
# to consider, to increase Gold and Platinum member satisfaction with elite 
# recognition?
hotel.rec.lm <- lm(satRecognition ~ satFrontStaff + satCleanRoom + satPerks + 
                     satPoints, 
                   data = subset(hotel.df.tr, 
                                 subset = eliteStatus %in% c('Gold', 
                                                             'Platinum')))
summary(hotel.rec.lm)
# satPerks + satPoints will deliver faster recognition in this segment

# 7. What ae some of the problems with this data.
# the model doesn't explain completely the variance of the response variable
# in fact, it only account for 19.45% of the variance. We need to explore the 
# whole set of variables to understand which other actions can be taken

# 8. From these data and the business question and model in the previous 
# question, would you recommend to invest more in cleanliness? Why or why not?
# I wouldn't because satCleanRoom is a poor predictor (high p-value) and has 
# little predictive power compared to satPerks and satPoints

# 9. Now suppose that we want to improve revenues in the restaurant, and with 
# to understand the relationship of average food spend per night as it relates
# to elite status (eliteStatus) and satisfaction with food price 
# (satDiningPrice). Model and interpret the relationships.
hotel.food.lm <- lm(avgFoodSpendPerNight ~ satDiningPrice + eliteStatus, 
                    data = hotel.df.tr)
summary(hotel.food.lm)
# eliteStatus doesn't predict avgFoodSpendPerNight, satDiningPrice does 
# although those 2 variables don't explain much of the overall variance of the 
# response variable

# 10. We might expect dining satisfaction to be higher when food costs less; 
# lower price often predicts higher satisfaction. However, we might also expect 
# the opposite relationship, where satisfied diners spend more. Which is it, in 
# these data?
hotel.foodsat.lm <- lm(avgFoodSpendPerNight ~ satDiningPrice, 
                       data = hotel.df.tr)
summary(hotel.foodsat.lm)
# there is a slight positive relationship between avgFoodSpendPerNight and 
# satDiningPrice. But satDiningPrice explains only 2.83% of the variance of
# avgFoodSpendPerNight

# 11. Plot the predicted food spend per night in dollars, as a function of 
# nights stayed. (Hint: start by fiting a linear model with one predictor.)
hotel.avgfoodnighs.lm <- lm(avgFoodSpendPerNight ~ nightsStayed, 
                            data = hotel.df.tr)
plot(jitter(exp(hotel.df.tr$nightsStayed)), 
     jitter(exp(fitted(hotel.avgfoodnighs.lm))))
abline(v = '40')
exp(predict(hotel.avgfoodnighs.lm, 
            newdata = data.frame(nightsStayed = log(40))))
# a good guess according to the model is 29.76 dollars

# 12. Is that model substantially differ if you limit the data to just Platinum 
# elite members? Plot the results to see the difference. What does this suggest 
# for a restaurant strategy?
hotel.platinumfood <- lm(avgFoodSpendPerNight ~ nightsStayed,
                         data = subset(hotel.df.tr, 
                                       subset = eliteStatus %in% 'Platinum' ))
summary(hotel.platinumfood)
summary(hotel.avgfoodnighs.lm)
# platinum members tend to spend slightly more  as they stay more nights compared 
# to the avg hotel user

plot(jitter(exp(hotel.df.tr$nightsStayed)), 
     jitter(exp(fitted(hotel.avgfoodnighs.lm))), col = 'red', 
     xlab = 'Nights Stayed', ylab = 'Mean Food Spend per Night ($)')
points(jitter(exp(hotel.df.tr$nightsStayed[hotel.df.tr$eliteStatus=='Platinum'])),
       jitter(exp(fitted(hotel.platinumfood))), col = 'blue')
legend("topleft", legend=c("All visitors", "Platinum members"), 
       col=c("red", "blue"), pch=1)

# 13. Fit the elite recognition model (Exercise 6 above) using Bayesian 
# regression. Which variables are most associated with members' satisfaction 
# with recognition?
library(MCMCpack)
hotel.rec.lm.b <- MCMCregress(satRecognition ~ satFrontStaff + satCleanRoom + 
                                satPerks + satPoints, 
                              data = subset(hotel.df.tr, 
                                            subset = eliteStatus %in% c('Gold', 
                                                                        'Platinum')))
summary(hotel.rec.lm)
summary(hotel.rec.lm.b)

# 14. How do the Bayesian estimates compare to the classical linear model 
# estimates in Exercise 6? Visualize the relationship among the coefficients 
# from each. What is the correlation coefficient?
hotel.rec.compare <- data.frame(classical = coef(hotel.rec.lm)[-1],
                                bayesian = 
                                  summary(hotel.rec.lm.b)$statistics[c(-1, -6), 1])
plot(bayesian ~ classical, data = hotel.rec.compare)
abline(0, 1)
cor(hotel.rec.compare$classical, hotel.rec.compare$bayesian)
