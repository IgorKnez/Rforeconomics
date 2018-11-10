### Correlation and Regression


# Several datasets used through openintro library
library(openintro)
library(HistData)

# libraries uesd 

library(ggplot2)
library(tidyverse)

### Visualization

# Scatterplot of weight vs. weeks
ggplot(ncbirths,aes(x=weeks,y=weight)) +
  geom_point()


# Boxplot of weight vs. weeks
ggplot(data = ncbirths, 
       aes(x = cut(weeks, breaks = 5), y = weight)) + 
  geom_boxplot()


# Mammals scatterplot
ggplot(mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point()


# Baseball player scatterplot
ggplot(mlbBat10, aes(x = OBP, y = SLG)) +
  geom_point()


# Body dimensions scatterplot
ggplot(bdims, aes(x = hgt, y = wgt, color = factor(sex))) +
  geom_point()


# Smoking scatterplot
ggplot(smoking, aes(x = age, y = amtWeekdays)) +
  geom_point()

# Scatterplot with coord_trans()
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() + 
  coord_trans(x = "log10", y = "log10")

# Scatterplot with scale_x_log10() and scale_y_log10()
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()

#indentyfing outliers 


# Scatterplot of SLG vs. OBP
mlbBat10 %>%
  filter(AB >= 200) %>%
  ggplot(aes(x = OBP, y = SLG)) +
  geom_point()

# Identify the outlying player
mlbBat10 %>%
  filter(AB >= 200 & OBP < 0.200)


# Compute correlation
ncbirths %>%
  summarize(N = n(), r = cor(weight,mage))

# Compute correlation for all non-missing pairs
ncbirths %>%
  summarize(N = n(), r = cor(weight,weeks, use = "pairwise.complete.obs"))


#Correlation for all baseball players
mlbBat10 %>%
  summarize(N = n(), r = cor(OBP, SLG))

# Correlation for all players with at least 200 ABs
mlbBat10 %>%
  filter(AB >= 200) %>%
  summarize(N = n(), r = cor(OBP, SLG))

# Correlation of body dimensions
bdims %>%
  group_by(sex) %>%
  summarize(N = n(), r = cor(hgt, wgt))

# Correlation among mammals, with and without log
mammals %>%
  summarize(N = n(), 
            r = cor(BodyWt, BrainWt), 
            r_log = cor(log(BodyWt), log(BrainWt)))



## simple linear regression

# Scatterplot with regression line
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)


# Print bdims_summary
bdims_summary <- bdims %>%
  summarise(N = n(),r = cor(hgt,wgt), mean_hgt = mean(hgt), sd_hgt = sd(hgt),
            mean_wgt = mean(wgt), sd_wgt = sd(wgt))


print(bdims_summary)

# Add slope and intercept manually

bdims_summary %>%
  mutate(slope = r * sd_wgt/sd_hgt, 
         intercept = mean_wgt - mean_hgt * slope)


#Regression to the mean is a concept attributed to Sir Francis Galton.
# The basic idea is that extreme random observations will tend to be less
# extreme upon a second trial.

# Height of children vs. height of father
ggplot(data = GaltonFamilies, aes(x = father, y = childHeight)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = 'lm', se = FALSE)

# Height of children vs. height of mother
ggplot(data = GaltonFamilies, aes(x = mother, y = childHeight)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = 'lm', se = FALSE)


# simple linear model calculation

# Linear model for weight as a function of height
mod <- lm(wgt ~ hgt, data = bdims)

# Linear model for SLG as a function of OBP
lm(SLG ~ OBP, data = mlbBat10)

# Log-linear model for body weight as a function of brain weight
lm(log(BodyWt) ~ log(BrainWt), data = mammals)

# Show the coefficients

coefs <- as.data.frame(transpose(data.frame(coef(mod))))
colnames(coefs) <- c("(Intercept)", "hgt")
# Show the full output
summary(mod)

# Mean of weights equal to mean of fitted values?
mean(bdims$wgt) == mean(fitted.values(mod))


# Mean of the residuals
mean(residuals(mod))


# Load broom
library(broom)

# Create bdims_tidy
bdims_tidy <- augment(mod)

# Glimpse the resulting data frame
glimpse(bdims_tidy)



ben <- data.frame(wgt = 74.8, hgt = 182.8)

# Predict the weight of ben
predict(mod, ben)


# Add the line to the scatterplot
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(data = coefs, 
              aes(intercept = `(Intercept)`, slope = `hgt`),  
              color = "dodgerblue")


## Assesing model fit

# View summary of model
summary(mod)

# Compute the mean of the residuals
mean(residuals(mod))

# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))



# Compute R-squared
bdims_tidy %>%
  summarize(var_y = var(wgt), var_e = var(.resid)) %>%
  mutate(R_squared = 1 - var_e/var_y)


# Rank points of high leverage
mod %>%
  augment() %>%
  arrange(desc(.hat)) %>%
  head()


# Rank influential points
mod %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  head()


# Removing outliers

# Create nontrivial_players
nontrivial_players <- mlbBat10 %>%
  filter(AB >= 10, OBP <0.5)


# Fit model to new data
mod_cleaner <- lm (SLG ~ OBP, nontrivial_players)

# View model summary
summary(mod_cleaner)

# Visualize new model
ggplot(data = nontrivial_players, aes(x =OBP, y = SLG)) +
  geom_point() +
  geom_smooth(method = 'lm')


# Rank high leverage points
mod %>%
  augment() %>%
  arrange(desc(.hat), .cooksd) %>%
  head()
