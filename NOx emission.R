### Gas Emissions Flexible Regression ###

#Emission of oxides of nitrogen, NOx, in 22 cars were studied as a function of the equivalence
#ratio, E, a measure of engine performance. It was of interest to investigate the relationship
#between the pollution emissions and the engine performance

#### Set up ####
setwd("~/Desktop/y4/Flexible Regression/Lab 1")
gas <- read.table('Gas.txt', header = TRUE)
View(gas)

library(ggplot2)
library(splines)


#### Exploratory analysis ####
# Produce a scatterplot of NOx (response) against engine performance
ggplot(data = gas, mapping = aes(x = E, y = NOx)) +
  geom_point()
# The relationship is clearly not linear. Could be quadratic but looks like there are two 
# peaks on the curve that could be significant

#### Intercept-only model ####
mod1 <- lm(data = gas, NOx ~ 1)
summary(mod1)


#### Simple linear model ####
mod2 <- lm(data = gas, NOx ~ E)
summary(mod2)


#### Non-parametric model ####
mod3 <- lm(data = gas, NOx ~ bs(E, df = 6))
summary(mod3)

#### Model comparison ####
# compare mod1 (intercept-only) with mod3 (non-parametric model), mod1 nested in mod3
anova(mod1, mod2)
anova(mod2, mod3)
# F for model1 vs model3 is significant => reject h0 => there exists a relationship 
# F for model 2 vs model3 is significant => reject h0 => the relationship is not linear

AIC(mod1) ; AIC(mod2) ; AIC(mod3)
# mod3 has lowest AIC => most favourable out of the three models

#### Diagnostic checks ####
par(mfrow = c(2,2))
plot(mod3)
# the residual vs fitted plot seems reasonably well behaved, though some evidence of 
# non-linearity. Q-Q plot appears to pass
# scale-location shows that residuals are decently homoscedastic.
# leverage plot shows thatt observations 1 and 22 are influential

#### Plot influential observations ####
ggplot(data = gas, mapping = aes(x = E, y = NOx)) +
  geom_point() +
  geom_text(data=gas[22,],aes(x = E, y = NOx, label = "22"), colour = 'red', nudge_y = -0.15) +
  geom_text(data=gas[1,],aes(x = E, y = NOx, label = "1"), colour = 'red', nudge_y = -0.15) 
# turns out it was just the observations at the extremities of the dataset, i think i will
# just ignore this, they are fine to include in the regression model


#### Plot data with fitted model ####
par(mfrow = c(1,1))
plot(gas$E, gas$NOx)

# Plot fitted line
lines(gas$E, predict(mod3))
# looks pretty good

# try different levels of smoothing
par(mfrow = c(2,2))
plot(gas$E, gas$NOx, main = "6 degrees of freedom")
lines(gas$E, predict(lm(data = gas, NOx ~ bs(E, df = 6))))
plot(gas$E, gas$NOx, main = "10 degrees of freedom")
lines(gas$E, predict(lm(data = gas, NOx ~ bs(E, df = 10))))
plot(gas$E, gas$NOx, main = "15 degrees of freedom")
lines(gas$E, predict(lm(data = gas, NOx ~ bs(E, df = 15))))
plot(gas$E, gas$NOx, main = "20 degrees of freedom")
lines(gas$E, predict(lm(data = gas, NOx ~ bs(E, df = 20))))
par(mfrow = c(1,1))
# 15 df looks better at accounting for the two peaks

AIC(mod3) ; AIC(lm(data = gas, NOx ~ bs(E, df = 15)))
# it has lower aic than the original model with 6df 


#### Fit model with cubic splines ####
# Try a model with natural cubic splines instead of B-splines. Natural cubic splines 
# are linear beyond the boundary knots
mod4 <- lm(data = gas, NOx ~ ns(E, df = 10))
plot(gas$E, gas$NOx, main = "ns with 10 df")
lines(gas$E, predict(mod4))
AIC(lm(data = gas, NOx ~ bs(E, df = 15))) ; AIC(mod4)
# the natural cubic spline with 10df has a lower AIC  than the 15df b-spline model