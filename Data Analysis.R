# Packages for Stepwise Linear Regression
#install.packages("mediation")
#install.packages("moderndive")

library(corrplot)
library(ggplot2)
library(ggplotify)
library(nnet)
library(mediation)
library(moderndive)
library(readxl)
library(interactions)
library(jtools)
library(forecast)
library(car)


## Correlation Plots
# ERA
correlation_matrix_ERA <- cor(FreeAgencyERA[, c("AGE", "YRS", "AVG. SALARY", "ERA", "IP", "WHIP","W", "SV", "WAR")])
corrplot(correlation_matrix_ERA, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - ERA")

# OPS
correlation_matrix_OPS <- cor(FreeAgencyOPS[, c("AGE", "YRS", "AVG. SALARY", "OPS", "WAR", "H", "RBI", "HR", "AVG", "PosNumber")])
corrplot(correlation_matrix_OPS, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - OPS")

## Scatter Plots
# ERA
# Scatter Plot Age vs. Salary
ggplot(FreeAgencyERA, aes(x = AGE, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: AGE V. Free Agent Salary", x = "Age", y = "Salary")
# Scatter Plot  Years vs. Salary
ggplot(FreeAgencyERA, aes(x = YRS, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: Years V. Free Agent Salary", x = "Years", y = "Salary")
# Scatter Plot ERA vs. Salary
ggplot(FreeAgencyERA, aes(x = ERA, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: ERA V. Free Agent Salary", x = "ERA", y = "Salary")
# Scatter Plot Innings Pitched vs. Salary
ggplot(FreeAgencyERA, aes(x = IP, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: Innings Pitched V. Free Agent Salary", x = "IP", y = "Salary")
# Scatter Plot WHIP vs. Salary
ggplot(FreeAgencyERA, aes(x = WHIP, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: WHIP V. Free Agent Salary", x = "WHIP", y = "Salary")
# Scatter Plot Wins vs. Salary
ggplot(FreeAgencyERA, aes(x = W, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: Wins V. Free Agent Salary", x = "Wins", y = "Salary")
# Scatter Plot Saves vs. Salary
ggplot(FreeAgencyERA, aes(x = SV, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: Saves V. Free Agent Salary", x = "Saves", y = "Salary")
# Scatter Plot WAR vs. Salary
ggplot(FreeAgencyERA, aes(x = WAR, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: WAR V. Free Agent Salary", x = "WAR", y = "Salary")

# OPS
# Scatter Plot Age vs. Salary
ggplot(FreeAgencyOPS, aes(x = AGE, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: AGE V. Free Agent Salary", x = "Age", y = "Salary")
# Scatter Plot  Years vs. Salary
ggplot(FreeAgencyOPS, aes(x = YRS, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: Years V. Free Agent Salary", x = "Years", y = "Salary")
# Scatter Plot OPS vs. Salary
ggplot(FreeAgencyOPS, aes(x = OPS, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: OPS V. Free Agent Salary", x = "OPS", y = "Salary")
# Scatter Plot Hits vs. Salary
ggplot(FreeAgencyOPS, aes(x = H, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: Hits V. Free Agent Salary", x = "Hits", y = "Salary")
# Scatter Plot RBIs vs. Salary
ggplot(FreeAgencyOPS, aes(x = RBI, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: RBIs V. Free Agent Salary", x = "RBI", y = "Salary")
# Scatter Plot Home Runs vs. Salary
ggplot(FreeAgencyOPS, aes(x = HR, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: Home Runs V. Free Agent Salary", x = "Home Runs", y = "Salary")
# Scatter Plot Average vs. Salary
ggplot(FreeAgencyOPS, aes(x = AVG, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: Batting Average V. Free Agent Salary", x = "Average", y = "Salary")
# Scatter Plot Position vs. Salary
ggplot(FreeAgencyOPS, aes(x = PosNumber, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: Position V. Free Agent Salary", x = "Position", y = "Salary")
# Scatter Plot WAR vs. Salary
ggplot(FreeAgencyOPS, aes(x = WAR, y = `AVG. SALARY`)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: WAR V. Free Agent Salary", x = "WAR", y = "Salary")


## Regressions
# Multiple Linear Regression ERA
model_ERA_multiLinear <- lm(`AVG. SALARY` ~ ERA + WAR + IP + WHIP + W + SV, data = FreeAgencyERA)
summary(model_ERA_multiLinear)
#Significance found in all except IP 

predictions_ERA <- predict(model_ERA_multiLinear, newdata = FreeAgencyERA)
plot(model_ERA_multiLinear)
# Linearity and Additivity
plot(model_ERA_multiLinear$fitted.values, model_ERA_multiLinear$residuals,
     xlab = "Predicted values", ylab = "Residuals")
  abline(h = 0, col = "red")
# Independence of Residuals
Acf(model_ERA_multiLinear$residuals, main = "ACF of Model Residuals")
# Homoscedasticity
plot(model_ERA_multiLinear$fitted.values, abs(model_ERA_multiLinear$residuals), 
     xlab = "Predicted values", ylab = "Absolute Residuals")
# Normality of Errors
qqnorm(model_ERA_multiLinear$residuals)
qqline(model_ERA_multiLinear$residuals)
# Absence of Multicollinearity
vif(model_ERA_multiLinear)
# No Influential Outliers
cooksd <- cooks.distance(model_ERA_multiLinear)
threshold <- 4/length(cooksd)
yrange <- c(0, max(cooksd, threshold) * 1.1)
plot(cooksd, ylim = yrange, main = "Influential Observations by Cook's distance", pch = 16)
  abline(h = threshold, col = "red")

  
  

# Multiple Linear Regression OPS
model_OPS_multiLinear <- lm(`AVG. SALARY` ~ OPS + WAR + H + RBI + HR + AVG + PosNumber, data = FreeAgencyOPS)
summary(model_OPS_multiLinear)
# Significance in WAR and HR only

predictions_OPS <- predict(model_OPS_multiLinear, newdata = FreeAgencyOPS)
plot(model_OPS_multiLinear, which = 1)
plot(model_OPS_multiLinear$fitted.values, model_OPS_multiLinear$residuals,
     xlab = "Predicted values", ylab = "Residuals")
  abline(h = 0, col = "red")


  

## Check for mediation and moderation while running stepwise linear regression
# Stepwise linear regression
step_model_ERA <- step(lm(`AVG. SALARY` ~ ., data = FreeAgencyERA), direction = "both")

# Mediation analysis
mediation_model_ERA <- lm(AGE ~ ERA, data = FreeAgencyERA)
mediate_model_ERA <- mediate(mediation_model_ERA, `AVG. SALARY` ~ ERA)

# Moderation analysis
moderation_model_ERA <- lm(`AVG. SALARY` ~ ERA * SV, data = FreeAgencyERA)

# Display results
summary(step_model_ERA)
summary(mediation_model_ERA)
summary(mediate_model_ERA)
summary(moderation_model_ERA)

