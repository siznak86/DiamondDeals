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


## Basic Stats
# ERA
# Mean
mean(FreeAgencyERA$AGE)
mean(FreeAgencyERA$`AVG. SALARY`)
mean(FreeAgencyERA$IP)
mean(FreeAgencyERA$ERA)
mean(FreeAgencyERA$WHIP)
mean(FreeAgencyERA$W)
mean(FreeAgencyERA$SV)
mean(FreeAgencyERA$WAR)

# Median
median(FreeAgencyERA$AGE)
median(FreeAgencyERA$`AVG. SALARY`)
median(FreeAgencyERA$IP)
median(FreeAgencyERA$ERA)
median(FreeAgencyERA$WHIP)
median(FreeAgencyERA$W)
median(FreeAgencyERA$SV)
median(FreeAgencyERA$WAR)

# Minimum
min(FreeAgencyERA$AGE)
min(FreeAgencyERA$`AVG. SALARY`)
min(FreeAgencyERA$IP)
min(FreeAgencyERA$ERA)
min(FreeAgencyERA$WHIP)
min(FreeAgencyERA$W)
min(FreeAgencyERA$SV)
min(FreeAgencyERA$WAR)

# Maximum
max(FreeAgencyERA$AGE)
max(FreeAgencyERA$`AVG. SALARY`)
max(FreeAgencyERA$IP)
max(FreeAgencyERA$ERA)
max(FreeAgencyERA$WHIP)
max(FreeAgencyERA$W)
max(FreeAgencyERA$SV)
max(FreeAgencyERA$WAR)

# Standard Deviation
sd(FreeAgencyERA$AGE)
sd(FreeAgencyERA$`AVG. SALARY`)
sd(FreeAgencyERA$IP)
sd(FreeAgencyERA$ERA)
sd(FreeAgencyERA$WHIP)
sd(FreeAgencyERA$W)
sd(FreeAgencyERA$SV)
sd(FreeAgencyERA$WAR)

# OPS
# Mean
mean(FreeAgencyOPS$AGE)
mean(FreeAgencyOPS$`AVG. SALARY`)
mean(FreeAgencyOPS$H)
mean(FreeAgencyOPS$RBI)
mean(FreeAgencyOPS$HR)
mean(FreeAgencyOPS$AVG)
mean(FreeAgencyOPS$OPS)
mean(FreeAgencyOPS$WAR)

# Median
median(FreeAgencyOPS$AGE)
median(FreeAgencyOPS$`AVG. SALARY`)
median(FreeAgencyOPS$H)
median(FreeAgencyOPS$RBI)
median(FreeAgencyOPS$HR)
median(FreeAgencyOPS$AVG)
median(FreeAgencyOPS$OPS)
median(FreeAgencyOPS$WAR)

# Minimum
min(FreeAgencyOPS$AGE)
min(FreeAgencyOPS$`AVG. SALARY`)
min(FreeAgencyOPS$H)
min(FreeAgencyOPS$RBI)
min(FreeAgencyOPS$HR)
min(FreeAgencyOPS$AVG)
min(FreeAgencyOPS$OPS)
min(FreeAgencyOPS$WAR)

# Maximum
max(FreeAgencyOPS$AGE)
max(FreeAgencyOPS$`AVG. SALARY`)
max(FreeAgencyOPS$H)
max(FreeAgencyOPS$RBI)
max(FreeAgencyOPS$HR)
max(FreeAgencyOPS$AVG)
max(FreeAgencyOPS$OPS)
max(FreeAgencyOPS$WAR)

# Standard Deviation
sd(FreeAgencyOPS$AGE)
sd(FreeAgencyOPS$`AVG. SALARY`)
sd(FreeAgencyOPS$H)
sd(FreeAgencyOPS$RBI)
sd(FreeAgencyOPS$HR)
sd(FreeAgencyOPS$AVG)
sd(FreeAgencyOPS$OPS)
sd(FreeAgencyOPS$WAR)


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


  

## Check for mediation and moderation while running step wise linear regression
# ERA
# Step wise linear regression
step_model_ERA <- step(lm(`AVG. SALARY` ~ AGE + IP + ERA + WHIP + W + SV + WAR, data = FreeAgencyERA), direction = "both")

summary(step_model_ERA)

# Mediation analysis
# Mediator(AGE) IV(WAR) DV(AVG. SALRARY) Control()
mediation_model_ERA_AGE <- lm(AGE ~ WAR, data = FreeAgencyERA)
outcome_model_ERA_AGE <- lm(`AVG. SALARY` ~ WAR + AGE, data = FreeAgencyERA)
mediate_model_ERA_AGE <- mediate(mediation_model_ERA_AGE, outcome_model_ERA_AGE, treat = "WAR", mediator = "AGE", boot = TRUE)

summary(mediation_model_ERA_AGE)
summary(mediate_model_ERA_AGE)
# Maybe Significant p = .064

# Mediator(ERA) IV(WAR) DV(AVG. SALRARY) Control()
mediation_model_ERA_ERA <- lm(ERA ~ WAR, data = FreeAgencyERA)
outcome_model_ERA_ERA <- lm(`AVG. SALARY` ~ WAR + ERA, data = FreeAgencyERA)
mediate_model_ERA_ERA <- mediate(mediation_model_ERA_ERA, outcome_model_ERA_ERA, treat = "WAR", mediator = "ERA", boot = TRUE)

summary(mediation_model_ERA_ERA)
summary(mediate_model_ERA_ERA)
# Maybe Significant p = .094

# Mediator(WHIP) IV(WAR) DV(AVG. SALRARY) Control()
mediation_model_ERA_WHIP <- lm(WHIP ~ WAR, data = FreeAgencyERA)
outcome_model_ERA_WHIP <- lm(`AVG. SALARY` ~ WAR + WHIP, data = FreeAgencyERA)
mediate_model_ERA_WHIP <- mediate(mediation_model_ERA_WHIP, outcome_model_ERA_WHIP, treat = "WAR", mediator = "WHIP", boot = TRUE)

summary(mediation_model_ERA_WHIP)
summary(mediate_model_ERA_WHIP)
# Not Significant

# Mediator(W) IV(WAR) DV(AVG. SALRARY) Control()
mediation_model_ERA_W <- lm(W ~ WAR, data = FreeAgencyERA)
outcome_model_ERA_W <- lm(`AVG. SALARY` ~ WAR + W, data = FreeAgencyERA)
mediate_model_ERA_W <- mediate(mediation_model_ERA_W, outcome_model_ERA_W, treat = "WAR", mediator = "W", boot = TRUE)

summary(mediation_model_ERA_W)
summary(mediate_model_ERA_W)
# Significant p = 2e-16

# Mediator(SV) IV(WAR) DV(AVG. SALRARY) Control()
mediation_model_ERA_SV <- lm(SV ~ WAR, data = FreeAgencyERA)
outcome_model_ERA_SV <- lm(`AVG. SALARY` ~ WAR + SV, data = FreeAgencyERA)
mediate_model_ERA_SV <- mediate(mediation_model_ERA_SV, outcome_model_ERA_SV, treat = "WAR", mediator = "SV", boot = TRUE)

summary(mediation_model_ERA_SV)
summary(mediate_model_ERA_SV)
# Not Significant

# Moderation analysis
# Moderator(AGE) IV(WAR) DV(AVG. SALARY)
moderation_model_ERA_AGE <- lm(`AVG. SALARY` ~ WAR * AGE, data = FreeAgencyERA)

summary(moderation_model_ERA_AGE)
# Not Significant

# Moderator(ERA) IV(WAR) DV(AVG. SALARY)
moderation_model_ERA_ERA <- lm(`AVG. SALARY` ~ WAR * ERA, data = FreeAgencyERA)

summary(moderation_model_ERA_ERA)
# Not Significant

# Moderator(WHIP) IV(WAR) DV(AVG. SALARY)
moderation_model_ERA_WHIP <- lm(`AVG. SALARY` ~ WAR * WHIP, data = FreeAgencyERA)

summary(moderation_model_ERA_WHIP)
# Significant p = 1.43e-07

# Moderator(W) IV(WAR) DV(AVG. SALARY)
moderation_model_ERA_W <- lm(`AVG. SALARY` ~ WAR * W, data = FreeAgencyERA)

summary(moderation_model_ERA_W)
# Significnat p = .000168

# Moderator(SV) IV(WAR) DV(AVG. SALARY)
moderation_model_ERA_SV <- lm(`AVG. SALARY` ~ WAR * SV, data = FreeAgencyERA)

summary(moderation_model_ERA_SV)
# Not Significant

# Combined Mediation-Moderation Analysis 
combined_model_ERA <- lm(`AVG. SALARY` ~ WAR * W * WHIP, data = FreeAgencyERA)

summary(combined_model_ERA)
 

# OPS
# Step wise linear regression
step_model_OPS <- step(lm(`AVG. SALARY` ~ AGE + H + RBI + HR + AVG + OPS + WAR, data = FreeAgencyOPS), direction = "both")

summary(step_model_OPS)

# Mediation analysis
# Mediator(AGE) IV(WAR) DV(AVG. SALARY)
mediation_model_OPS_AGE <- lm(AGE ~ WAR, data = FreeAgencyOPS)
outcome_model_OPS_AGE <- lm(`AVG. SALARY` ~ WAR + AGE, data = FreeAgencyOPS)
mediate_model_OPS_AGE <- mediate(mediation_model_OPS_AGE, outcome_model_OPS_AGE, treat = "WAR", mediator = "AGE", boot = TRUE)

summary(mediation_model_OPS_AGE)
summary(mediate_model_OPS_AGE)
# Not Significant

# Mediator(HR) IV(WAR) DV(AVG. SALARY)
mediation_model_OPS_HR <- lm(HR ~ WAR, data = FreeAgencyOPS)
outcome_model_OPS_HR <- lm(`AVG. SALARY` ~ WAR + HR, data = FreeAgencyOPS)
mediate_model_OPS_HR <- mediate(mediation_model_OPS_HR, outcome_model_OPS_HR, treat = "WAR", mediator = "HR", boot = TRUE)

summary(mediation_model_OPS_HR)
summary(mediate_model_OPS_HR)
# Significant p = 2e-16

# Mediator(OPS) IV(WAR) DV(AVG. SALARY)
mediation_model_OPS_OPS <- lm(OPS ~ WAR, data = FreeAgencyOPS)
outcome_model_OPS_OPS <- lm(`AVG. SALARY` ~ WAR + OPS, data = FreeAgencyOPS)
mediate_model_OPS_OPS <- mediate(mediation_model_OPS_OPS, outcome_model_OPS_OPS, treat = "WAR", mediator = "OPS", boot = TRUE)

summary(mediation_model_OPS_OPS)
summary(mediate_model_OPS_OPS)
# Significant p = .004

# Moderation analysis
# Moderator(AGE) IV(WAR) DV(AVG. SALARY)
moderation_model_OPS_AGE <- lm(`AVG. SALARY` ~ WAR * AGE, data = FreeAgencyOPS)

summary(moderation_model_OPS_AGE)
# Significant p = .000125

# Moderator(HR) IV(WAR) DV(AVG. SALARY)
moderation_model_OPS_HR <- lm(`AVG. SALARY` ~ WAR * HR, data = FreeAgencyOPS)

summary(moderation_model_OPS_HR)
# Maybe Significant p = .945

# Moderator(OPS) IV(WAR) DV(AVG. SALARY)
moderation_model_OPS_OPS <- lm(`AVG. SALARY` ~ WAR * OPS, data = FreeAgencyOPS)

summary(moderation_model_OPS_OPS)
# Significant p = .000564

# Combined Mediation-Moderation Analysis 
combined_model_OPS <- lm(`AVG. SALARY` ~ WAR * AGE * HR * OPS, data = FreeAgencyOPS)

summary(combined_model_OPS)
