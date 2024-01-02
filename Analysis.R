# Stepwise Multinomial Logistic Regression

#install.packages("nnet")
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(broom)
library(corrplot)
library(dplyr)

# Graphing Data
# Independent Variables x (ERA, OPS, WAR, Age)
# Dependent Variables y (Salary, Position, Contract Length)

# Scatter Plot ERA vs. Salary
ggplot(Free.Agency.ERA, aes(x = ERA, y = contract_value)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: ERA V. Free Agent Salary", x = "ERA", y = "Salary")
# Scatter Plot ERA vs. Contract Length
ggplot(Free.Agency.ERA, aes(x = ERA, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: ERA V. Free Agent Years", x = "ERA", y = "Years")
# Scatter Plot ERA vs. Position
ggplot(Free.Agency.ERA, aes(x = ERA, y = POS.)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: ERA V. Free Agent Position", x = "ERA", y = "Position")
# Scatter Plot ERA vs. Position
ggplot(Free.Agency.ERA, aes(x = ERA, y = WAR)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: ERA V. Free Agent WAR", x = "ERA", y = "WAR")
# Scatter Plot ERA AGE vs. Salary
ggplot(Free.Agency.ERA, aes(x = AGE, y = contract_value)) + 
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: Age V. Free Agent Salary", x = "Age", y = "Salary")
# Scatter Plot ERA AGE vs. Contract Length
ggplot(Free.Agency.ERA, aes(x = AGE, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA: Age V. Free Agent Years", x = "Age", y = "Years")

# Scatter Plot OPS vs. Salary
ggplot(Free.Agency.OPS, aes(x = OPS, y = contract_value)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: OPS V. Free Agent Salary", x = "OPS", y = "Salary")
# Scatter Plot OPS vs. Contract Length
ggplot(Free.Agency.OPS, aes(x = OPS, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: OPS V. Free Agent Years", x = "OPS", y = "Years")
# Scatter Plot OPS vs. Position
ggplot(Free.Agency.OPS, aes(x = OPS, y = POS.)) + 
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: OPS V. Free Agent Position", x = "OPS", y = "Position")
# Scatter Plot OPS vs. WAR
ggplot(Free.Agency.OPS, aes(x = OPS, y = WAR)) + 
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: OPS V. Free Agent WAR", x = "OPS", y = "WAR")
# Scatter Plot OPS AGE vs. Salary
ggplot(Free.Agency.OPS, aes(x = AGE, y = contract_value)) + 
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: Age V. Free Agent Salary", x = "Age", y = "Salary")
# Scatter Plot OPS AGE vs. Contract Length
ggplot(Free.Agency.OPS, aes(x = AGE, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS: Age V. Free Agent Years", x = "Age", y = "Years")

# Scatter Plot WAR vs. Salary
ggplot(Free.Agency.2022, aes(x = WAR, y = contract_value)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "WAR V. Free Agent Salary", x = "WAR", y = "Salary")
# Scatter Plot WAR vs. Contract Length
ggplot(Free.Agency.2022, aes(x = WAR, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "WAR V. Free Agent Years", x = "WAR", y = "Years")
# Scatter Plot WAR vs. Position
ggplot(Free.Agency.2022, aes(x = WAR, y = POS.)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "WAR V. Free Agent Position", x = "WAR", y = "Position")

# Scatter Plot Age vs. Salary
ggplot(Free.Agency.WAR, aes(x = AGE, y = contract_value)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "WAR: Age V. Free Agent Salary", x = "Age", y = "Salary")
# Scatter Plot Age vs. Contract Length
ggplot(Free.Agency.WAR, aes(x = AGE, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "WAR: Age V. Free Agent Years", x = "Age", y = "Years")
# Scatter Plot Age vs. Position
ggplot(Free.Agency.WAR, aes(x = AGE, y = POS.)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "WAR: Age V. Free Agent Position", x = "Age", y = "Position")


# Creating a Correlation Matrix
#columns pulled out with numeric values
#correlation_matrix <- cor(Free.Agency.2022[, c("AGE", "dummy_Years", "PosNumber", "contract_value", "H", "RBI", "HR", "AVG", "OPS", "IP", "ERA", "WHIP", "W", "SV", "WAR")])
#corrplot(correlation_matrix, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black")

#ERA 2022
correlation_matrix_ERA2022 <- cor(Free.Agency.2022.ERA[, c("AGE", "YRS", "contract_value", "ERA", "IP", "WHIP","W", "SV", "WAR","PosNumber")])
corrplot(correlation_matrix_ERA2022, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - ERA 2022")
#OPS 2022
correlation_matrix_OPS2022 <- cor(Free.Agency.2022.OPS[, c("AGE", "YRS", "contract_value", "OPS", "WAR", "dummy_Pos", "H", "RBI", "HR", "AVG", "PosNumber")])
corrplot(correlation_matrix_OPS2022, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - OPS 2022")
#WAR 2022
correlation_matrix_WAR2022 <- cor(Free.Agency.2022.WAR[, C("AGE", "YRS", "contract_value", "dummy_Pos")])
corrplot(correlation_matrix_WAR2022, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - WAR 2022")

#ERA 2023
correlation_matrix_ERA2023 <- cor(Free.Agency.2023.ERA[, c("AGE", "YRS", "contract_value", "ERA", "IP", "WHIP","W", "SV", "WAR","PosNumber")])
corrplot(correlation_matrix_ERA2023, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - ERA 2023")
#OPS 2023
correlation_matrix_OPS2023 <- cor(Free.Agency.2023.OPS[, c("AGE", "YRS", "contract_value", "OPS", "WAR", "dummy_Pos", "H", "RBI", "HR", "AVG","PosNumber")])
corrplot(correlation_matrix_OPS2023, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - OPS 2023")
#WAR 2023
correlation_matrix_WAR2023 <- cor(Free.Agency.2023.WAR[, C("AGE", "YRS", "contract_value", "dummy_Pos")])
corrplot(correlation_matrix_WAR2023, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - WAR 2023")

#ERA Total
correlation_matrix_ERA <- cor(Free.Agency.ERA[, c("AGE", "YRS", "contract_value", "ERA", "IP", "WHIP","W", "SV", "WAR", "PosNumber")])
corrplot(correlation_matrix_ERA, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - ERA")
#OPS Total
correlation_matrix_OPS <- cor(Free.Agency.OPS[, c("AGE", "YRS", "contract_value", "OPS", "WAR", "dummy_Pos", "H", "RBI", "HR", "AVG", "PosNumber")])
corrplot(correlation_matrix_OPS, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - OPS")
# WAR Total


# Regressions 

# Multiple Linear Regression
model_ERA_multiLinear <- lm(contract_value ~ ERA + WAR + IP + WHIP + W + SV, data = Free.Agency.ERA)
summary(model_ERA_multiLinear)

predictions_ERA <- predict(model_ERA_multiLinear, newdata = Free.Agency.ERA)
plot(model_ERA_multiLinear, which = 1)

model_OPS_multiLinear <- lm(contract_value ~ OPS + WAR + H + RBI + HR + AVG, data = Free.Agency.OPS)
summary(model_OPS_multiLinear)

predictions_OPS <- predict(model_OPS_multiLinear, newdata = Free.Agency.OPS)
plot(model_OPS_multiLinear, which = 1)

# Stepwise Multinomial Logistic Regression 
Free.Agency.WAR1 <- Free.Agency.WAR[complete.cases(Free.Agency.WAR), ]
model_WAR_stepwise <- multinom(WAR ~ PosNumber, data = Free.Agency.WAR1)
step_model_WAR <- step(model_WAR_stepwise)
summary(step_model_WAR)

