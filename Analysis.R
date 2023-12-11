# Stepwise Multinomial Logistic Regression

#install.packages("nnet")
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(broom)
library(corrplot)

# Fit a multinomial logistic regression model using the multinom function
model_ERA <- multinom(dummy_Pos + dummy_Years + dummy_salary + dummy_Age ~ ERA, data = Free.Agency.2022.ERA)
# Perform stepwise selection using the step function
step_model_ERA <- step(model_ERA)
# Display the final model
summary(step_model_ERA)

# Fit a multinomial logistic regression model using the multinom function
model_OPS <- multinom(dummy_Pos + dummy_Years + dummy_salary + dummy_Age ~ OPS, data = Free.Agency.2022.OPS)
# Perform stepwise selection using the step function
step_model_OPS <- step(model_OPS)
# Display the final model
summary(step_model_OPS)


# Fit a multinomial logistic regression model using the multinom function
model_WAR <- multinom(dummy_salary ~ WAR, data = Free.Agency.2022.WAR)
# Perform stepwise selection using the step function
step_model_WAR <- step(model_WAR)
# Display the final model
summary(step_model_WAR)

# Graphing Data
# Independent Variables x (ERA, OPS, WAR, Age)
# Dependent Variables y (Salary, Position, Contract Length)

# Scatter Plot ERA vs. Salary
ggplot(Free.Agency.2022, aes(x = ERA, y = contract_value)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA V. Free Agent Salary", x = "ERA", y = "Salary")
# Scatter Plot ERA vs. Contract Length
ggplot(Free.Agency.2022, aes(x = ERA, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA V. Free Agent Years", x = "ERA", y = "Years")
# Scatter Plot ERA vs. Position
ggplot(Free.Agency.2022, aes(x = ERA, y = POS.)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ERA V. Free Agent Position", x = "ERA", y = "Position")

# Scatter Plot OPS vs. Salary
ggplot(Free.Agency.2022, aes(x = OPS, y = contract_value)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS V. Free Agent Salary", x = "OPS", y = "Salary")
# Scatter Plot OPS vs. Contract Length
ggplot(Free.Agency.2022, aes(x = OPS, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS V. Free Agent Years", x = "OPS", y = "Years")
# Scatter Plot OPS vs. Position
ggplot(Free.Agency.2022, aes(x = OPS, y = POS.)) + 
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPS V. Free Agent Position", x = "OPS", y = "Position")

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
ggplot(Free.Agency.2022, aes(x = AGE, y = contract_value)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Age V. Free Agent Salary", x = "Age", y = "Salary")
# Scatter Plot Age vs. Contract Length
ggplot(Free.Agency.2022, aes(x = AGE, y = YRS)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Age V. Free Agent Years", x = "Age", y = "Years")
# Scatter Plot Age vs. Position
ggplot(Free.Agency.2022, aes(x = AGE, y = POS.)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Age V. Free Agent Position", x = "Age", y = "Position")


# Creating a Correlation Matrix
#columns pulled out with numeric values
#correlation_matrix <- cor(Free.Agency.2022[, c("AGE", "dummy_Years", "PosNumber", "contract_value", "H", "RBI", "HR", "AVG", "OPS", "IP", "ERA", "WHIP", "W", "SV", "WAR")])
#corrplot(correlation_matrix, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black")

correlation_matrix_ERA2022 <- cor(Free.Agency.2022.ERA[, c("AGE", "YRS", "contract_value", "ERA", "IP", "WHIP","W", "SV", "WAR","PosNumber")])
corrplot(correlation_matrix_ERA2022, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - ERA 2022")

correlation_matrix_OPS2022 <- cor(Free.Agency.2022.OPS[, c("AGE", "YRS", "contract_value", "OPS", "WAR", "dummy_Pos", "H", "RBI", "HR", "AVG", "PosNumber")])
corrplot(correlation_matrix_OPS2022, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - OPS 2022")

correlation_matrix_WAR2022 <- cor(Free.Agency.2022.WAR[, c("AGE", "YRS", "contract_value", "WAR", "dummy_Pos","PosNumber")])
corrplot(correlation_matrix_WAR2022, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black")

correlation_matrix_ERA2023 <- cor(Free.Agency.2023.ERA[, c("AGE", "YRS", "contract_value", "ERA", "IP", "WHIP","W", "SV", "WAR","PosNumber")])
corrplot(correlation_matrix_ERA2023, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - ERA 2023")

correlation_matrix_OPS2023 <- cor(Free.Agency.2023.OPS[, c("AGE", "YRS", "contract_value", "OPS", "WAR", "dummy_Pos", "H", "RBI", "HR", "AVG","PosNumber")])
corrplot(correlation_matrix_OPS2023, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black", main="Correlation Plot - OPS 2023")

correlation_matrix_WAR2023 <- cor(Free.Agency.2023.WAR[, c("AGE", "YRS", "contract_value", "WAR", "dummy_Pos","PosNumber")])
corrplot(correlation_matrix_WAR2023, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black")

