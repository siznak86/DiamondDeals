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
correlation_matrix_Total <- cor(Free.Agency.2022_omit2[, c("AGE", "YRS", "contract_value", "ERA", "OPS", "WAR", "dummy_Pos" )])
corrplot(correlation_matrix_Total, method = "color", is.corr = TRUE, addCoef.col = "black")
# Now do for ERA and OPS
correlation_matrix_ERA <- cor(Free.Agency.2022.ERA[, c("AGE", "YRS", "contract_value", "ERA", "WAR")])
corrplot(correlation_matrix_ERA, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black")

correlation_matrix_OPS <- cor(Free.Agency.2022.OPS[, c("AGE", "YRS", "contract_value", "OPS", "WAR", "dummy_Pos")])
corrplot(correlation_matrix_OPS, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black")

correlation_matrix_WAR <- cor(Free.Agency.2022.WAR[, c("AGE", "YRS", "contract_value", "WAR", "dummy_Pos")])
corrplot(correlation_matrix_WAR, method = "color", type = "full", tl.col = "black", tl.srt = 45, is.corr = TRUE, addCoef.col = "black")
