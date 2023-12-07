# Stepwise Multinomial Logistic Regression

#install.packages("nnet")
library(nnet)

# Fit the initial model
#initial_model <- multinom(response ~ predictor1 + predictor2 + predictor3, data = your_data)
initial_model_position <- multinom(dummy_Pos ~ ERA + OPS + WAR, data = Free.Agency.2022)
initial_model_contract <- multinom(YRS ~ ERA + OPS + WAR, data = Free.Agency.2022)
initial_model_salary <- multinom(AVG..SALARY ~ ERA + OPS + WAR, data = Free.Agency.2022)
initial_model_age <- multinom(AGE ~ ERA + OPS + WAR, data = Free.Agency.2022)

# Perform stepwise model selection
#final_model <- step(initial_model, direction = "both")
final_model_position <- step(initial_model_position, direction = "both")
final_model_contract <- step(initial_model_contract, direction = "both")
final_model_salary <- step(initial_model_salary, direction = "both")
final_model_age <- step(initial_model_age, direction = "both")


# View the final model summary
#summary(final_model)
summary(final_model_position)
summary(final_model_contract)
summary(final_model_salary)
summary(final_model_age)



table(Free.Agency.2022$dummy_Pos)
levels(Free.Agency.2022$dummy_Pos)
Free.Agency.2022$dummy_Pos <- factor(Free.Agency.2022$dummy_Pos, levels = c("1", "2", "3", "4"))
sum(is.na(Free.Agency.2022$dummy_Pos))
