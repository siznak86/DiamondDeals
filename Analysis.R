# Stepwise Multinomial Logistic Regression

#install.packages("nnet")
library(nnet)

#set.seed(123)
#your_data <- data.frame(
#  response = factor(sample(1:3, 100, replace = TRUE)),
#  predictor1 = rnorm(100),
#  predictor2 = rnorm(100),
#  predictor3 = rnorm(100)
#)

# Fit the initial model
#initial_model <- multinom(response ~ predictor1 + predictor2 + predictor3, data = your_data)

# Perform stepwise model selection
#final_model <- step(initial_model, direction = "both")

# View the final model summary
#summary(final_model)