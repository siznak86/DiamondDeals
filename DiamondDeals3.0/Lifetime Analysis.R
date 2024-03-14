library(ggplot2)

# Linear Regression
model_Linear_Batters <- step(lm(AvgSalary ~ . -Player -PlayerID -Years -Dollars -Season -game_normal_minmax_salary -game_normal_z_salary, data = finalBatting))
summary(model_Linear_Batters)
# R-Squared = .6755 = 67.55%

model_Linear_Pitchers <- step(lm(AvgSalary ~ . -Player -PlayerID -Years -Dollars -Season, data = finalPitching))
summary(model_Linear_Pitchers)
# R-Squared = .4981 = 49.81%

# Prediction
Batting$LinearPredictedSalary <- abs(-18991083 + 8308623*finalBatting$WAR - 35181*finalBatting$G + 41986*finalBatting$R - 92191*finalBatting$H + 67825*finalBatting$X1B + 75033*finalBatting$X2B + 113032*finalBatting$X3B + 54714*finalBatting$RBI + 61793742*finalBatting$BA + 125680*finalBatting$OPS. - 236298*finalBatting$SF - 14039632*finalBatting$WAA - 6598334*finalBatting$oWAR + 13153636*finalBatting$dWAR + 1279545*finalBatting$Rbat - 239726*finalBatting$Rbaser + 1387029*finalBatting$Rbaser...Rdp - 749272*finalBatting$Rfield)

Pitching$LinearPredictedSalary <- abs(27844581 - 15738*finalPitching$G - 471595*finalPitching$CG - 38125*finalPitching$H + 93765*finalPitching$ER - 5293847*finalPitching$FIP + 755554*finalPitching$WAA)

# Prediction graphed
plot(Batting$AvgSalary, type = "l", col = "blue", lwd = 2, xlab = "", ylab = "Salary", main = "Predicted Linear and Actual Batting")
lines(Batting$LinearPredictedSalary, col = "red", lwd = 2)

plot(Pitching$AvgSalary, type = "l", col = "blue", lwd = 2, xlab = "", ylab = "Salary", main = "Predicted Linear and Actual Pitching")
lines(Pitching$LinearPredictedSalary, col = "red", lwd = 2)

# Percent Change = ((Predicted - AVG.SALARY)/AVG. SALARY)
Batting$Linear_Model_Error <- abs((Batting$LinearPredictedSalary - Batting$AvgSalary)/(Batting$AvgSalary))
Pitching$Linear_Model_Error <- abs((Pitching$LinearPredictedSalary - Pitching$AvgSalary)/(Pitching$AvgSalary))
# Mean Error
mean(Batting$Linear_Model_Error)
# 1.363279 = 136.33%
mean(Pitching$Linear_Model_Error)
# 1.173528 = 117.35%

# Percent Difference = |Predicted – Observed|/[(Predicted + Observed)/2]
Batting$Linear_Model_Difference <- abs(Batting$LinearPredictedSalary - Batting$AvgSalary) / ((Batting$LinearPredictedSalary + Batting$AvgSalary)/2)
Pitching$Linear_Model_Difference <- abs(Pitching$LinearPredictedSalary - Pitching$AvgSalary) / ((Pitching$LinearPredictedSalary + Pitching$AvgSalary)/2)
# Mean Difference
mean(Batting$Linear_Model_Difference)
# .5886695 = 58.87%
mean(Pitching$Linear_Model_Difference)
# .6080005 = 60.8%

# Exponential Regression
model_Exponential_Batters <- step(lm(log(AvgSalary) ~ . -Player -PlayerID -Years -Dollars -Season -game_normal_minmax_salary -game_normal_z_salary, data = finalBatting))
summary(model_Exponential_Batters)
# R-Squared = .6397 = 63.97%
model_Exponential_Pitchers <- step(lm(log(AvgSalary) ~ . -Player -PlayerID -Years -Dollars -Season, data = finalPitching))
summary(model_Exponential_Pitchers)
# R-Squared = .4907 = 49.07%

# Prediction
Batting$ExpPredictedSalary <- exp(11.489955 + 0.897636*finalBatting$WAR - 0.005398*finalBatting$G - 0.015428*finalBatting$PA + 0.016954*finalBatting$AB + 0.029435*finalBatting$X3B + 0.003880*finalBatting$RBI - 0.012874*finalBatting$CS + 0.018337*finalBatting$BB + 0.035335*finalBatting$OPS. + 0.024001*finalBatting$HBP + 0.023591*finalBatting$SH - 1.228335*finalBatting$WAA - 1.092653*finalBatting$oWAR + 1.477830*finalBatting$dWAR + 0.139464*finalBatting$Rbat + 0.141667*finalBatting$Rbaser...Rdp - 0.116644*finalBatting$Rfield)
Pitching$ExpPredictedSalary <- exp(20.375746+0.018493*finalPitching$W - 1.610045*finalPitching$W.L. + 0.005219*finalPitching$GS - 0.146600*finalPitching$CG + 0.197413*finalPitching$SHO + 0.008596*finalPitching$R + 0.009560*finalPitching$WP - 0.001291*finalPitching$BF - 0.557324*finalPitching$FIP - 29.223717*finalPitching$WHIP + 3.027459*finalPitching$H9 + 3.013013*finalPitching$BB9 + 0.042386*finalPitching$WAA)

# Prediction Graphed
plot(Batting$AvgSalary, type = "l", col = "blue", lwd = 2, xlab = "", ylab = "Salary", main = "Predicted Exponential and Actual Batting")
lines(Batting$ExpPredictedSalary, col = "red", lwd = 2)

plot(Pitching$AvgSalary, type = "l", col = "blue", lwd = 2, xlab = "", ylab = "Salary", main = "Predicted Exponential and Actual Pitching")
lines(Pitching$ExpPredictedSalary, col = "red", lwd = 2)


# Percent Change = ((Predicted - AVG.SALARY)/AVG. SALARY)
Batting$Exp_Model_Error <- abs((Batting$ExpPredictedSalary - Batting$AvgSalary)/(Batting$AvgSalary))
Pitching$Exp_Model_Error <- abs((Pitching$ExpPredictedSalary - Pitching$AvgSalary)/(Pitching$AvgSalary))
# Mean Error
mean(Batting$Exp_Model_Error)
# .6279325 = 62.8%
mean(Pitching$Exp_Model_Error)
# .7358081 = 73.58%

# Percent Difference = |Predicted – Observed|/[(Predicted + Observed)/2]
Batting$Exp_Model_Difference <- abs(Batting$ExpPredictedSalary - Batting$AvgSalary) / ((Batting$LinearPredictedSalary + Batting$AvgSalary)/2)
Pitching$Exp_Model_Difference <- abs(Pitching$ExpPredictedSalary - Pitching$AvgSalary) / ((Pitching$LinearPredictedSalary + Pitching$AvgSalary)/2)
# Mean Difference
mean(Batting$Exp_Model_Difference)
# .4242127 = 42.42%
mean(Pitching$Linear_Model_Difference)
# .6080005 = 60.8%

# Save Batters
write.csv(Batting, "Batting.csv", row.names = FALSE)
# Save Pitchers
write.csv(Pitching, "Pitching.csv", row.names = FALSE)

