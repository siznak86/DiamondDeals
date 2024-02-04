# Linear Regression
model_Linear_Batters <- step(lm(AvgSalary ~ . -Player -PlayerID -Years -Dollars -Season, data = finalBatting))
summary(model_Linear_Batters)
# R-Squared = .6755 = 67.55%
model_Linear_Pitchers <- step(lm(AvgSalary ~ . -Player -PlayerID -Years -Dollars -Season, data = finalPitching))
summary(model_Linear_Pitchers)
# R-Squared = .4981 = 49.81%

# Prediction
Batting$LinearPredictedSalary <- abs(-66774432 + 5214866*finalBatting$WAR - 33385*finalBatting$G+151727*finalBatting$PA - 92428*finalBatting$AB - 298356*finalBatting$H + 170959*finalBatting$`1B` + 161898*finalBatting$`2B` + 194471*finalBatting$`3B` + 94387*finalBatting$RBI - 176455*finalBatting$BB + 157927237*finalBatting$OBP + 65937267*finalBatting$SLG - 165972*finalBatting$HBP - 445942*finalBatting$SF + 209887*finalBatting$IBB - 11031861*finalBatting$oWAR + 6717442*finalBatting$dWAR + 699401*finalBatting$Rbat + 696090*finalBatting$`Rbaser + Rdp` - 1235379*finalBatting$Rfield)
Pitching$LinearPredictedSalary <- abs(27844581 - 15738*finalPitching$G - 471595*finalPitching$CG - 38125*finalPitching$H + 93765*finalPitching$ER - 5293847*finalPitching$FIP + 755554*finalPitching$WAA)

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
model_Exponential_Batters <- step(lm(log(AvgSalary) ~ . -Player -PlayerID -Years -Dollars -Season, data = finalBatting))
summary(model_Exponential_Batters)
# R-Squared = .6397 = 63.97%
model_Exponential_Pitchers <- step(lm(log(AvgSalary) ~ . -Player -PlayerID -Years -Dollars -Season, data = finalPitching))
summary(model_Exponential_Pitchers)
# R-Squared = .4907 = 49.07%

# Prediction
Batting$ExpPredictedSalary <- exp(7.028902 + 0.914584*finalBatting$WAR - 0.004677*finalBatting$G + 0.004784*finalBatting$AB - 0.024887*finalBatting$H + 0.013665*finalBatting$`1B` + 0.012802*finalBatting$`2B` + 0.033728*finalBatting$`3B` + 0.007982*finalBatting$RBI + 9.588743*finalBatting$BA + 7.552162*finalBatting$OPS + 0.004256*finalBatting$HBP - 0.021950*finalBatting$SF - 1.103740*finalBatting$WAA - 1.187811*finalBatting$oWAR + 1.438514*finalBatting$dWAR + 0.145552*finalBatting$Rbat - 0.209157*finalBatting$Rdp - 0.199402*finalBatting$Rbaser + 0.339517*finalBatting$`Rbaser + Rdp` - 0.127289*finalBatting$Rfield)
Pitching$ExpPredictedSalary <- exp(20.375746+0.018493*finalPitching$W - 1.610045*finalPitching$`W-L%` + 0.005219*finalPitching$GS - 0.146600*finalPitching$CG + 0.197413*finalPitching$SHO + 0.008596*finalPitching$R + 0.009560*finalPitching$WP - 0.001291*finalPitching$BF - 0.557324*finalPitching$FIP - 29.223717*finalPitching$WHIP + 3.027459*finalPitching$H9 + 3.013013*finalPitching$BB9 + 0.042386*finalPitching$WAA)

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


