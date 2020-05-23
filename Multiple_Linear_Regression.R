# Load the pre-processed Irish housing statistical data
housing_data <- read.csv("./Data/HousingStatsProcessed.csv")
# Filter out non-numeric data
analyze_data <- housing_data[, c("TownSize", "PersonsPerSqKm", "TotalPopulation", "AllHouseholds", 
                                 "Household_Income", "Avg_Rent", "RentIncomeRatio", "PeoplePerHouse")]

# Pull the rent to income ratio apart to obtain the most expensive quarter of rental properties
rentIncomeCharacteristics <- summary(analyze_data$RentIncomeRatio)
analyze_expensive_areas = analyze_data[analyze_data$RentIncomeRatio > rentIncomeCharacteristics['3rd Qu.'],]
analyze_rest_of_ire = analyze_data[analyze_data$RentIncomeRatio < rentIncomeCharacteristics['3rd Qu.'],]

library(car)
# Scatterplot for all variables for all of Ireland
scatterplotMatrix(analyze_data[, c("PersonsPerSqKm", "TotalPopulation", "Household_Income", 
                                   "Avg_Rent", "PeoplePerHouse", "RentIncomeRatio")])

# Scatterplot for all variables for the most expensive accomodation (3rd Quartile of rent to income)
scatterplotMatrix(analyze_expensive_areas[, c("PersonsPerSqKm", "TotalPopulation", "Household_Income", 
                                              "Avg_Rent", "PeoplePerHouse", "RentIncomeRatio")])


# ALternative view of similar data - plots of the distributions of the various variables used in the
# analysis

#install.packages("e1071")
library(e1071)
# View outcome variable distribution
par(mfrow = c(1, 1))
plot(density(analyze_data$PeoplePerHouse), main = "Density Plot: Household Density", ylab = "Frequency", 
     sub = paste(" Skewness:", round(e1071::skewness(analyze_data$PeoplePerHouse), 2)))
polygon(density(analyze_data$PeoplePerHouse), col = "red")

# divide graph area in 2 x 2 grid
# Plot predictor variables
par(mfrow = c(2, 2))
plot(density(analyze_data$RentIncomeRatio), main = "Density Plot: Rent to Income Ratio", ylab = "Frequency", 
     sub = paste(" Skewness:", round(e1071::skewness(analyze_data$RentIncomeRatio), 2)))
polygon(density(analyze_data$RentIncomeRatio), col = "blue")

plot(density(analyze_data$Household_Income), main = "Density Plot: Household Income", ylab = "Frequency", 
     sub = paste(" Skewness:", round(e1071::skewness(analyze_data$Household_Income), 2)))
polygon(density(analyze_data$Household_Income), col = "green")

plot(density(analyze_data$PersonsPerSqKm), main = "Density Plot: People per Square km", ylab = "Frequency", 
     sub = paste(" Skewness:", round(e1071::skewness(analyze_data$PersonsPerSqKm), 2)))
polygon(density(analyze_data$PersonsPerSqKm), col = "aliceblue")

plot(density(analyze_data$Avg_Rent), main = "Density Plot: Average Rent", ylab = "Frequency", 
     sub = paste(" Skewness:", round(e1071::skewness(analyze_data$Avg_Rent), 2)))
polygon(density(analyze_data$Avg_Rent), col = "gold")

# Inspect correlation of the various variables
cor(analyze_data$PeoplePerHouse, analyze_data$RentIncomeRatio)
cor(analyze_data$PeoplePerHouse, analyze_data$Household_Income)
cor(analyze_data$PeoplePerHouse, analyze_data$PersonsPerSqKm)
cor(analyze_data$PeoplePerHouse, analyze_data$Avg_Rent)

# Split the data into a train set (80 % of the data), and a test set (20% of the data)
sampled_records <- sample(1:nrow(analyze_data), 0.8 * nrow (analyze_data))
train_data <- analyze_data[sampled_records,]
test_data <- analyze_data[-sampled_records,]


uni_linear_model1 <- lm(PeoplePerHouse ~ PersonsPerSqKm, data=train_data)
uni_linear_model2 <- lm(PeoplePerHouse ~ Avg_Rent, data=train_data)
multi_linear_model1 <- lm(PeoplePerHouse ~ PersonsPerSqKm + Household_Income:Avg_Rent, 
                          data=train_data)
multi_linear_model2 <- lm(PeoplePerHouse ~ PersonsPerSqKm + Household_Income + 
                                  Household_Income:Avg_Rent, data=train_data)
multi_linear_model3 <- lm(PeoplePerHouse ~ PersonsPerSqKm + Household_Income + Avg_Rent + 
                                  Household_Income:Avg_Rent, data=train_data)
multi_linear_model4 <- lm(PeoplePerHouse ~ PersonsPerSqKm + Household_Income + Avg_Rent + 
                                  (Avg_Rent ^ 2) + Household_Income:Avg_Rent, data=train_data)
multi_linear_model5 <- lm(PeoplePerHouse ~ PersonsPerSqKm + Household_Income + Avg_Rent + 
                                  log(Avg_Rent) + Household_Income:Avg_Rent, data=train_data)

# Use the broom package to import the glance function, which is a more convenient way to obtain
# stats from the model summary
require(broom)

m1 <- glance(summary(uni_linear_model1))
m2 <- glance(summary(uni_linear_model2))
m3 <- glance(summary(multi_linear_model1))
m4 <- glance(summary(multi_linear_model2))
m5 <- glance(summary(multi_linear_model3))
m6 <- glance(summary(multi_linear_model4))
m7 <- glance(summary(multi_linear_model5))

formulas <- c("PeoplePerHouse ~ PersonsPerSqKm",
              "PeoplePerHouse ~ Avg_Rent",
              "PeoplePerHouse ~ PersonsPerSqKm + Household_Income:Avg_Rent", 
              "PeoplePerHouse ~ PersonsPerSqKm + Household_Income + Household_Income:Avg_Rent", 
              "PeoplePerHouse ~ PersonsPerSqKm + Household_Income + Avg_Rent + Household_Income:Avg_Rent",
              "PeoplePerHouse ~ PersonsPerSqKm + Household_Income + Avg_Rent + (Avg_Rent ^ 2) + Household_Income:Avg_Rent", 
              "PeoplePerHouse ~ PersonsPerSqKm + Household_Income + Avg_Rent + log(Avg_Rent) + Household_Income:Avg_Rent")
        
adj_r_squared <- c(m1$adj.r.squared, m2$adj.r.squared, m3$adj.r.squared, m4$adj.r.squared, m5$adj.r.squared,
                   m6$adj.r.squared, m7$adj.r.squared)        

Akaike_info_crit <- c(AIC(uni_linear_model1), AIC(uni_linear_model2), AIC(multi_linear_model1), 
                      AIC(multi_linear_model2), AIC(multi_linear_model3), AIC(multi_linear_model4), 
                      AIC(multi_linear_model5))
Bayesian_info_crit <- c(BIC(uni_linear_model1), BIC(uni_linear_model2), BIC(multi_linear_model1), 
                        BIC(multi_linear_model2), BIC(multi_linear_model3), BIC(multi_linear_model4), 
                        BIC(multi_linear_model5))


model_stats_comparison <- data.frame(formulas, adj_r_squared, Akaike_info_crit, 
                                     Bayesian_info_crit)

# Print best model stats
summary(multi_linear_model3)

best_model <- multi_linear_model3
# Plot the model residuals
plot(best_model)

#install.packages("effects")
library(effects)

# Plot the effect of the individual variables
plot(effect("PersonsPerSqKm", best_model), multiline=TRUE)
plot(effect("Household_Income", best_model), multiline=TRUE)
plot(effect("Avg_Rent", best_model), multiline=TRUE)
plot(effect("Household_Income:Avg_Rent", best_model), multiline=TRUE)


# Predict the household dendity for the test dataframe
density_predicted <- predict(best_model, test_data)
# Bind the actual and predicted values into a dataframe
actuals_preds <- data.frame(cbind(actuals = test_data$PeoplePerHouse, predicted = density_predicted))
# Calculate the correlation between the actual and predicted values
correlation_accuracy <- cor(actuals_preds)
# Display the correlation
correlation_accuracy

# Calculate min/max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# Calculate Mean Absolute Percentage Error
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape


#install.packages("DAAG")
library(DAAG)

par(mfrow = c(1, 1))
cvResults <- suppressWarnings(CVlm(data = analyze_data, form.lm = PeoplePerHouse ~ PersonsPerSqKm + 
                                           Household_Income + Avg_Rent + Household_Income:Avg_Rent, 
                                   m = 5, dots = FALSE , seed = 29, legend.pos = "topleft",
                                   printit = FALSE, main = "Cross-validated Chart"))


confint(best_model)

qqPlot(best_model,  id.method="identify", simulate=TRUE, main="Q-Q Plot")


# Model testing
row_names <- c("Tiny Town", "Letterkenny", "City - low income", "City - high income")
PersonsPerSqKm <- c(10, 1000, 2000, 2000)
Household_Income <- c(30000, 40000, 40000, 100000)
Avg_Rent <- c(400, 650, 1700, 1700)
test_df <- data.frame(row_names, PersonsPerSqKm, Household_Income, Avg_Rent)
test_df$house_density <- predict(best_model, test_df)

test_df
