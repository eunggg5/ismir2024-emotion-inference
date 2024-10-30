################################################################################################################
# discrete_cross_validation.R: Leave-one-song-out approach for cross-validation of discrete LME models
################################################################################################################

# Define a function for leave-one-out cross-validation (LOO-CV)
leave_one_out_cv <- function(data, model_formula1, model_formula2, id_col, response_var) {
    unique_ids <- unique(data[[id_col]])
    results <- data.frame(
        idstimuli = character(),
        model1_mae = numeric(),
        model2_mae = numeric(),
        model1_mse = numeric(),
        model2_mse = numeric(),
        model1_rmse = numeric(),
        model2_rmse = numeric(),
        model1_mape = numeric(),
        model2_mape = numeric(),
        stringsAsFactors = FALSE
    )

    # Loop through each unique ID for leave-one-out
    for (leave_out_id in unique_ids) {
        # Split the data into training (without the test ID) and test sets (only the test ID)
        train_data <- data %>% filter(data[[id_col]] != leave_out_id)
        test_data <- data %>% filter(data[[id_col]] == leave_out_id)

        # Fit models to the training data
        model1 <- lmer(model_formula1, data = train_data)
        model2 <- lmer(model_formula2, data = train_data)

        # Generate predictions for the left-out test data
        predictions_model1 <- predict(model1, newdata = test_data, allow.new.levels = TRUE)
        predictions_model2 <- predict(model2, newdata = test_data, allow.new.levels = TRUE)

        # Actual observed values in test set
        actuals <- test_data[[response_var]]

        # Calculate performance metrics: MAE, MSE, RMSE, and MAPE
        model1_mae <- mae(actuals, predictions_model1)
        model2_mae <- mae(actuals, predictions_model2)

        model1_mse <- mse(actuals, predictions_model1)
        model2_mse <- mse(actuals, predictions_model2)

        model1_rmse <- rmse(actuals, predictions_model1)
        model2_rmse <- rmse(actuals, predictions_model2)

        model1_mape <- mape(actuals, predictions_model1)
        model2_mape <- mape(actuals, predictions_model2)

        # Store the results for this test ID
        results <- rbind(results, data.frame(
            idstimuli = leave_out_id,
            model1_mae = model1_mae,
            model2_mae = model2_mae,
            model1_mse = model1_mse,
            model2_mse = model2_mse,
            model1_rmse = model1_rmse,
            model2_rmse = model2_rmse,
            model1_mape = model1_mape,
            model2_mape = model2_mape,
            stringsAsFactors = FALSE
        ))
    }
    return(results)
}

# Define model formulas
model_formula1 <- observer ~ rms + flatness + zerocross + centroid + (1 | idob)
model_formula2 <- observer ~ arousal_d + valence_d + dominance_d + rms + flatness + zerocross + centroid + (1 | idob)

# Run leave-one-out cross-validation
results <- leave_one_out_cv(df_ob_dis, model_formula1, model_formula2, "idstimuli", "observer")

# Display cross-validation results
print(results)

# Compare models using Wilcoxon signed-rank test for paired data
wilcox_test_results <- list(
    mae = wilcox.test(results$model1_mae, results$model2_mae, paired = TRUE),
    mse = wilcox.test(results$model1_mse, results$model2_mse, paired = TRUE),
    rmse = wilcox.test(results$model1_rmse, results$model2_rmse, paired = TRUE),
    mape = wilcox.test(results$model1_mape, results$model2_mape, paired = TRUE)
)

# Calculate the mean performance metrics for each model
mean_results <- results %>%
    summarise(
        mean_model1_mae = mean(model1_mae),
        mean_model2_mae = mean(model2_mae),
        mean_model1_mse = mean(model1_mse),
        mean_model2_mse = mean(model2_mse),
        mean_model1_rmse = mean(model1_rmse),
        mean_model2_rmse = mean(model2_rmse),
        mean_model1_mape = mean(model1_mape),
        mean_model2_mape = mean(model2_mape)
    )

# Summarize results for better readability
results_summary <- data.frame(
    Metric = c("MAE", "MSE", "RMSE", "MAPE"),
    Model1 = c(mean_results$mean_model1_mae, mean_results$mean_model1_mse, mean_results$mean_model1_rmse, mean_results$mean_model1_mape),
    Model2 = c(mean_results$mean_model2_mae, mean_results$mean_model2_mse, mean_results$mean_model2_rmse, mean_results$mean_model2_mape),
    PValue = c(wilcox_test_results$mae$p.value, wilcox_test_results$mse$p.value, wilcox_test_results$rmse$p.value, wilcox_test_results$mape$p.value)
)

# Display summarized results with Wilcoxon test p-values
print("Wilcoxon signed-rank test results with means:")
print(results_summary)

################################################################################################################

# Confirmation message
print("Leave-one-song-out cross-validation of discrete LME models complete.")

################################################################################################################
