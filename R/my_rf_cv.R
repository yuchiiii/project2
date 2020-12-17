#' Random Forest Cross-Validation function
#'
#' This function performs random Forest Cross-Validation.
#'
#' @param k Number of folds.
#' @keywords prediction
#' @return A numeric with the cross-validation error.
#'
#' @examples
#' penguins_df <- my_penguins
#' penguins_df <- na.omit(penguins_df)
#' my_rf_cv(5)
#' @export

my_rf_cv <- function(k) {
  # Empty vector to store the MSE.
  MSE <- vector()
  penguins_df <- my_penguins
  penguins_df <- na.omit(penguins_df)
  # Split data randomly.
  fold <- sample(rep(1:k, length = nrow(penguins_df)))

  # Do random forest cross-validation and calculate the MSE.
  for (i in 1:k) {
    data_train <- dplyr::filter(penguins_df, fold != i)
    data_train <- dplyr::select(data_train, bill_length_mm, bill_depth_mm,flipper_length_mm, body_mass_g)
    data_test <- dplyr::filter(penguins_df,fold == i)
    data_test <- dplyr::select(data_test, bill_length_mm, bill_depth_mm, flipper_length_mm)
    mass <- penguins_df$body_mass_g
    mass_test <- mass[fold == i]

    # Train the model.
    model_rf <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_train, ntree = 100)

    # Make predictions.
    prediction <- predict(model_rf, data_test)

    # Calculate  and store MSE.
    MSE[i] <- mean((prediction - mass_test)^2)

    # Evaluate the average MSE.
    result <- mean(MSE)
    return(result)
  }
}
