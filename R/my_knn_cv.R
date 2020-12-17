#' K-Nearest Neighbors Cross-Validation function
#'
#' This function performs k-nearest neighbors in cross-validation.
#'
#' @param train An input data frame.
#' @param cl True class value of your training data.
#' @param k_nn An integer representing the number of neighbors.
#' @param k_cv An integer representing the number of folds.
#' @keywords prediction
#' @return A list with \code{class}, a vector of the predicted class Ŷi for all
#'   observations and \code{cv_err}, a numeric with the cross-validation misclassification
#'   error.
#' @examples
#' penguins_df <- my_penguins
#' penguins_df <- na.omit(penguins_df)
#' penguins_df$species <- as.character(penguins_df$species)
#' my_knn_cv(penguins_df, penguins_df$species, 1, 5)
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Split data randomly.
  fold <- sample(rep(1:k_cv, length = nrow(train)))

  # Create a new data frame storing the folds.
  # train_new <- cbind(train, fold)

  # Empty vector to store predictions and prediction error.
  pred <- vector()
  class <- vector()

  data_train <- dplyr::select(train, bill_length_mm, bill_depth_mm,flipper_length_mm, body_mass_g)
  data_test <- dplyr::select(train, bill_length_mm, bill_depth_mm,flipper_length_mm, body_mass_g)

  # Do k-nearest neighbors cross-validation and calculate the misclassification rate for each iteration.
  for (i in 1:k_cv) {
    data_train_cv <- dplyr::filter(data_train, fold != i)
    data_test_cv <-  dplyr::filter(data_train, fold == i)
    cl_train <- cl[fold != i]
    cl_test <- cl[fold == i]
    prediction <- class::knn(data_train_cv, data_test_cv, cl_train, k = k_nn)
    prediction <- as.character(prediction)
    pred_err <- sum(cl_test != prediction) / length(cl_test)
    pred[fold == i] <- pred_err
  }
  all_pred <- class::knn(data_train, data_test, cl, k = k_nn)
  all_pred <- as.character(all_pred)
  cv_error <- mean(pred)

  # Create a list to store the cv_error and all predictions.
  result <- list("class" = all_pred, "cv_err" = cv_error)
  return(result)
}
