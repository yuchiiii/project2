#' Linear model function
#'
#' This function fits a linear model.
#'
#' @param formula A \code{formula} class object, similar to \code{lm()}.
#' @param data An input data frame.
#' @keywords inference
#' @return A table with rows for each coefficient and columns for the \code{Estimate},
#'    \code{Std. Error}, \code{t value}, and \code{Pr(>|t|)}.
#' @examples
#' test <- my_gapminder
#' test_formula <- lifeExp ~ gdpPercap + continent
#' my_lm(test_formula, test)
#'
#' @export

my_lm <- function(formula, data) {
  x <- model.matrix(formula, data)
  frame <- model.frame(formula, data)
  y <- model.response(frame)

  # Solve the linear coefficients.
  beta <- solve(t(x) %*% x, t(x) %*% y)

  # Estimate sigma^2.
  df <- nrow(data) - nrow(beta)
  sigma_2 <- sum((y - x %*% beta)^2 / df)

  # Estimate the standard error for the coefficients.
  se_mid <- sigma_2 * solve(t(x)  %*% x)
  se <- sqrt(diag(se_mid))

  # Estimate the t-value and correspondent p-value.
  t_obs <- (beta - 0) / se
  p_value <- 2 * pt(abs(t_obs), df, lower.tail = FALSE)

  # Create a data frame to save the results.
  result <- data.frame("Estimate" = beta,
                       "Std.Error" = se,
                       "t_value" = t_obs,
                       "Pr(>|t|)" = p_value,
                       check.names=FALSE)
  return(result)
}
