#' Constructor for the \code{dorem} control object
#'
#' @param weights Default is NULL
#' @param na.rm Default is TRUE
#' @param loss_func Function used in optimization objective function. Default is \code{\link{dorem_loss_func}}
#'     which returns MSE
#' @param link_func Function used to convert predictions. Default is \code{\link{dorem_no_link_func}}
#'     which returns the original value
#' @param perf_func Function used to quantify model fit. Default is \code{\link{dorem_perf_func}}
#' @param optim_method Optimization method. The list of supported methods is \code{c("L-BFGS-B", "DE", "CMA-ES", "gridSearch")}
#' @param optim_maxit Maximal number of iterations for the optimization method. Default is 1000
#' @param optim_VTR Value To Reach in the optimization method. Default is -Inf
#' @param optim_grid_n Number of search values for each predictor used in gridSearch method. Default is 5
#' @param optim_trace Should optimization trace be shown? Default is FALSE
#' @param coefs_start Starting values for coefficients
#' @param coefs_lower Lower bound for coefficients
#' @param coefs_upper Upper bound for coefficients
#' @param cv_repeats Number of CV repeats
#' @param cv_folds Number of CV folds
#' @param shuffle Should shuffle be performed? Default is FALSE
#' @param iter Should iter be shown? Default is TRUE
#' @param seed Random number seed
#' @export
#' @examples
#' data("bike_score")
#'
#' banister_model <- dorem(
#'   Test_5min_Power ~ BikeScore,
#'   bike_score,
#'   method = "banister",
#'   control = dorem_control(
#'         cv_folds = 3,
#'         cv_repeats = 5,
#'         shuffle = TRUE
#'   )
#' )
dorem_control <- function(weights = NULL,
                          na.rm = TRUE,
                          loss_func = dorem_loss_func,
                          link_func = dorem_no_link_func,
                          perf_func = dorem_perf_func,
                          optim_method = valid_optimization_methods(),
                          optim_maxit = 1000,
                          optim_VTR = -Inf,
                          optim_grid_n = 5,
                          optim_trace = FALSE,

                          coefs_start = NULL,
                          coefs_lower = NULL,
                          coefs_upper = NULL,

                          cv_repeats = NULL,
                          cv_folds = NULL,

                          shuffle = FALSE,

                          iter = TRUE,
                          seed = NULL) {

  # Check if appropriate optim method is provided
  optim_method <- optim_method[1]
  rlang::arg_match(optim_method, valid_optimization_methods())

  # Create seed
  if(is.null(seed)) {
    seed <- round(stats::runif(1, 1, 10000), 0)
  }

  list(
    weights = weights,
    na.rm = na.rm,
    loss_func = loss_func,
    link_func = link_func,
    perf_func = perf_func,
    optim_method = optim_method,
    optim_maxit = optim_maxit,
    optim_VTR = optim_VTR,
    optim_grid_n = optim_grid_n,
    optim_trace = optim_trace,
    coefs_start = coefs_start,
    coefs_lower = coefs_lower,
    coefs_upper = coefs_upper,
    cv_repeats = cv_repeats,
    cv_folds = cv_folds,
    shuffle = shuffle,
    iter = iter,
    seed = seed
  )
}

# ------------------------------------------------------------------------------
# All valid optimization methods
valid_optimization_methods <- function() {
  c("L-BFGS-B", "DE", "CMA-ES", "gridSearch")
}

# ------------------------------------------------------------------------------
#' Default loss function
#'
#' This function is used inside the objective function to return model fit.
#'     Default loss function is MSE.
#'
#' @param obs Numeric vector. Observed values
#' @param pred Numeric vector. Predicted values
#' @param weights Numeric vector. Weights for weighting residuals
#' @param na.rm Logical
#' @return Mean Squared Error (MSE)
#' @export
#' @examples
#' dorem_loss_func(seq(1:10), 5, rep(1, 10), na.rm = TRUE)
dorem_loss_func <- function(obs, pred, weights, na.rm) {
  mean((weights * (pred - obs))^2, na.rm = na.rm)
}


# --------------------------------------------------------------------------------
#' Default performance function
#'
#' List of estimators used to quantify model fit
#' @param obs Numeric vector. Observed values
#' @param pred Numeric vector. Predicted values
#' @param na.rm Logical
#' @return Named vector
#' @export
#' @examples
#' dorem_perf_func(rnorm(10), rnorm(10), na.rm = TRUE)
dorem_perf_func <- function(obs, pred, na.rm) {
  if (na.rm == TRUE) {
    N <- length(stats::na.omit(pred - obs))
  } else {
    N <- length(pred - obs)
  }

  meanDiff <- mean(pred - obs, na.rm = na.rm)
  SDdiff <- stats::sd(pred - obs, na.rm = na.rm)
  RMSE <- sqrt(mean((pred - obs)^2, na.rm = na.rm))
  MAE <- mean(abs(pred - obs), na.rm = na.rm)
  minErr <- min((pred - obs), na.rm = na.rm)
  maxErr <- max((pred - obs), na.rm = na.rm)
  MAPE <- 100 * mean(abs((pred - obs) / obs), na.rm = na.rm)
  R_squared <- stats::summary.lm(stats::lm(pred ~ obs))$r.squared

  performance <- list(
    N = N,
    meanDiff = meanDiff,
    SDdiff = SDdiff,
    RMSE = RMSE,
    MAE = MAE,
    minErr = minErr,
    maxErr = maxErr,
    MAPE = MAPE,
    R_squared = R_squared
  )

  unlist(performance)
}

# -------------------------------------------------------------
#' Default link function
#'
#' By default there is no link function.
#'
#' @param x Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' dorem_no_link_func(1:10)
dorem_no_link_func <- function(x) {
  x
}
