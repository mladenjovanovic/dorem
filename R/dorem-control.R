#' Constructor for the \code{dorem} control object
#'
#' @param weights Will be explained
#' @param na.rm Will be explained
#' @param loss_func Will be explained
#' @param link_func Will be explained
#' @param perf_func Will be explained
#' @param optim_method Will be explained
#' @param optim_maxit Will be explained
#' @param optim_trace Will be explained
#' @param coefs_start Will be explained
#' @param coefs_lower Will be explained
#' @param coefs_upper Will be explained
#' @param cv_repeats Will be explained
#' @param cv_folds Will be explained
#' @param shuffle Will be explained
#' @param iter Will be explained
#' @param seed Will be explained
#' @export
dorem_control <- function(weights = NULL,
                          na.rm = TRUE,
                          loss_func = function(obs, pred, weights, na.rm) {
                            mean(weights * ((pred - obs))^2, na.rm = na.rm)
                          },
                          link_func = function(x) {
                            x
                          },
                          perf_func = function(obs, pred, na.rm) {
                            meanDiff <- mean(pred - obs, na.rm = na.rm)
                            SDdiff <- stats::sd(pred - obs, na.rm = na.rm)
                            RMSE <- sqrt(mean((pred - obs)^2, na.rm = na.rm))
                            MAE <- mean(abs(pred - obs), na.rm = na.rm)
                            minErr <- min((pred - obs), na.rm = na.rm)
                            maxErr <- max((pred - obs), na.rm = na.rm)
                            MAPE <- 100 * mean(abs((pred - obs) / obs), na.rm = na.rm)
                            R_squared <- stats::summary.lm(stats::lm(pred ~ obs))$r.squared

                            performance <- list(
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
                          },
                          optim_method = valid_optimization_methods(),
                          optim_maxit = 1000,
                          optim_VTR = -Inf,
                          optim_trace = FALSE,

                          coefs_start = NULL,
                          coefs_lower = NULL,
                          coefs_upper = NULL,

                          cv_repeats = NULL,
                          cv_folds = NULL,

                          shuffle = FALSE,

                          iter = FALSE,
                          seed = round(stats::runif(1, 1, 10000), 0)) {

  # Check if appropriate optim method is provided
  optim_method <- optim_method[1]
  rlang::arg_match(optim_method, valid_optimization_methods())

  list(
    weights = weights,
    na.rm = na.rm,
    loss_func = loss_func,
    link_func = link_func,
    perf_func = perf_func,
    optim_method = optim_method,
    optim_maxit = optim_maxit,
    optim_VTR = optim_VTR,
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
  c("L-BFGS-B", "DE", "CMA-ES")
}
