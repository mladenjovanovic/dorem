#' Constructor for dorem control object
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
#' @param iter Will be explained
#' @param seed Will be explained
#' @export
dorem_control <- function(loss_func = function(obs, pred, weights, na.rm = TRUE) {
                            mean(weights*((pred - obs))^2, na.rm = na.rm)
                          },
                          link_func = function(x) {x},
                          perf_func = function(obs, pred, na.rm = TRUE) {
                            list(
                              RMSE = sqrt(mean((pred - obs)^2, na.rm = na.rm)),
                              MAE =  mean(abs(pred - obs), na.rm = na.rm),
                              maxAbsErr = max(abs(pred - obs), na.rm = na.rm),
                              MAPE = 100 * mean(abs((pred - obs)/obs), na.rm = na.rm)
                            )
                          },
                          optim_method = ifelse(any(!is.null(coefs_lower), !is.null(coefs_upper)), "L-BFGS-B", "BFGS"),
                          optim_maxit = 1000,
                          optim_trace = FALSE,

                          coefs_start = NULL,
                          coefs_lower = NULL,
                          coefs_upper = NULL,

                          cv_repeats = NULL,
                          cv_folds = NULL,
                          iter = FALSE,
                          seed = round(stats::runif(1, 1, 10000))){


  list(
    loss_func = loss_func,
    link_func = link_func,
    perf_func = perf_func,
    optim_method = optim_method,
    optim_maxit = optim_maxit,
    optim_trace = optim_trace,
    coefs_start = coefs_start,
    coefs_lower = coefs_lower,
    coefs_upper = coefs_upper,
    cv_repeats = cv_repeats,
    cv_folds = cv_folds,
    iter = iter,
    seed = seed
  )

}
