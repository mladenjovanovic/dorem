# Function that is called for optimization
dorem_optim <- function(par, predict_func, predictors, outcome, control) {
  objective_func <- function(par, predict_func, predictors, outcome, weights, na.rm) {
    stdev <- par[[1]]
    par <- par[-1]

    stdev <- abs(stdev)

    # Get predictions
    pred <- predict_func(par, predictors)

    # Calculate log likelihood
    LL <- sum(
      stats::dnorm(
        outcome,
        mean = pred,
        sd = stdev,
        log = TRUE
      ),
      na.rm = na.rm
    )

    return(-LL)
  }

  # Add another param at the start with indicated stdev
  initial_stdev <- sd(outcome, na.rm = control$na.rm)*0.2
  par <- c(initial_stdev, par)

  # L-BFGS-B method
  model <- optimx::optimx(
    par = par,
    fn = objective_func,
    method = control$optim_method,
    lower_bounds <- rep(0, length(par)),
    upper = rep(Inf, length(par)),
    hessian = TRUE,
    control = list(
      trace = control$optim_trace,
      maxit = control$optim_maxit
    ),

    # ---------------------
    # ... parameters (extra)
    # to be forwarded to objective_func
    predict_func = predict_func,
    predictors = predictors,
    outcome = outcome,
    weights = control$weights,
    na.rm = control$na.rm
  )

  stdev <- par[[1]]
  par <- par[-1]

  par <- stats::coef(model)
  loss_func_value <- model$value
  predicted <- predict_func(par, predictors)

  # Generate performance metrics
  performance <- control$perf_func(
    obs = outcome,
    pred = predicted
  )

  # List to be returned
  list(
    par = par,
    loss_func_value = loss_func_value,
    predicted = predicted,
    performance = performance
  )
}
