new_dorem <- function(method, data, weights, coefs, loss_func_value, performance, cross_validation, control, blueprint) {
  hardhat::new_model(
    method = method,
    data = data,
    weights = weights,
    coefs = coefs,
    loss_func_value = loss_func_value,
    performance = performance,
    cross_validation = cross_validation,
    control = control,
    blueprint = blueprint,
    class = "dorem")
}
