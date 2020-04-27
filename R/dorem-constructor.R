new_dorem <- function(method, data, coefs, loss_func_value, performance, cross_validation, shuffle, control, optim_model, blueprint) {
  hardhat::new_model(
    method = method,
    data = data,
    coefs = coefs,
    loss_func_value = loss_func_value,
    performance = performance,
    cross_validation = cross_validation,
    shuffle = shuffle,
    control = control,
    optim_model = optim_model,
    blueprint = blueprint,
    class = "dorem"
  )
}
