test_that("banister model prediction works as expected", {

  test_model <- list(coefs=list(intercept=496,
				w=list(PTE_gain=.07, PTE_tau=60,
				       NTE_gain=.27, NTE_tau=13)),
                     control=list(link_func=function(x) x))


  predictors_df <- data.frame(w = c(0, 0, 0))
  pred_zeros <- banister_predict(test_model, predictors_df)

  expect_equal(pred_zeros, rep(496, 3))

  predictors_df <- data.frame(w = c(1, 0, 0))
  pred_unit <- banister_predict(test_model, predictors_df)

  expect_equal(pred_unit[1], 496)  # time 0: no fitness or fatigue accumulated
})
