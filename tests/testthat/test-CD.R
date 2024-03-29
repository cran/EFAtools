cd_grips <- CD(GRiPS_raw)

test_that("output class and dimensions are correct", {
  expect_is(cd_grips, "CD")
  expect_named(cd_grips, c("n_factors", "eigenvalues", "RMSE_eigenvalues",
                           "settings"))
  expect_is(cd_grips$RMSE_eigenvalues, "matrix")
})

test_that("CD returns the correct values", {
  expect_equal(cd_grips$n_factors, 1)
  expect_equal(sum(cd_grips$eigenvalues), 8)
})

grips_na <- GRiPS_raw
grips_na[c(1,3,5), c(2, 4)] <- NA
test_that("errors etc. are thrown correctly", {
  expect_error(CD(1:10), " 'x' is neither a matrix nor a dataframe. Provide a dataframe or matrix with raw data.\n")
  expect_error(CD(test_models$baseline$cormat), " 'x' is a correlation matrix, but CD only works with raw data.\n")

  expect_warning(CD(GRiPS_raw, n_factors_max = 5), " n_factors_max was set to 5 but maximum possible factors to extract is 4 . Setting n_factors_max to 4 .\n")
  expect_warning(CD(grips_na, n_factors_max = 3),
                 " The data contained missing values that were removed using stats::na.omit(). 3 row(s) were removed.\n",
                 fixed = TRUE)
})

rm(cd_grips, grips_na)
