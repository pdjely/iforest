gen_norm_data = function(nrows, ncols) {
  cols = lapply(1:ncols, function(.) rnorm(nrows))
  data.frame(cols)
}


test_that("Non-numeric X fails", {
  data = data.frame(c('a', 'b', 'c'), c(1, 2, 3), stringsAsFactors = FALSE)
  expect_error(iForest(data), regexp = 'X must be a numeric matrix')
})

test_that("max_samples adjusted to max rows if larger", {
  data = gen_norm_data(10, 10)
  forest = iForest(data, max_samples = 256)
  expect_equal(forest$max_samples, nrow(data))
})

test_that('Invalid max samples are rejected', {
  data = gen_norm_data(10, 10)
  expect_error(iForest(data, max_samples = 0), regexp = 'max_samples must be >= 2')
})

test_that('Forest does not contain nulls', {
  data = gen_norm_data(1000, 10)
  forest = iForest(data)
  expect_false(all(sapply(forest$forest, is.null)))
})

test_that('Forest finds obvious outlier', {
  # data generation based on:
  #   # https://www.kaggle.com/norealityshows/outlier-detection-with-isolation-forest-in-rs

  # Generate normal dis data set
  n = 500
  Var1 = rnorm(n, 0, 0.05)
  Var2 = rnorm(n, 0, 0.05)

  data = data.frame(Var1, Var2)

  # change point 200 to be outlier
  data[200, ] = c(1, 2)

  forest = iForest(data)
  ypred = predict(forest, data)
  expect_gt(ypred[200], 0.7)
})
