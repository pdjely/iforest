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

test_that('Forest does not accept different number of features', {
  data1 = gen_norm_data(100, 10)
  forest = iForest(data1)
  data2 = gen_norm_data(100, 9)
  expect_error(predict(forest, data2), regexp = 'iforest trained with')
})

test_that('Forest finds obvious outlier', {
  # data generation based on:
  #   # https://www.kaggle.com/norealityshows/outlier-detection-with-isolation-forest-in-rs

  # Generate normally distributed dataset (low variance) and grow forest
  n = 500
  Var1 = rnorm(n, 0, 0.05)
  Var2 = rnorm(n, 0, 0.05)
  data = data.frame(Var1, Var2)
  forest = iForest(data)

  # create new dataset with major outliers
  out_data = data.frame(Var1 = c(1, -1), Var2 = c(2, 2))
  ypred = predict(forest, out_data)
  expect_true(all(ypred > 0.7))
})

test_that('Forest finds no outliers in uniform dataset', {
  data = data.frame(Var1=runif(1000, 0, 100), Var2=runif(1000, 0, 100))
  forest = iForest(data)
  y = predict(forest, data)
  expect_lt(var(y), 0.01)
})
