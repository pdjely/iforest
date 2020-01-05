gen_norm_data = function(nrows, ncols) {
  cols = lapply(1:ncols, function(.) rnorm(nrows))
  data.frame(cols)
}

test_that("Non-numeric X fails", {
  data = data.frame(c('a', 'b', 'c'), c(1, 2, 3), stringsAsFactors = FALSE)
  expect_error(iTree(data), regexp = '*input data must be a matrix*')
})

test_that('Cannot send matrix with NA', {
  data = matrix(nrow=2, ncol=2)
  data[1, ] = c(1, 2)
  expect_error(iTree(data), regexp = '*contains NA*')
})

test_that('Default height limit correctly computed', {
  data = as.matrix(gen_norm_data(nrows=5, ncols=3))
  tree = iTree(data)

  correct_height = ceiling(log2(5))
  expect_equal(correct_height, tree$height_limit)
})

test_that('Cannot provide height limit less than 1', {
  data = as.matrix(gen_norm_data(nrows=5, ncols=3))
  expect_error(iTree(data, -1), regexp = 'invalid height')
})

test_that('Tree grows to limited height or less', {
  data = as.matrix(gen_norm_data(nrows=1000, ncols=8))
  # iTree is a binary tree, with 1000 examples it should artificially be
  # limited to depth of 3. Traverse left child until we hit null
  tree_depth = function(node) {
    # base case:
    if (is.null(node$left)) {
      return(1)
    } else {
      return(1 + tree_depth(node$left))
    }
  }

  # Growing the tree is stochastic, so repeat a few times. None should
  # exceed the max depth of 3
  depths = sapply(1:20, function(.) {
    tree = iTree(data, 3)
    tree_depth(tree$root)
  })

  # Add 1 to limit because root = height 0
  expect_true(all(depths <= 4))
})
