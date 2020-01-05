#' Implement isolation forest algorithm described in Liu, Ting, and Zhou (2008)
#'
#' @name iForest
#' @description Implement isolation forest by Liu, Ting, and Zhou.


#' Create an isolation forest
#'
#' @param X matrix, sample data
#' @param ntree integer, number of trees in forest
#' @param max_samples integer, max sub-samples for each tree
#' @return fully grown isolation forest
#' @export
iForest = function(X,
                   ntree = 100,
                   max_samples = 256)
{
  if (class(X) != 'matrix')
    X = as.matrix(X)
  if (!all(apply(X, 2, is.numeric)))
    stop('X must be a numeric matrix')
  if (nrow(X) < max_samples)
    max_samples = nrow(X)
  if (max_samples < 2)
    stop('max_samples must be >= 2')

  forest = lapply(1:ntree, function(.) {
    # shuffle and sample
    X = X[sample.int(nrow(X)), ]
    x_samp = X[sample.int(max_samples), ]
    iTree(x_samp)
  })

  ifor = list(
    forest = forest,
    ntree = ntree,
    max_samples = max_samples
  )

  class(ifor) = 'iForest'
  return(ifor)
}


# Methods

predict.iForest = function(iforest, X) {
  if (class(X) != 'matrix')
    X = as.matrix(X)

  n_instances = iforest$max_samples

  # get path lengths for each tree in forest for each sample
  avg_path = lapply(iforest$forest,
                    function(t) path_length(t, X))
  avg_path = Reduce('+', avg_path) / iforest$ntree
  c_n = 2 * (log(n_instances - 1) + -digamma(1)) - ((2 * n_instances - 1)
                                                    / n_instances)
  s = function(x, n) {
    2^(-x / n)
  }

  return(vapply(avg_path, FUN=s, FUN.VALUE=double(1), n = c_n))
}


print.iForest = function(iforest) {
  txt = glue::glue('iForest with {iforest$ntree} trees')
  cat(txt)
}
