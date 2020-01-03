# =========================================================
# iForest constructor and methods
# =========================================================

#' Implement isolation forest algorithm described in Liu, Ting, and Zhou (2008)
#'
#' @name iForest
#' @description Implement isolation forest by Liu, Ting, and Zhou.
iForest = function(X,
                   ntree = 100,
                   max_samples = 256)
{
  if (class(X) == 'matrix')
    X = as.data.frame(X)
  if (nrow(X) < 256)
    max_samples = nrow(X)

  forest = lapply(1:ntree, function(.) {
    # shuffle and sample
    X = dplyr::sample_frac(X, size = 1)
    x_samp = dplyr::sample_n(X, size = max_samples, replace = FALSE)
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


new_iforest = function() {
  NULL
}


validate_iforest = function() {
  NULL
}

# Methods

predict.iForest = function(iforest, X) {
  if (class(X) == 'matrix')
    X = as.data.frame(X)

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
