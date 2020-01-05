# =========================================================
# iTree constructor and methods
# =========================================================


#' Create a new isolation tree based on input data X
#'
#' @param X matrix, input data
#' @param height_limit integer, height limit to grow tree to. If not provided,
#'     computed as specified in the paper
#' @return isolation tree grown up to height limit
#' @export
iTree = function(X, height_limit=NULL) {
  if (any(is.na(X)))
    stop('input data contains NA values')
  if (class(X) != 'matrix')
    stop('input data must be a matrix')

  if (missing(height_limit))
    height_limit = ceiling(log2(nrow(X)))
  if (height_limit < 1)
    stop('invalid height limit provided (must be >= 1)')

  new_itree(X, height_limit)
}


#' Internal iTree constructor
#'
#' Fits a new iTree to dataset X
#' @param X matrix, input data
#' @param height_limit integer, tree height limit
#' @return new iTree
new_itree = function(X, height_limit) {
  root = new_itree_node(X, height_limit, 0)
  t = list(root=root,
           height_limit = height_limit,
           n_features = ncol(X))
  class(t) = 'iTree'
  return(t)
}


#' This function could be used to validate the tree's internal structure
#' but is not currently used
validate_itree = function() {
  stop('Not implemented')
}


#' Internal constructor for itree node. Called recursively to build tree.
#'
#' @param X matrix, data for current node
#' @param height_limit integer, max height of tree
#' @param current_height integer, height of current node
#' @param type internal or external (leaf) node
#' @return new node
new_itree_node = function(X,
                          height_limit,
                          current_height=0,
                          type='internal')
{
  # base case: exceeded height limit or out of data or all rows same
  if (current_height >= height_limit || nrow(X) <= 1 ||
      sum(apply(X, 2, function(.) min(.) == max(.))) == ncol(X) ) {
    node = list(
      n_components = nrow(X),
      node_level = current_height,
      left = NULL,
      right = NULL,
      split_attr = NULL,
      split_val = NULL,
      node_type = 'external')
    class(node) = c('itree_external', 'itree_node')
    return(node)
  }

  # randomly choose an attribute to split on and split value
  q = NULL
  retry_count = 0
  while (retry_count < 3) {
    q = floor(runif(1, min=1, max=ncol(X) + 1))
    p = runif(1, min=min(X[, q]), max=(X[, q] + 1))

    # try a different attribute if zero variance
    if (min(X[, q] != max(X[, q])))
      break
    else
      retry_count = retry_count + 1
  }

  # print(glue::glue('Split Attr: {q}\nSplit Val: {p}\n'))
  # left < p; right >= p
  left_ind = which(X[, q] < p)
  right_ind = which(X[, q] >= p)

  node = list(
    n_components = nrow(X),
    node_level = current_height + 1,
    left = new_itree_node(X[left_ind, , drop=FALSE],
                          height_limit,
                          current_height + 1),
    right = new_itree_node(X[right_ind, , drop=FALSE],
                           height_limit,
                           current_height + 1),
    split_attr = q,
    split_val = p,
    node_type = 'internal')

  class(node) = c('itree_internal', 'itree_node')
  return(node)
}


###         ###
### Methods ###
###         ###


#' Compute path length for all features in a data set
#'
#' @param tree an iTree
#' @param X data.frame
#' @return vector of path lengths
path_length = function(tree, data) {
  l = vector(mode='double', length=nrow(data))
  for (i in 1:length(l)) {
    l[i] = path_length_node(tree$root, data[i, ])
  }

  return(l)
}


#' Compute path length from root node for given instance x
#'
#' @param root iTree root node
#' @param data numeric vector containing one sample
#' @param cur_length current path length (for recursion)
#' @return numeric path length
path_length_node = function(root, data, cur_length = 0) {
  avg_path_length = function(n_instances) {
    if (n_instances <= 1) {
      0
    } else {
      2 * (log(n_instances - 1) + -digamma(1)) - ((2 * n_instances - 1)
                                                  / n_instances)
    }
  }

  # base case: external node
  if (root[['node_type']] == 'external') {
    return(cur_length + avg_path_length(root$n_components))
  }

  # Optimized to remove q temporary assignment and $ referencing
  if (data[[root[['split_attr']]]] < root[['split_val']]) {
    return(path_length_node(root$left, data, cur_length + 1))
  } else {
    return(path_length_node(root$right, data, cur_length + 1))
  }
}


print.iTree = function(tree) {
  txt = glue::glue('an iTree with {itree$n_features} features')
  cat(txt)
}
