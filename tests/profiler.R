library(ggplot2)
library(solitude)
library(profvis)

# source('R/generics.R')
# source('R/iforest.R')
# source('R/itree.R')

# Test based on
# https://www.kaggle.com/norealityshows/outlier-detection-with-isolation-forest-in-r

profile_build_forest = function(data) {
  iForest(data)
}


profile_predict_forest = function(iforest, data) {
  predict(iforest, data)
}

#' First run on artificial dataset: 23660 ms
#' Optimization log:
#'   - Made path_length non-generic: 21180 ms
#'   - Optimized path_length_node to drop temporary q variable and
#'     index data array with [[]] instead of dataframe: 17660 ms
#'   - Use vapply instead of sapply in return of predict.iForest: 16140
#'   - Change $ to [[]] in 'external' conditional: 17600
