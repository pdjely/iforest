library(ggplot2)
library(solitude)
library(profvis)
source('tests/profiler.R')

# Test based on
# https://www.kaggle.com/norealityshows/outlier-detection-with-isolation-forest-in-r

n = 1000
Var1 = c(rnorm(n, 0, 0.5), rnorm(n * 0.1,-2, 1))
Var2 = c(rnorm(n, 0, 0.5), rnorm(n * 0.1,  2, 1))
outliers = c(rep(0, n), rep(1, (0.1 * n))) + 3
data = data.frame(Var1, Var2)
ggplot(data, aes(x = Var1, y = Var2)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour = "Legend")

iforest = iForest(data)
profvis(profile_predict_forest(iforest, data))

f = iForest(data, ntree=100)
y = predict(f, data)
outlier = as.factor(ifelse(y >=0.50, "outlier", "normal"))
table(outlier)
ggplot(data, aes(x = Var1, y = Var2, color = outlier)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")


cover = R.matlab::readMat('tests/data/cover.mat')
X = as.data.frame(cover$X)
y = cover$y
iforest = iForest(X)
# ypred = predict(iforest, X)
profvis(profile_predict_forest(iforest, X))
yfac = factor(ifelse(ypred >= 0.56, 1, 0), levels=c(0, 1))
print(caret::confusionMatrix(yfac, factor(y, levels=c(0, 1))))


iso = isolationForest$new()
iso$fit(X)
print(iso$scores)
# time
soltime = Sys.time()
sol_pred = iso$predict(X)
soltime = Sys.time() - soltime
print(cat(glue::glue('Finished in {soltime}')))
sol_fac = factor(ifelse(sol_pred$anomaly_score >= 0.56, 1, 0),
                 levels = c(0, 1))
print(caret::confusionMatrix(sol_fac, factor(y, levels=c(0, 1))))
