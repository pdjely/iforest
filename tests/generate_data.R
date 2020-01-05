library(ggplot2)
library(solitude)
library(profvis)
source('tests/profiler.R')


# Find outliers in simulated gaussian dataset with extreme outliers
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

# Profile prediction
profvis(profile_predict_forest(iforest, data))

# predict again and plot output
y = predict(f, data)
outlier = as.factor(ifelse(y >=0.60, "outlier", "normal"))
table(outlier)
ggplot(data, aes(x = Var1, y = Var2, color = outlier)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")


# Compare results to solitude package
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


# Run on tight 2-d gaussian
n = 1000
Var1 = rnorm(n, 0, 1)
Var2 = rnorm(n, 0, 1)
data = data.frame(Var1, Var2)
forest = iForest(data)
ypred = predict(forest, data)
outlier = as.factor(ifelse(ypred >=0.60, "outlier", "normal"))
table(outlier)
ggplot(data, aes(x = Var1, y = Var2, color = outlier)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")

data2 = data.frame(Var1=c(1,1.5), Var2=c(1, 1))
yhat = predict(forest, data2)
