library(ggplot2)
library(solitude)
library(profvis)
source('tests/profiler.R')

# Example taken from:
# https://www.kaggle.com/norealityshows/outlier-detection-with-isolation-forest-in-r

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

f = iForest(data, ntree=100)
y = predict(f, data)
outlier = as.factor(ifelse(y >=0.50, "outlier", "normal"))
print(table(outlier))
ggplot(data, aes(x = Var1, y = Var2, color = outlier)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")
