# Run forest cover dataset
cover = R.matlab::readMat('tests/data/cover.mat')
X = as.data.frame(cover$X)
y = cover$y
iforest = iForest(X)
ypred = predict(iforest, X)

yfac = factor(ifelse(ypred >= 0.56, 1, 0), levels=c(0, 1))
print(caret::confusionMatrix(yfac, factor(y, levels=c(0, 1))))
