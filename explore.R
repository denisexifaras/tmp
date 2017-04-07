

library(randomForest) #rf
library(neuralnet) #nn 
library(MASS)  # data
library(rpart) #decision tree
library(e1071) #svm
head(Boston)
attach(Boston)

maxs = apply(Boston, 2, max) 
mins = apply(Boston, 2, min)
index = sample(1:nrow(Boston),round(0.75*nrow(Boston)))
scaled = as.data.frame(scale(Boston, center = mins, scale = maxs - mins))
train_ = scaled[index,]
test_ = scaled[-index,]
scaledData = train_
attach(scaledData)



################################################################
# generalised linear model
################################

lm.fit = glm(medv~., data=train_)
# variable importance
summary(lm.fit)

# predictions on new data
predictions = predict(lm.fit, test_)

################################################################
# KNN
################################
#

################################################################
# DECISION TREE
################################
#
# variables are:
# minsplit minimum number of observations for a split to happen
# cp a split must decrease the overall lack of fit by a factor of 0.001 (cost complexity factor) before being attempted
# method is "class" for classification and "anova" for regression

minsplit = 30
cp = 0.001
method = "anova"

output.decisionTree = rpart(medv~., data=train_,control=rpart.control(minsplit=minsplit, cp=0.001),method=method)

plot(output.decisionTree, uniform=TRUE, main="Classification Tree for Kyphosis")
text(output.decisionTree, use.n=TRUE, all=TRUE, cex=.8)

# variable importance
summary(output.decisionTree)

# predictions on new data
predictions = predict(output.decisionTree, test_)



################################################################
# RANDOM FOREST
################################
#
# variables are:
# ntree Number of trees to grow. 
# nodesize Minimum size of terminal nodes. 

ntree = 10
nodesize = 3

output.forest = randomForest(medv ~ ., data = train_, ntree=ntree, nodesize = nodesize)
# variable importance
print(importance(output.forest,type = 2)) 

# predictions on new data
predictions = predict(output.forest, test_)


################################################################
# SVM
################################
#
# variables are:
# cross -- if a integer value k>0 is specified, a k-fold cross validation on the training data is
performed to assess the quality of the model: the accuracy rate for classification
and the Mean Squared Error for regression

cross = 1

output.svm = svm(medv ~ . , train_,cross=cross)
 
predictions = predict(output.svm, test_)



################################################################
# NEURAL NETWORK
################################
#
# variables are:
# hidden in the form of c(n,m) for n neurons for each of m hidden layers
# rep the number of repetitions for the neural network’s training.

hidden = c(5,3)
rep = 2

n = names(train_)
f = as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
output.nn = neuralnet(f,data=train_,hidden=hidden,linear.output=T,rep=rep)

# predictions on new data
predictions = compute(output.nn, test_[,1:13])$net.result



################################################################
# ASSESSMENT
################################

plot(test_$medv, predictions, xlab="original",ylab="predicted",bty="n",pch=16, col="orange");abline(0,1,lty=2)
MSE = sum((predictions - test_$medv)^2)/nrow(test_)
print(MSE

