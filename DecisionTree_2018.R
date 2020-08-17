###Decision Tree
install.packages("ISLR")
install.packages("tree")

library(ISLR)
library(tree) #to fit the model
attach(Carseats)
View(Carseats)
head(Carseats)

#Data manipulation
range(Sales) #range from 0 to 16

#create categorical variables bases for Sales
High=ifelse(Sales>=8, "Yes", "No")
#append High to the Carseats dataset
Carseats=data.frame(Carseats, High)
names(Carseats)

Carseats=Carseats[,-1]
dim(Carseats)

#Split data into testing and training
set.seed(2)

train= sample(1:nrow(Carseats),nrow(Carseats)/2)
test= -train
training_data = Carseats[train,]
test_data=Carseats[test,]
testing_High= High[test]

#fit the tree model using training data
tree_model= tree(High~., training_data)
plot(tree_model)
text(tree_model, pretty=0)

#use the test data to see how the model's performance
tree_predict=predict(tree_model, test_data, type = "class")
mean(tree_predict!=testing_High) #28.5% misclassification

###pruning the tree

#cross validation to check where to stop 
set.seed(3)
cv_tree=cv.tree(tree_model, FUN= prune.misclass)

names(cv_tree) #dev is deviance or the error rate

plot(cv_tree$size, cv_tree$dev, type="b")

#create the pruned tree
pruned_model=prune.misclass(tree_model, best = 9)
plot(pruned_model)
text(pruned_model, pretty=0)

tree_pred=predict(pruned_model, test_data, type="class")
mean(tree_pred!=testing_High)

##################################################################################
install.packages("TH.data")

library(TH.data)
data("bodyfat", package = "TH.data")

dim(bodyfat)


#preprocessing: data is split into training and test subsets, and a decision tree is built on the training data.
#seed for the random number generator
set.seed(1)

ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
ind

bodyfat.train <- bodyfat[ind==1,]
dim(bodyfat.train)

bodyfat.test <- bodyfat[ind==2,]
dim(bodyfat.test)

# train a decision tree
library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth

bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))
plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)



attributes(bodyfat_rpart)

#To determine if the tree is appropriate or if some of the branches need to be 
#subjected to pruning we can use the cptable element of the rpart object
#print(bodyfat_rpart)


print(bodyfat_rpart$cptable)

#The xerror column contains of estimates of cross-validated prediction error 
#for different numbers of splits (nsplit)

#complexity parameter. Any split that does not decrease the overall lack of 
#fit by a factor of cp is not attempted. For instance, with anova splitting, 
#this means that the overall R-squared must increase by cp at each step. 
#The main role of this parameter is to save computing time by pruning off 
#splits that are obviously not worthwhile. Essentially,the user informs the 
#program that any split which does not improve the fit by cp will likely be 
#pruned off by cross-validation, and that hence the program need not pursue it.

opt <- which.min(bodyfat_rpart$cptable[,"xerror"])

cp <- bodyfat_rpart$cptable[opt, "CP"]

bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
print(bodyfat_prune)
plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)

DEXfat_predict<-predict(bodyfat_prune, newdata=bodyfat.test)
xlim<-range(bodyfat$DEXfat)
plot(DEXfat_predict~DEXfat,data=bodyfat.test,xlab="Observed",ylab="predicted",ylim=xlim,xlim=xlim)
abline(a=0,b=1)

#################################################################################

# More information: https://cran.r-project.org/web/packages/HSAUR/vignettes/Ch_recursive_partitioning.pdf

#################################################################################
￼
