#classification Tree

# by using the algorithm C50 in R packages

install.packages("C50")
require(C50)

data(iris)

head(iris)

str(iris)

table(iris$Species)

#you can do classification using logistic regression or kNN algorithms

set.seed(9850)

g<- runif(nrow(iris))

irisr <- iris[order(g),]

#now check the order of species that is either random or not

str(irisr)

#using first 100 rows for of shuffled data as training set
# to eliminate target feature species from training data we use -5 which is fifth colums
# format of C5.0 is 
# C5.0(training_set, target_column_training_data)


train <- irisr[1:100,]
test <- irisr[101:150,]

m1  <- C5.0(irisr[1:100,-5],irisr[1:100,5])

m1<- C5.0(train[,-5],train[,5])

# to see the detailed output of model m1 use summary command

summary(m1)

# now creating confusion matrix using testing data set
# predict(model_name, test_data_set)

p1 <- predict(m1, irisr[101:150,])
p1

table(irisr[101:150,5],p1)
#probability of correctly identifying is 47/50
#probability of error is 3/50

plot(m1)



# second lecture on classification tree


install.packages("rpart")

install.packages("rpart.plot")


require("rpart")

require("rpart.plot")

# same iris data set will be used for this lecture

data(iris)

head(iris)

str(iris)

table(iris$Species)

#you can do classification using logistic regression or kNN algorithms

set.seed(9850)

g<- runif(nrow(iris))

irisr <- iris[order(g),]

#now check the order of species that is either random or not

str(irisr)

# format of rpart(DV~ IV)
# if you use .  dot as IV then all predictors will be used automatically

# rpart (Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = data_name , method="class")
          # use method = class for classification 

# OR simply we can write

# rpart(Species ~ ., data = data_name , method="class")

m3 <- rpart (Species ~ ., data = irisr[1:100,] , method="class")

# to see the output use 

m3

rpart.plot(m3, type = 3, extra= 101, fallen.leaves=T)


summary(m3)


p3 <- predict(m3, irisr[101:150,], type="class" )


table(irisr[101:150,5],predicted=p3)


# we compare the predicted results of two algorithms c5.0 and  classifiction tree


table(irisr[101:150,5],p1)

