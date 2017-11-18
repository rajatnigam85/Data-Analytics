require(readr)
require(rpart)

# Load the data
responses <- read_csv("../Data/imputed_responses.csv")

# Subset the Data for train and test
set.seed(10)
datasplit <- sort(sample(nrow(responses), 0.7*nrow(responses)))
responses.train <- responses[datasplit, ]
responses.test <- responses[-datasplit, ]

# Groups of Column Number
interest.group <- c(32:63)
spend.group <- c(134:140)
# Subset of Dataframe
responses.save.train <- responses.train[,interest.group]
responses.save.train <- cbind(responses.save.train, responses.train[,spend.group[1]])
responses.save.test <- responses.test[,interest.group]
responses.save.test <- cbind(responses.save.test, responses.test[,spend.group[1]])

# Creating a function to convert the columns of the dataframe to vector
col.to.vec <- function(df, colnum){
  vec <- unlist(df[,colnum])
  return(vec)
}

# Create a function to convert the multilabel of 1 to 5 to classes of 1 to 3 as 0 and 4,5 as 1
convert.two.target <- function(x, threshold){
  x[which(x <= threshold)] <- 0
  x[which(x > threshold)] <- 1
  return(x)
}

# Changing the multilabels to two labels
responses.save.train$Finances <- convert.two.target(responses.save.train$Finances, 3)
responses.save.test$Finances <- convert.two.target(responses.save.test$Finances, 3)

tree_model <- rpart(Finances ~ ., data = responses.save.train, method = "class")
summary(tree_model)

plot(tree_model)
text(tree_model, pretty=0)

printcp(tree_model)

plotcp(tree_model)

# Pruning the tree
prune_tree_model <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"])
#plot(prune_tree_model)

# Predicting the Save Response with tree without pruning
predictedsaveresponses <- predict(tree_model, newdata = responses.save.test, type ="class")
table(predictedsaveresponses, responses.save.test$Finances)
confusionMatrix(predictedsaveresponses, responses.save.test$Finances)

# Prediction using pruned tree
predictedsaveresponses <- predict(prune_tree_model, newdata = responses.save.test, type ="class")
table(predictedsaveresponses, responses.save.test$Finances)
confusionMatrix(predictedsaveresponses, responses.save.test$Finances)
