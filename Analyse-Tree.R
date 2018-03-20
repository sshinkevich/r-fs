rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "dataframe.rds"
flname <- "dataframe-normalized.rds"

#Import data.farme as RDS-file
#==========================================
df <- readRDS(flname)

cat("Is there any NA?:", anyNA(df), "\n")
summary(df)
cat("Ratio of OPEN vs. CLOSED accounts: ", sum(df$STATUS=="OPEN")/length(df$STATUS), "vs.", 
    sum(df$STATUS=="CLOSED")/length(df$STATUS) )
#df$IS_FIRM <- NULL



# ## e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien
# library("e1071")

require("caret")

# ## Find and remove low NZV features
# #============================================
# nzv <- nearZeroVar(df, uniqueCut = 5, saveMetrics = TRUE, allowParallel = TRUE)
# cat("Low NSV features:", paste(rownames(nzv)[nzv$nzv]),"\n") #Gives item: BK_LAND_KODE
# dft <- df[,!nzv$nzv]
# df <- dft
# #============================================
df$BK_LAND_KODE <- NULL

# # Find highly correlated variables
# #============================================
# dft <- df[, sapply(df, is.numeric)]
# numr_corr <- cor(dft, use="complete.obs")
# highlyCor <- findCorrelation(numr_corr, cutoff = .9, names=TRUE)
# print(highlyCor)
# #============================================

## Data Slicing
#==========================================
set.seed(123)
sindex <- createDataPartition(y = df$STATUS, p= 0.7, list = FALSE)
df.train <- df[sindex,]
df.test <- df[-sindex,]
#==========================================
#rm(list=c("df", "dft", "highlyCor", "nzv"))
rm(df)
myFormula <- as.formula("STATUS ~ .")


#Confusion matrix 
#=============================================================
FormConfMatrix <- function(Pred, Real){
  #confMatrix <- table(df.pred, df.test$STATUS)
  confMatrix <- table(Pred, Real)
  print(confMatrix)
  #Recall
  #True positive rate (TPR), Recall, Sensitivity, probability of detection
  #===========================
  TPR <- confMatrix[1,1]/sum(confMatrix[ ,1])
  
  #Precision
  #Positive predictive value (PPV), Precision
  #===========================
  PPV <- confMatrix[1,1]/sum(confMatrix[1, ])
  cat("Sensitivity (TPR):", TPR, "\n")
  cat("Precision:", PPV, "\n")
}
#=============================================================
# confMatrix <- table(df.pred, df.test$STATUS)
# print(confMatrix)




require("e1071")

# #SVM - too slow
#=======================================================
#ctrl <- trainControl(method = "none", number = 1)
##modelSVM <- train(myFormula, data=df.test, method='svmLinear')
##myModel <- train(myFormula, data=df.train, method='gbm', metric = "ROC")

# tmp <- df.train[sample(1:length(df.train[[1]]), 1e4), ]
# #myModel <- train(myFormula, data=tmp, method='gbm')
# myModel <- train(myFormula, data=tmp, method='svmLinear', trControl=ctrl)
# 
# 
# myModel <- train(myFormula, data=df.train, method='svmLinear', trControl=ctrl)
# print(myModel)
# 
# df.pred <- predict(myModel, newdata = df.test)
#========================================================

# Create model weights (they sum to one)
#========================================================
myWeights <- ifelse(df.train$STATUS == "CLOSED",
                        (1/table(df.train$STATUS)[1]) * 0.5,
                        (1/table(df.train$STATUS)[2]) * 0.5
                    )
#sum(myWeights)
#========================================================

require("rpart")

myModel<-NULL
## Recursive Partitioning and Regression Trees
#=========================================================
ctrl <- rpart.control(minsplit = 1, cp = 0.0001, maxcompete = 5)
start_time <- Sys.time()
myModel <- rpart(myFormula, weights=myWeights,
             data=df.train,
             method="class", model=TRUE,
             control = ctrl)
end_time <- Sys.time()
cat("Training time:", format(end_time-start_time), "\n")

#plot(myModel)
#text(myModel, use.n = TRUE)

#summary(myModel)
printcp(myModel)
#plotcp(myModel)
df.pred <- predict(myModel, df.test, type="class")

FormConfMatrix(df.pred, df.test$STATUS)
confusionMatrix(table(df.pred, df.test$STATUS))

#Plot part of the tree
#plot(prune(myModel, cp = 0.01))
#text(prune(myModel, cp = 0.01))

# Variable importance
#================================
print(myModel$variable.importance) #It looks like ANTALL_BARN is not important feature
#================================

require("randomForest")

## Classification and Regression with Random Forest
#===============================================================
set.seed(1234)
start_time <- Sys.time()
myModel <- randomForest(myFormula,
                 data=df.train,
                 importance=TRUE,
                 ntree=100)
end_time <- Sys.time()
cat("Training time:", format(end_time-start_time), "\n")

df.pred <- predict(myModel, df.test, type="class")
FormConfMatrix(df.pred, df.test$STATUS)
confusionMatrix(table(df.pred, df.test$STATUS))

print(myModel)

#Importance
#===================================
round(importance(myModel),3)
tmp <- importance(myModel)
tmp[order(tmp[,3], decreasing = TRUE), ]

# tmp <- importance(myModel, type=1)
# tmp[order(tmp[1], decreasing = TRUE)]
# order(tmp, decreasing = TRUE)


# tmp <- varImp(myModel)
# tmp[order(tmp[1], decreasing = TRUE),]
# #varImpPlot(myModel)
#===================================
