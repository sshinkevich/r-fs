rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "dataframe.rds"
#flname <- "dataframe-normalized.rds"

#Import data.farme as RDS-file
#==========================================
df <- readRDS(flname)

cat("Is there any NA?:", anyNA(df), "\n")
#summary(df)
cat("Ratio of OPEN vs. CLOSED accounts: ", sum(df$STATUS=="OPEN")/length(df$STATUS), "vs.", 
    sum(df$STATUS=="CLOSED")/length(df$STATUS) )
#df$IS_FIRM <- NULL

# Remove DØDSBO
#==========================================
sindex <- df$IS_DEAD=="1"
cat("Removed number of DØDSBO:", sum(sindex))
if (length(sindex)>0) {
  dft <- subset(df, !sindex)
  dft$IS_DEAD <- NULL
  df <- dft
}
#==========================================

#Transform data frame
#==========================================
#sindex <- df$STATUS == "CLOSED"
sindex <- df$STATUS == "OPEN"
if (sum(sindex)>0) {
  dft <- df
  dft$STATUS <- as.integer(sindex)
  df <- dft
}
#cat("Ratio of OPEN vs. CLOSED accounts: ", sum(df$STATUS==0)/length(df$STATUS), "vs.", 
#    sum(df$STATUS==1)/length(df$STATUS) )
#==========================================



require("caret")
## Find and remove low NZV features
# #============================================
# nzv <- nearZeroVar(df, uniqueCut = 5, saveMetrics = TRUE, allowParallel = TRUE)
# cat("Low NSV features:", paste(rownames(nzv)[nzv$nzv]),"\n") #Gives item: BK_LAND_KODE
# dft <- df[,!nzv$nzv]
# df <- dft
# #============================================
df$BK_LAND_KODE <- NULL

## Data Slicing
#==========================================
set.seed(1234)
sindex <- createDataPartition(y = df$STATUS, p= 0.7, list = FALSE)
df.train <- df[sindex,]
df.test <- df[-sindex,]
#==========================================


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



require(xgboost)
#XGBoost - eXtreme Gradient Boosting
#============================================
dft <- df.train[ , !(names(df.train) %in% c("STATUS"))]
features <- names(dft)
dtrain <- xgb.DMatrix(data = data.matrix(dft), label = df.train$STATUS)
 
dft <- df.test[ , !(names(df.test) %in% c("STATUS"))]
dtest <- xgb.DMatrix(data = data.matrix(dft), label = df.test$STATUS)

watchlist <- list(train=dtrain, test=dtest)

# Training a XGBoost classifier
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree", #Default - gbtree
                eta                 = 0.1, #learning rate Default = 0.3
                max_depth           = 30, #Default = 6
                nthread             = 4, #number of cpu threads
                #subsample           = 0.7, #Default = 1 in otrder to avoid overfitting
                #colsample_bytree    = 0.8,
                eval_metric         = "auc"
                #min_child_weight    = 6,
                #alpha               = 4
                # lambda = 1
)

start_time <- Sys.time()
myModel <- xgb.train( params              = param, 
                      data                = dtrain, 
                      nrounds             = 500, 
                      verbose             = 1, 
                      early_stopping_rounds    = 10,
                      watchlist           = watchlist,
                      maximize            = TRUE
                      )
end_time <- Sys.time()
cat("Training time:", format(end_time-start_time), "\n")

label = getinfo(dtest, "label")
pred.prob <- predict(myModel, dtest)
pred.label <- as.integer(pred.prob > 0.5)
confusionMatrix(table(pred.label, label))
FormConfMatrix(pred.label, label)

#err <- as.numeric(sum(as.integer(pred.prob > 0.5) != label))/length(label)
#print(paste("test-error=", err))

#tmp <- cbind(pred.label, df.test$STATUS)

importance_matrix <- xgb.importance(feature_names = features, model = myModel)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# require("DiagrammeR")
# xgb.plot.tree(feature_names = features, model = myModel,  n_first_tree = 3)
