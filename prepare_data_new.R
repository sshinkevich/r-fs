rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "data_import-export.csv"

#Import txt-file
#==========================================
df <- read.csv2(flname, header=TRUE, stringsAsFactors=FALSE)

# colnames(df)
# str(df)

#Check NA values in items
#==========================================
if (anyNA(df)) {
  for (t in colnames(df)) {
    tmp <- sum(is.na(df[[t]]))
    if (tmp>0) cat("Number of NA in", t, ": ", tmp, "\n")
  }
} else {
  cat("Is there any NA?:", anyNA(df), "\n")
}
#==========================================


# # Set zeros instead of NA
# #===============================================
# dft <- df
# dft[is.na.data.frame(dft)] <- 0
# df <- dft
# cat("Is there any NA?:", anyNA(df), "\n")
# #===============================================



#Convert to Char
#==========================================
ConToChar <- function(x, vName = c(#"BK_KONTO_NR","KUNDE_NR", 
  "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "IS_OPEN", "IS_FIRM", "IS_DEAD", "BK_POSTSTED_KODE") 
  ) {
  for (t in vName) {
    if (t %in% names(x)) {
      x[, t] <- as.character(x[,t])
    }
  }
  return(x)
}
dft <- ConToChar(df)
df <- dft
#==========================================

#Convert to factor
#==========================================
ConToFactor <- function(x, vName = c("STATUS","BK_KJONN_KODE", "BK_LAND_KODE", "BK_SIVILSTAND_KODE", 
                                     "IS_OPEN", "IS_FIRM", "IS_DEAD"
) ) {
  for (t in vName) {
    if (t %in% names(x)) {
      x[[t]] <- as.factor(x[[t]])
    }
  }
  return(x)
}
dft <- ConToFactor(df)
df <- dft
#print(str(df))
#==========================================


#Save full dataframe into R-file
#=========================================
flname <- "dataframe_full.rds"
saveRDS(df, file=flname)
#=========================================

#dft <- df[, c("KUNDE_NR", "BK_KONTO_NR", "SUM_JUNI_2015", "SUM_JUNI_2016", "SUM_JUNI_2017", "STATUS", "IS_OPEN", 
#              "ANTALL_JUNI_2015", "ANTALL_JUNI_2016", "ANTALL_JUNI_2017", "IS_FIRM")]





###########################################################################
# LOAD R-file
###########################################################################
rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "dataframe_full.rds"

#Import data.farme as RDS-file
#==========================================
df <- readRDS(flname)

#dim.data.frame(df)

#Function format column name
#==============================================
FormColName <- function(d, pref="") {
  MyMonths <- c("JAN","FEB","MARS",
                "APRIL","MAI","JUNI",
                "JULI","AUG","SEPT",
                "OKT","NOV","DES")
  mNa <- as.integer(format(d, format = "%m"))
  tmp <- paste0(pref, MyMonths[mNa], "_", format(d, format = "%Y"))
  return(tmp)
}
#==============================================


# Name range of Columns
#==============================================
tDate <-seq(as.Date("01/01/2015", format='%d/%m/%Y'), as.Date("01/12/2017", format='%d/%m/%Y'), by = "month")
aName <- FormColName(tDate, pref = "ANTALL_")
sName <- FormColName(tDate, pref = "SUM_")
#==============================================

#Remove aName and sName columns
#==========================================
dft <- df
vName <- c(aName,sName)
for (t in vName) {
  dft[[t]] <- NULL
}
df <- dft
#==========================================


# Remove DØDSBO
#==========================================
# dft <- subset(df, IS_DEAD=="1")
# hist(dft$ALDER)
sindex <- df$IS_DEAD=="1"
cat("Removed number of DØDSBO:", sum(sindex))
if (length(sindex)>0) {
  dft <- subset(df, !sindex)
  dft$IS_DEAD <- NULL
  df <- dft
}
#==========================================


require("caret")

## Find and remove low NZV features
#============================================
nzv <- nearZeroVar(df, uniqueCut = 5, saveMetrics = TRUE, allowParallel = TRUE)
cat("Low NSV features:", paste(rownames(nzv)[nzv$nzv]),"\n") #Gives item: BK_LAND_KODE, CLOSED_DATE
dft <- df[,!nzv$nzv]
df <- dft
#============================================
#df$BK_LAND_KOE <- NULL

# #Set ALDER=200 for IS_FIRM
# #==============================================
# sindex <- df$IS_FIRM=="1"
# View( subset(df, IS_FIRM=="1" & ALDER!=999))
# cat("Number of IS_FIRM:",sum(sindex)," to set ALDER=200\n")
# df$ALDER[sindex] <- 200
# #sum(df$ALDER==200)
# #==============================================

#==============================================
aName <- paste0("ANTALL_", 1:12)
sName <- paste0("SUM_", 1:12)
tName <- paste0("TRAN_", 1:12)
#==============================================

#Remove columns
#==========================================
dft <- df
vName = c(#"KUNDE_NR", "BK_KONTO_NR", 
          #"BK_GEOGRAFI_KODE",
          "BK_LAND_KODE", #From NZV - analysis
          "SK_KUNDE_SCD_ID","ETTERNAVN", "FORNAVN", "ADRESSELINJE_1", "POSTSTED_NAVN", #"BK_POSTSTED_KODE",
          "KUNDE_START_DATO", #"IS_FIRM",
          "FODSELS_DATO", "CLOSED_DATE" )
for (t in vName) {
  dft[[t]] <- NULL
}
df <- dft
#==========================================



############################################
#ANALYSYS
############################################
## Data Slicing for ML
#==========================================
set.seed(123)
#sindex <- createDataPartition(y = df$STATUS, p= 0.7, list = FALSE)
sindex <- createDataPartition(y = df$IS_OPEN, p= 0.7, list = FALSE)
df.train <- df[sindex,]
df.test <- df[-sindex,]
#==========================================
#==========================================


#Choose feature-columns
#feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_START_DATO", "NUMBER_BK_KONTO_NR", aName, sName)
#feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_START_DATO", "NUMBER_BK_KONTO_NR", aName, sName,"ANTALL_INTERCEPT", "ANTALL_SLOPE")
#feature_num <- c("ALDER", "DAYS_START_DATO", "NUMBER_BK_KONTO_NR", "ANTALL_12", "ANTALL_INTERCEPT", "ANTALL_SLOPE")
#feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_START_DATO", aName, sName,"ANTALL_INTERCEPT", "ANTALL_SLOPE")
feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, sName,"ANTALL_INTERCEPT", "ANTALL_SLOPE")
feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, sName, "DELTA_SUM", "DELTA_ANTALL", "ANTALL_INTERCEPT", "ANTALL_SLOPE")
#feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", "ANTALL_12", "SUM_12", "DELTA_SUM", "DELTA_ANTALL", "ANTALL_INTERCEPT", "ANTALL_SLOPE")
feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, sName,"ANTALL_INTERCEPT", "ANTALL_SLOPE", "ANTALL_WINDOW_INTERCEPT", "ANTALL_WINDOW_SLOPE", "SUM_WINDOW_INTERCEPT", "SUM_WINDOW_SLOPE")
feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", "ANTALL_12", "SUM_12", "ANTALL_INTERCEPT", "ANTALL_SLOPE", "ANTALL_WINDOW_INTERCEPT", "ANTALL_WINDOW_SLOPE", "SUM_WINDOW_INTERCEPT", "SUM_WINDOW_SLOPE")

feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, sName, "DELTA_SUM", "DELTA_ANTALL", "ANTALL_INTERCEPT", "ANTALL_SLOPE", "SUM_INTERCEPT", "SUM_SLOPE", "ANTALL_WINDOW_INTERCEPT", "ANTALL_WINDOW_SLOPE", "SUM_WINDOW_INTERCEPT", "SUM_WINDOW_SLOPE")
#All features
feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, sName, tName, "DELTA_SUM", "DELTA_ANTALL",
                 "DELTA_TRAN", "ANTALL_INTERCEPT", "ANTALL_SLOPE", "TRAN_INTERCEPT", "TRAN_SLOPE",
                 "SUM_INTERCEPT", "SUM_SLOPE", "ANTALL_WINDOW_INTERCEPT", "ANTALL_WINDOW_SLOPE", 
                 "SUM_WINDOW_INTERCEPT", "SUM_WINDOW_SLOPE", "TRAN_WINDOW_INTERCEPT", "TRAN_WINDOW_SLOPE")
#Without SUM but with TRAN
feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, tName, "DELTA_ANTALL",
                 "DELTA_TRAN", "ANTALL_INTERCEPT", "ANTALL_SLOPE", "TRAN_INTERCEPT", "TRAN_SLOPE",
                 "ANTALL_WINDOW_INTERCEPT", "ANTALL_WINDOW_SLOPE", 
                 "TRAN_WINDOW_INTERCEPT", "TRAN_WINDOW_SLOPE")
#Without SUM and 11-month but with TRAN
feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", "ANTALL_12", "TRAN_12", "DELTA_ANTALL",
                 "DELTA_TRAN", "ANTALL_INTERCEPT", "ANTALL_SLOPE", "TRAN_INTERCEPT", "TRAN_SLOPE",
                 "ANTALL_WINDOW_INTERCEPT", "ANTALL_WINDOW_SLOPE", 
                 "TRAN_WINDOW_INTERCEPT", "TRAN_WINDOW_SLOPE")

feature_num <- c("ALDER", "DAYS_BEING_CUSTOMER", aName, tName, 
                 "ANTALL_INTERCEPT", "ANTALL_SLOPE", "TRAN_INTERCEPT", "TRAN_SLOPE",
                 "ANTALL_WINDOW_INTERCEPT", "ANTALL_WINDOW_SLOPE", 
                 "TRAN_WINDOW_INTERCEPT", "TRAN_WINDOW_SLOPE")

feature_num <- c("NUMBER_BK_KONTO_NR", "ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, tName, "TRAN_INTERCEPT", "TRAN_SLOPE",
                 "ANTALL_INTERCEPT", "ANTALL_SLOPE")

#MAin
feature_num <- c("ALDER", "DAYS_BEING_CUSTOMER", aName, tName, 
                 "ANTALL_INTERCEPT", "ANTALL_SLOPE")

feature_num <- c("ALDER", "DAYS_BEING_CUSTOMER", aName, tName, 
                 "TRAN_INTERCEPT", "TRAN_SLOPE", "ANTALL_INTERCEPT", "ANTALL_SLOPE")

feature_num <- c("ALDER", "DAYS_BEING_CUSTOMER", aName, sName, "ANTALL_SLOPE")



#feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_START_DATO", aName[aName!="ANTALL_12"], sName[aName!="ANTALL_12"],"ANTALL_INTERCEPT", "ANTALL_SLOPE")

#feature_chr <- c("BK_KJONN_KODE", "BK_SIVILSTAND_KODE", "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "IS_FIRM")
feature_chr <- c("BK_KJONN_KODE", "BK_SIVILSTAND_KODE", "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "BK_POSTSTED_KODE")
feature_chr <- c("BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE")
feature_chr <- c("BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "BK_POSTSTED_KODE")
#feature_chr <- c("BK_ANSVARSTED_KODE", "BK_POSTSTED_KODE")
feature_nm <- c(feature_num, feature_chr)
#feature_nm <- c("DAYS_START_DATO", "ALDER", "ANTALL_12", "ANTALL_1", "SUM_12", "BK_ANSVARSTED_KODE")
label_nm <- "IS_OPEN"


#dft <- df[c(feature_nm, label_nm)]
#df <- dft



#Choose features
df.train <- df.train[c(feature_nm, label_nm)]
df.test <- df.test[c(feature_nm, label_nm)]

# Create model weights (they sum to one)
#========================================================
myWeights <- ifelse(df.train$IS_OPEN == "0",
                    (1/(table(df.train$IS_OPEN)[1])) * 0.5,
                    (1/(table(df.train$IS_OPEN)[2])) * 0.5
)
table(df.train$IS_OPEN)
myWeights <- 1e+5*myWeights
cat("Weights unique values:", unique(myWeights), "\n")
sum(myWeights)
sumwpos <- sum(myWeights * (df.train$IS_OPEN == "0"))
sumwneg <- sum(myWeights * (df.train$IS_OPEN == "1"))
#========================================================


require(xgboost)
#XGBoost - eXtreme Gradient Boosting
#============================================
dft <- df.train[feature_nm]
tmp <- sapply(df.train[[label_nm]], function(x) as.numeric(levels(x))[x])
dtrain <- xgb.DMatrix(data = data.matrix(dft), label = tmp, weight = myWeights)
#dtrain <- xgb.DMatrix(data = data.matrix(dft), label = tmp)

dft <- df.test[feature_nm]
tmp <- sapply(df.test[[label_nm]], function(x) as.numeric(levels(x))[x])
dtest <- xgb.DMatrix(data = data.matrix(dft), label = tmp)

watchlist <- list(train=dtrain, test=dtest)
#watchlist <- list(train=dtrain)

# Training a XGBoost classifier
param <- list(  objective           = "binary:logistic", #"reg:linear", 
                scale_pos_weight = sumwneg / sumwpos, 
                booster             = "gbtree", #Default - gbtree
                eta                 = 0.05, #learning rate Default = 0.3
                max_depth           = 70, #Default = 6
                #nthread             = 4, #number of cpu threads, Deafult - max
                subsample           = 0.6, #Default = 1 in order to avoid overfitting
                colsample_bytree    = 0.8,
                max_delta_step      = 7, #Default =0, Maximum delta step we allow each tree’s weight estimation to be.
                seed = 48608,
                eval_metric         = "auc", #"ndcg" #"map" #"error" #"logloss" 
                #min_child_weight    = 6, # [default=1]
                reg_alpha            = 1, #Default = 0 #L1
                reg_lambda           = 0.99  #Default = 1 #L2
)

start_time <- Sys.time()
myModel <- xgb.train( params              = param,
                      data                = dtrain, 
                      nrounds             = 900, 
                      verbose             = 1, 
                      early_stopping_rounds    = 10,
                      watchlist           = watchlist,
                      maximize            = TRUE
)
end_time <- Sys.time()
cat("Training time:", format(end_time-start_time), "\n")

#dcontrol <- dtest

dft <- df_gtest[feature_nm]
tmp <- sapply(df_gtest[[label_nm]], function(x) as.numeric(levels(x))[x])
dcontrol <- xgb.DMatrix(data = data.matrix(dft), label = tmp)



label = getinfo(dcontrol, "label")
pred.prob <- predict(myModel, dcontrol)
pred.label <- as.integer(pred.prob > 0.5)
confusionMatrix(table(pred.label, label))
#FormConfMatrix(pred.label, label)

#err <- as.numeric(sum(as.integer(pred.prob > 0.5) != label))/length(label)
#print(paste("test-error=", err))

#tmp <- cbind(pred.label, df.test$STATUS)

importance_matrix <- xgb.importance(feature_names = feature_nm, model = myModel)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


###################################################################################
# AUC calculations
###################################################################################
require("pROC")

# auc <- roc(label, pred.label)
auc <- roc(label, pred.prob)
print(auc$auc)
#print(roc(label, pred.prob))

plot.roc(label, pred.prob, col = "blue", print.auc = TRUE, print.auc.col = "red", grid = TRUE)
#plot.roc(label, pred.label, col = "blue", print.auc = TRUE, print.auc.col = "red", grid = TRUE)

# plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
# abline(h=1,col='blue',lwd=2)
# abline(h=0,col='red',lwd=2)

# df_auc <- data.frame(tpr = auc$sensitivities, fpr = 1 - auc$specificities)
# ggplot(aes(x = fpr,  y = tpr), data = df_auc) +
#   geom_line(color = "blue", size = 1) +
#   geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
#   theme_bw(base_size = 18)


###################################################################################
# Cross-validation
###################################################################################
start_time <- Sys.time()
myModel <- xgb.cv( params              = param,
                      data                = dtrain,
                      nfold               = 5,
                      metrics             = list("auc"),
                      showsd              = TRUE,
                      stratified = TRUE,
                      nrounds             = 900, 
                      verbose             = 1, 
                      early_stopping_rounds    = 10,
                      maximize            = TRUE
)
end_time <- Sys.time()
cat("Training time:", format(end_time-start_time), "\n")

#print(myModel)


label = getinfo(dtest, "label")
pred.prob <- predict(myModel, dtest)


####################################################################################
# NORMALIZATION
####################################################################################

# mx <- max(df[, aName])
# summary(df[, aName])
# 
# 
# dft <- df[, aName]
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "ANTALL vs. Date"
# )

# # MinMax - normalization
# #========================================
# Normalize.MinMax<- function(x)
# {
#   ####normalize to [0,1]
#   dft <- x
#   mx <- max(dft)
#   mn <- min(dft)
#   dft <- (dft-mn)/(mx-mn)
#   return(dft)
# }
# #========================================
# 
# 
# dft <- Normalize.MinMax(df[, aName])
# # boxplot(dft,
# #         xlab = "Date", ylab = "Antall",
# #         main = "MinMax - ANTALL vs. Date"
# # )
# #summary(dft)
# df[, aName] <- dft
# #========================================
# 
# 
# #Normalize items by MinMax
# #=========================================
# cName <- c("ALDER", "ANTALL_BARN", "NUMBER_BK_KONTO_NR", "DAYS_START_DATO")
# dft <- df[, cName]
# #dft <- sapply(dft[, cName], Normalize.MinMax)
# for (t in cName) {
#   dft[[t]] <- Normalize.MinMax(df[[t]])
# }
# df[, cName] <- dft
# #=========================================
# 
# 
# # dft <- Normalize.MinMax(df[, sName])
# # boxplot(dft,
# #         xlab = "Date", ylab = "Antall",
# #         main = "MinMax - SUM vs. Date"
# # )
# # summary(dft)
# 
# #summary(df[, sName])
# #hist(df[, aName[12]], probability=TRUE)
# 
# 
# #Zscore - normalization
# #========================================
# Normalize.Zscore<- function(x)
# {
#   ####normalize to [0,1]
#   dft <- x
#   tmp <- unlist(dft, recursive = TRUE, use.names = FALSE)
#   md <- mean(tmp)
#   sdv <- sd(tmp)
#   dft <- (dft-md)/sdv
#   return(dft)
# }
# # dft <- Normalize.Zscore(df[, aName])
# # boxplot(dft,
# #         xlab = "Date", ylab = "Antall",
# #         main = "Zscore - ANTALL vs. Date"
# # )
# # summary(dft[, aName])
# 
# #========================================
# dft <- Normalize.Zscore(df[, sName])
# # boxplot(dft,
# #         xlab = "Date", ylab = "SUM",
# #         main = "Zscore - SUM vs. Date"
# # )
# # summary(dft)
# df[, sName] <- dft
# 
# 
# 
# 
# 
# 
# #Logistic - normalization
# #========================================
# dft[, aName] <- 1/(1+exp(-df[, aName]))
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "Logistic - ANTALL vs. Date"
# )
# #========================================
# 
# #summary(dft[, aName])
# 
# #Tanh
# #========================================
# Normalize.Tanh <- function(x)
# {
#   ####normalize to [0,1]
#   dft <- x
#   tmp <- unlist(dft, recursive = TRUE, use.names = FALSE)
#   md <- mean(tmp)
#   sdv <- sd(tmp)
#   dft <- (dft-md)/sdv
#   dft <- sapply(dft, function(x) tanh(x/2))
#   return(dft)
# }
# # dft <- scale(df[, aName])
# # boxplot(dft,
# #         xlab = "Date", ylab = "Antall",
# #         main = "Scale - ANTALL vs. Date"
# # )
# 
# # tmp <- unlist(df[, aName], recursive = TRUE, use.names = FALSE)
# # md <- mean(tmp)
# # sdv <- sd(tmp)
# # dft <- df[, aName]
# # dft <- (dft-md)/sdv
# # dft <- sapply(dft, function(x) tanh(x/2))
# 
# dft <- Normalize.Tanh(df[, aName])
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "Tanh - ANTALL vs. Date"
# )
# summary(dft)
# #========================================
# # dft <- Normalize.Tanh(df[, sName])
# # boxplot(dft,
# #         xlab = "Date", ylab = "Antall",
# #         main = "Tanh - SUM vs. Date"
# # )
# # summary(dft)
# 
# 
# 
# # tmp <- sapply(df[, aName], mean)
# # plot(tmp, type="b")
# 
# #plot(tanh(seq(0, 10, by=0.1)/2), type="b")
# 





#=========================================
flname <- "dataframe.rds"
#flname <- "dataframe-normalized.rds"
saveRDS(df, file=flname)
