rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "churn_version2.txt"

#Import txt-file
#==========================================
df <- read.csv2(flname, header=TRUE, stringsAsFactors=FALSE)

###########################################################################
#Load
###########################################################################

#Check empty fields for STATUS
#==========================================
sindex <- toupper(df$STATUS) %in% c("OPEN","CLOSED")
cat("Number of non-correct STATUS-items:", sum(!sindex), "\n")
cat("Remove non-correct STATUS-items:", sum(!sindex), "\n")
df <- subset(df, sindex)
cat("%% of OPEN vs. CLOSED accounts: ", sum(df$STATUS=="OPEN")/length(df$STATUS), "vs.", 
    sum(df$STATUS=="CLOSED")/length(df$STATUS) )

#Setting flag as IS_OPEN
sindex <- df$STATUS=="OPEN"
df$IS_OPEN <- as.integer(sindex)
#df$IS_OPEN <- ifelse(df$STATUS=="OPEN",1,0)
#==========================================

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

#Replace NA values with ""
#==========================================
ReplaceNA <- function(x, vName = c("BK_LAND_KODE"), rValue = "" ) {
  for (t in vName) {
    x[[t]][is.na(x[[t]])] <- ""
  }
  return(x)
}
dft <- ReplaceNA(df)
df <- dft
#==========================================


# Set zeros instead of NA
#===============================================
dft <- df
dft[is.na.data.frame(dft)] <- 0
df <- dft
cat("Is there any NA?:", anyNA(df), "\n")
#===============================================


#Convert to integer
#==========================================
dft <- df
vName = c("ALDER","ANTALL_BARN")
for (t in vName) {
  dft[[t]] <- as.integer(df[[t]])
}
df <- dft
#==========================================

#Define companies AS
#==========================================
tmp <- paste(df$ETTERNAVN, df$FORNAVN, df$ADRESSELINJE_1)
sindex <- grepl(" AS$|^AS ", tmp, ignore.case = TRUE)
cat("Number of AS in ADRESSE:", sum(sindex))
sindex <- grepl(" AS$|^AS ", tmp, ignore.case = TRUE) | df$ALDER >= 150 | trimws(df$FORNAVN) == ""
cat("Total number of IS_FIRM:", sum(sindex))

#Setting flag as Is_Firm
df$IS_FIRM <- as.integer(sindex)
#==========================================


#DØDSBO
#==========================================
tmp <- paste(df$ETTERNAVN, df$FORNAVN, df$ADRESSELINJE_1)
sindex <- grepl("DØDSBO", tmp, ignore.case = TRUE)
cat("Number of DØDSBO:", sum(sindex))

#Setting flag as DØDSBO
df$IS_DEAD <- as.integer(sindex)
#==========================================



#Check to remove SK_KUNDE_SCD_ID
#==========================================
cat("NB: Number of non-unique accounts:", length(df$BK_KONTO_NR)-length(unique(df$BK_KONTO_NR)), "\n")
cat("NB: Number of non-unique users:", length(df$KUNDE_NR)-length(unique(df$KUNDE_NR)), "\n")
cat("NB: Number of non-unique SK_KUNDE_SCD_ID:", length(df$SK_KUNDE_SCD_ID)-length(unique(df$SK_KUNDE_SCD_ID)), "\n")

tmp <- duplicated(df$KUNDE_NR)
t <- duplicated(df$SK_KUNDE_SCD_ID)
#If XOR = 0 then we have complet correlation
cat("NB: XOR of dublicated KUNDE_NR and SK_KUNDE_SCD_ID:", sum(xor(tmp, t)), "\n") 
cat("NB: Number of unique accounts vs. users:", length(unique(df$BK_KONTO_NR)), "vs.", length(unique(df$KUNDE_NR)), "\n")
# Analyse 
#=============================================
dft <- aggregate(BK_KONTO_NR ~ KUNDE_NR, data=df, length)
names(dft)[names(dft)=="BK_KONTO_NR"] <- "NUMBER_BK_KONTO_NR" #Rename to NUMER of aggregated values
#head(dft)

dft <- aggregate(KUNDE_NR ~ NUMBER_BK_KONTO_NR, data=dft, length)
names(dft)[names(dft)=="KUNDE_NR"] <- "NUMBER_KUNDE_NR" #Rename to NUMER of aggregated values
print(dft)
#==============================================

# Write number of accounts for a customer
#==============================================
dft <- aggregate(BK_KONTO_NR ~ KUNDE_NR, data=df, length)
names(dft)[names(dft)=="BK_KONTO_NR"] <- "NUMBER_BK_KONTO_NR"
dft <- merge(df, dft, by="KUNDE_NR")
cat("Is there any NA in NUMBER_BK_KONTO_NR?:", anyNA(dft$NUMBER_BK_KONTO_NR), "\n")
#sort(unique(dft$NUMBER_BK_KONTO_NR))
df <- dft
#==============================================

#Convert to Char
#==========================================
ConToChar <- function(x, vName = c(#"BK_KONTO_NR","KUNDE_NR", 
  "BK_ANSVARSTED_KODE", "BK_POSTSTED_KODE", "BK_GEOGRAFI_KODE", "IS_OPEN", "IS_FIRM", "IS_DEAD") 
) {
  for (t in vName) {
    x[[t]] <- as.character(x[[t]])
  }
  return(x)
}
dft <- ConToChar(df)
df <- dft
#==========================================

#Convert to factor
#==========================================
ConToFactor <- function(x, vName = c("STATUS","BK_KJONN_KODE", "BK_LAND_KODE", "BK_SIVILSTAND_KODE", 
                                     "IS_OPEN", "IS_FIRM", "IS_DEAD") ) {
  for (t in vName) {
    if (t %in% names(x)) {
      x[[t]] <- as.factor(x[[t]])
    }
  }
  return(x)
}
dft <- ConToFactor(df)
df <- dft
#==========================================



###########################################################################
#END of Load
###########################################################################


###########################################################################
#FEature ingeneering
###########################################################################

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


#Convert Start Dato to Number days
#==============================================
#Check that all items have START_DATO
cat("Number of items don't having START DATE:", sum(df$KUNDE_START_DATO==""), "\n")
dft <- df
tmp <- as.Date(dft$KUNDE_START_DATO, format='%d/%m/%Y')
dft$DAYS_START_DATO <- as.integer(tmp - as.Date("01/01/1970", format='%d/%m/%Y'))
df <- dft

cat("Number of items don't having correct START DATE:", sum(df$DAYS_START_DATO==0 & df$ALDER<=37), "\n")
dft <- aggregate(BK_KONTO_NR ~ DAYS_START_DATO, data=df, length)
head(dft)
#============================================



## Copy 12-month time slot
#===========================================================
dft <- df
#Copy for the last 12 month for OPEN accounts
tDate <- seq.Date(as.Date("01/01/2017", format='%d/%m/%Y'), by = "month", length.out = 12)
#tDate <- seq.Date(as.Date("01/01/2016", format='%d/%m/%Y'), by = "month", length.out = 12)#We use 2016 year to be sure in behaviour of open accounts
cName <- FormColName(tDate, pref = "")
cName1 <- paste0("ANTALL_", cName)
cName <- paste0("SUM_", cName)
dfmyA <- matrix(cName1, nrow = nrow(dft), ncol = 12, byrow = TRUE)
dfmyS <- matrix(cName, nrow = nrow(dft), ncol = 12, byrow = TRUE)

ncName1 <- paste0("ANTALL_", 1:12)
ncName <- paste0("SUM_", 1:12)
#Create zeros-columns
dft[ncName1]<-0
dft[ncName]<-0

sindex <-  df$STATUS == "CLOSED"
tDate <- dft[sindex, "CLOSED_DATE"]
#Shift Closed date to the middle of month
tDate <- gsub( "^[0-3][0-9]", "15", tDate)
rw <- as.integer(rownames(dft)[sindex]) #Row names for CLOSED accounts
tDate <- as.Date(tDate, format='%d/%m/%Y')-365

start_time <- Sys.time()
for (i in 1:length(tDate)) {
  tmp <- seq.Date(tDate[i], by = "month", length.out = 12)
  cName <- FormColName(tmp, pref = "")
  cName1 <- paste0("ANTALL_", cName)
  cName <- paste0("SUM_", cName)
  dfmyA[rw[i], ] <- cName1
  dfmyS[rw[i], ] <- cName
}
tmp <- 1:length(sindex)
for (i in 1:length(ncName)) { #Replace by column
  dft[ncName1[i]] <- sapply(tmp, FUN = function(x) df[x, dfmyA[ x,i]], USE.NAMES = FALSE)
  dft[ncName[i]] <- sapply(tmp, FUN = function(x) df[x, dfmyS[ x,i]], USE.NAMES = FALSE)
}
end_time <- Sys.time()
cat("Computation time:", format(end_time-start_time), "\n")
df <- dft
#==================================================================


#Convert DATA_CLOSED to DAYS_BEING_CUSTOMER
#==================================================================
dft <- df
tDate <-as.Date(ifelse(dft$CLOSED_DATE!="", dft$CLOSED_DATE, "31/12/2017"), format='%d/%m/%Y')
dft$DAYS_BEING_CUSTOMER <- as.integer(tDate - as.Date("01/01/1970", format='%d/%m/%Y')) - dft$DAYS_START_DATO
df <- dft
#==================================================================


#Export file
#==========================================
# flname <- "data_import-export.csv"
# write.csv2(df, file = flname, row.names=FALSE) #, na="")

#Save full dataframe into R-file
#=========================================
flname <- "dataframe_full.rds"
#saveRDS(df, file=flname)
#=========================================




############################################
#ANALYSYS
############################################

require("caret")

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

# Remove negative DAYS_BEING_CUSTOMER
#==========================================
sindex <- df$DAYS_BEING_CUSTOMER <= 0
cat("Removing items with negative DAYS_BEING_CUSTOMER:", sum(sindex), "\n")
dft <- df[!sindex, ]
df <- dft
#==========================================

## Find and remove low NZV features
#============================================
nzv <- nearZeroVar(df, uniqueCut = 5, saveMetrics = TRUE, allowParallel = TRUE)
cat("Low NSV features:", paste(rownames(nzv)[nzv$nzv]),"\n") #Gives item: BK_LAND_KODE, CLOSED_DATE
dft <- df[,!nzv$nzv]
df <- dft
#============================================

#==========================================
aName <- paste0("ANTALL_", 1:12)
sName <- paste0("SUM_", 1:12)
tName <- paste0("TRAN_", 1:12)
#==========================================



#Set transaction
#==========================================
#Transactions
SetTRAN <- function(x, as_abs = TRUE) {
  df <- x
  aLength <- sum(grepl("^ANTALL_[[:digit:]]", colnames(df), ignore.case = TRUE))
  tName <- paste0("TRAN_", 1:aLength)
  for (i in 1:length(tName)) {
    df[[tName[i]]] <- df[[sName[i]]]/df[[aName[i]]]
  }
  for (i in tName) {
    df[is.na(df[,i]),i] <- 0
  }
  if (as_abs) {
    df[ , tName] <- abs(df[ , tName])
  }
  return(df)
}
df <- SetTRAN(df, as_abs = TRUE)
#==========================================


#Set DELTA
#===========================================
df$DELTA_SUM <- df$SUM_12-df$SUM_11
df$DELTA_ANTALL <- df$ANTALL_12-df$ANTALL_11
df$DELTA_TRAN <- df$TRAN_12-df$TRAN_11
#===========================================


#Set Linear FIT
#===========================================
MyFitWindow <- function(x, pref = "ANTALL_", FitWindow = 3) {
  df <- x
  aLength <- sum(grepl("^ANTALL_[[:digit:]]", colnames(df), ignore.case = TRUE))
  aName <- paste0(pref, 1:aLength)
  aFName <- c(paste0(pref, "WINDOW_INTERCEPT"), paste0(pref, "WINDOW_SLOPE"))
  if (FitWindow==aLength) {
    aFName <- c(paste0(pref, "INTERCEPT"), paste0(pref, "SLOPE"))
  }
  xFitWindow <- 0:(FitWindow-1)
  FitWindow <- seq(from = aLength-FitWindow+1, length.out = FitWindow)
  
  dft <- df[ , aName[FitWindow]]
  #dft <- abs(dft)
  tmp <- 1:nrow(dft)
  tmp <- t(sapply(tmp, function(y) coef(lm(unlist(dft[y, ]) ~ xFitWindow)) ))
  for (i in 1:2) {
    df[[aFName[i]]] <- tmp[, i]
  }
  return(df)
}
#------------------------------------------------
start_time <- Sys.time()
df <- MyFitWindow(df, pref= "ANTALL_", FitWindow = 12)
df <- MyFitWindow(df, pref= "SUM_", FitWindow = 12)
df <- MyFitWindow(df, pref= "TRAN_", FitWindow = 12)
end_time <- Sys.time()
cat("Calculation time of Linear Fit:", format(end_time-start_time), "\n")


start_time <- Sys.time()
df <- MyFitWindow(df, pref= "ANTALL_", FitWindow = 3)
df <- MyFitWindow(df, pref= "SUM_", FitWindow = 3)
df <- MyFitWindow(df, pref= "TRAN_", FitWindow = 3)
end_time <- Sys.time()
cat("Calculation time of Linear Fit for Window:", format(end_time-start_time), "\n")

#===========================================

################################################
#END of Analysis
################################################

#Save prepared dataframe into R-file
#=========================================
flname <- "dataframe_prep.rds"
#saveRDS(df, file=flname)
#=========================================



################################################
#SLICING and SAMPLING
################################################

## Data Slicing
#==========================================
require("caret")
set.seed(12)
sindex <- createDataPartition(y = df$STATUS, p= 0.7, list = FALSE)
dfw <- df[-sindex,]   #Test data-frame
df_gtest <- dfw
df <- df[sindex,]     #Training data-frame
df_gtrain <- df
#==========================================



# #UNDERSAMPLING
# #########################################################
# #Reduce size in order to ballance data
# #==========================================
# sindex <- df$IS_OPEN=="0"
# cat("Number of closed accountd:", sum(sindex), "\n")
# 
# dft <- subset(df, !sindex)
# set.seed(1)
# tmp <- sample(1:nrow(dft), sum(sindex))
# dft <- rbind(subset(df, sindex), dft[tmp, ])
# 
# #Permutations
# set.seed(2)
# tmp <- sample(1:nrow(dft))
# dft <- dft[tmp, ]
# 
# 
# df <- dft
# #Choose train set as undersampling of 70% and test set as 30% of unballanced
# df_gtrain <- df
# #df.train <- df
# #df.test <- df_gtest
# #==========================================
# 
# 
# 
# 
# #SMOTE
# ########################################################################
# #==========================================
# require("DMwR")
# #df <- df_gtrain
# 
# #Set AS_FIRM
# df$AS_FIRM <- ifelse(df$ALDER==999,"1", "0")
# df$ALDER[df$ALDER==999] <- 0
# 
# feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, sName)
# 
# feature_chr <- c("BK_KJONN_KODE", "BK_SIVILSTAND_KODE", "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", 
#                  "BK_POSTSTED_KODE", "AS_FIRM")
# 
# #Convert to factor
# for (i in feature_chr) {
#   df[[i]] <- as.factor(df[[i]])
# }
# feature_nm <- c(feature_num, feature_chr)
# label_nm <- "IS_OPEN"
# 
# df <- df[c(feature_nm, label_nm)]
# 
# set.seed(21)
# #dft <- SMOTE(IS_OPEN ~ ., df, perc.over = 400, perc.under=125, k = 5)
# #dft <- SMOTE(IS_OPEN ~ ., df, perc.over = 100, perc.under=200, k = 5)
# #dft <- SMOTE(IS_OPEN ~ ., df, perc.over = 100, perc.under=1300, k = 5)
# dft <- SMOTE(IS_OPEN ~ ., df, perc.over = 100, perc.under=1300, k = 3)
# 
# print(table(dft$IS_OPEN))
# cat("Relative numbers (balanced) of IS_OPEN accounts:\n")
# print(prop.table(table(dft$IS_OPEN)))
# 
# #As Integer
# #----------------------------------------------
# cName <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName)
# for (i in cName) {
#   dft[[i]] <- as.integer(round(dft[[i]]))
# }
# for (i in sName) {
#   dft[[i]] <- round(dft[[i]], digits = 2)
# }
# #----------------------------------------------
# 
# #Return ALDER=999 for FIRM back
# dft$ALDER[dft$AS_FIRM=="1"] <- 999
# dft$AS_FIRM <- NULL
# 
# #Check selvconsistent
# #-----------------------------------------------
# for (i in 1:12) {
#   sindex <- dft[[aName[i]]]==0 # & dft[[sName[i]]]!=0
#   dft[sindex, sName[i]] <- 0 #Set zero in SUM if ANTALL is zero
# }
# #-----------------------------------------------
# 
# 
# df <- dft
# #Find DELTA
# ###################
# #Calculate DELTA or Window FIT
# #===========================================
# dft <- df
# dft$DELTA_SUM <- dft$SUM_12-dft$SUM_11
# dft$DELTA_ANTALL <- dft$ANTALL_12-dft$ANTALL_11
# dft$DELTA_TRAN <- dft$TRAN_12-dft$TRAN_11
# 
# cName <- c("ANTALL_", "SUM_", "TRAN_")
# start_time <- Sys.time()
# for (i in cName) {
#   cat("Start of Linear Fit calculation for ", i, "...\n")
#   dft <- MyFitWindow(dft, pref= i, FitWindow = 12)
#   dft <- MyFitWindow(dft, pref= i, FitWindow = 3)
# }
# end_time <- Sys.time()
# cat("Calculation time of Linear Fit:", format(end_time-start_time), "\n")
# df <- dft
# #===========================================
# 
# 
# #Choose train set
# df_gtrain <- df
# #df.train <- df
# #df.test <- df_gtest
# #==========================================

################################################
#END of SLICING and SAMPLING
################################################



#####################################################################
# Machine Learning
#####################################################################
df <- df_gtrain
## Data Slicing for ML
#==========================================
set.seed(123)
sindex <- createDataPartition(y = df$IS_OPEN, p= 0.7, list = FALSE)
df.train <- df[sindex,]
df.test <- df[-sindex,]
#==========================================


#Choose main feature-columns
#------------------------------------------
#Numeric names
#feature_num <- c("ALDER", "DAYS_BEING_CUSTOMER", aName, sName, "TRAN_INTERCEPT", "TRAN_SLOPE", "ANTALL_INTERCEPT", "ANTALL_SLOPE")
feature_num <- c("ALDER", "DAYS_BEING_CUSTOMER", aName, sName, "ANTALL_SLOPE")

#Character names
feature_chr <- c("BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "BK_POSTSTED_KODE")

#Combine all feature names
feature_nm <- c(feature_num, feature_chr)

#Label name
label_nm <- "IS_OPEN"
#------------------------------------------


#Choose features
df.train <- df.train[c(feature_nm, label_nm)]
df.test <- df.test[c(feature_nm, label_nm)]


# Create model weights (they also can sum to one)
#========================================================
myWeights <- ifelse(df.train$IS_OPEN == "0",
                    (1/(table(df.train$IS_OPEN)[1])) * 0.5,
                    (1/(table(df.train$IS_OPEN)[2])) * 0.5
)
print(table(df.train$IS_OPEN))
myWeights <- 1e+5*myWeights
cat("Weights unique values:", unique(myWeights), "\n")
cat("Sum of all weights:", sum(myWeights), "\n")
sumwpos <- sum(myWeights * (df.train$IS_OPEN == "0"))
sumwneg <- sum(myWeights * (df.train$IS_OPEN == "1"))
#========================================================



#XGBoost - eXtreme Gradient Boosting
################################################
require(xgboost)
#Define control set
#-----------------------------------------------
dft <- df_gtest[feature_nm]
tmp <- sapply(df_gtest[[label_nm]], function(x) as.numeric(levels(x))[x])
dcontrol <- xgb.DMatrix(data = data.matrix(dft), label = tmp)
#-----------------------------------------------

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
################################################


# Predic labels and construct Confusion Matrix
#-----------------------------------------------
label = getinfo(dcontrol, "label")
pred.prob <- predict(myModel, dcontrol)
pred.label <- as.integer(pred.prob > 0.5)
confusionMatrix(table(pred.label, label))
confusionMatrix(table(ifelse(pred.label=="0",1,0), ifelse(label=="0",1,0)), positive = "1")
#-----------------------------------------------


#Importance matrix
#-----------------------------------------------
importance_matrix <- xgb.importance(feature_names = feature_nm, model = myModel)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
#-----------------------------------------------


# AUC calculations
#-----------------------------------------------
require("pROC")
auc <- roc(label, pred.prob)
print(auc$auc)
#print(roc(label, pred.prob))
plot.roc(label, pred.prob, col = "blue", print.auc = TRUE, print.auc.col = "red", grid = TRUE)
#-----------------------------------------------


#####################################################################
# END of Machine Learning
#####################################################################


#####################################################################
# Write results in file
#####################################################################
# Start writing to an output file
flname <- "Analysis_output.txt"
sink(file = flname, append = TRUE)
cat("Date:", as.character.Date(Sys.time()), "\n\n")
#cat(sprintf("x has %d elements:\n", length(x)))
print(confusionMatrix(table(pred.label, label)))
print(auc$auc)

cat("===================================================================\n")
print(importance_matrix)
cat("###################################################################\n")
# Stop writing to the file
sink()

#####################################################################
