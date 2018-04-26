rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "dataframe.rds"
#flname <- "dataframe_lm.rds"
flname <- "dataframe_full.rds"
#flname <- "dataframe_undersampling.rds"
#flname <- "dataframe-normalized.rds"

#Import data.farme as RDS-file
#==========================================
df <- readRDS(flname)

#==========================================
aName <- paste0("ANTALL_", 1:12)
sName <- paste0("SUM_", 1:12)
#==========================================


#Skip ############################
#dfw <- df #To analayse "dataframe_full.rds"
# dfw <- subset(df, STATUS!="CLOSED" & ALDER < 150)
# dfw <- subset(df, STATUS!="CLOSED" & IS_FIRM=="0")
# dfw <- subset(df, STATUS!="CLOSED")

dfw <- subset(df, IS_OPEN=="1" & ALDER!=999)
dfw <- subset(df, IS_OPEN=="1")
hist(dfw$ALDER)

dfw <- subset(df, IS_OPEN=="0" & ALDER!=999)
dfw <- subset(df, IS_OPEN=="0")
dfw <- subset(df, IS_OPEN=="1" & DAYS_BEING_CUSTOMER < 365)
#hist(dfw$ALDER)

#


#Sum
dft <- dfw[, sName]

dft <- abs(dfw[, sName])
sindex <- dft == 0
dft[sindex] <- 0.01
dft <- log10(dft)

#Antall
#dft <- sapply(dfw, function(x) x[, sName]/x[, aName])
dft <- dfw[, aName]


#Ratio
dft <- dfw[, aName]
sindex <- dft == 0
dft[sindex] <- 0.1
dft <- dfw[, sName]/dft
dft <- abs(dft)

#
dft <- subset(df, IS_OPEN=="1" & IS_DEAD=="0")[, sName]
dft <- subset(df, IS_OPEN=="0" & IS_FIRM=="0" & IS_DEAD=="0")[, sName]

boxplot(dft,
        xlab = "Date", ylab = "Antall",
        main = "ANTALL vs. Date"
)
tmp <- sapply(dft, mean)
tmp0 <- sapply(dft, median)
plot(tmp, type = "b", main = "Mean Antall")
plot(tmp0, type = "b", main = "Median Antall")
tmp0 <- sapply(dft, sd)
plot(tmp0, type = "b", main = "Standart deviation Antall")

#dfw <- subset(df, STATUS!="CLOSED" & IS_FIRM=="0")
#pairs(df[, c("ANTALL_12", "ANTALL_11")], col = df$STATUS, upper.panel = NULL, pch = 16, cex = 0.5)
pairs(df[, c("ANTALL_INTERCEPT", "ANTALL_SLOPE")], col = factor(df$STATUS), pch = 19, cex = 0.6)
pairs(df[, aName], col = df$STATUS, upper.panel = NULL, pch = 16, cex = 0.5)

dft <- prcomp(df[, c("ANTALL_INTERCEPT", "ANTALL_SLOPE")], center = FALSE, scale. = FALSE)
print(dft)
plot(dft$x[,1:2], col = factor(df$STATUS), pch = 19, cex = 0.6, alpha = 0.3)


library(ggplot2)
p <- ggplot(df, aes(ANTALL_INTERCEPT, ANTALL_SLOPE))
p + geom_point(aes(colour = df$STATUS), alpha = 1/8, size=1, stroke = 1)

p <- ggplot(dft$x, aes(PC1, PC2))
p + geom_point(aes(colour = df$STATUS), alpha = 1/8, size=1, stroke = 1)

# 
# yy <- unlist(dft[1,])
# #xx <- 1:12
# #plot(tmp, type = "b", main = "Mean Antall")
# plot(yy, type = "b", main = "y(x)")
# #lModel <- lm(xx ~ yy)
# lModel <- lm(yy ~ c(1:12))
# 
# lModel <- lm(unlist(dft[1,]) ~ c(1:12))
# 
# 
# abline(reg = lModel, col = "red")
# lcoeff <- coef(lModel)
# print(lcoeff)
# 
# MyFunction <- function(d) {
#   yy <- unlist(d)
#   lModel <- lm(yy ~ c(1:12))
#   return(coef(lModel))
# }
# MyFunction(dft[1:2, ])


#dft <- head(dfw[, aName], 10)
#tmp <- sapply(1:10, function(y) sapply(dft[y, ],  function(x) coef(lm(unlist(x) ~ c(1:12))) ))

#################################################
#Calculation Linear Fit
#################################################
#dft <- dfw[ , aName]
dft <- df[ , aName] #For all
tmp <- 1:nrow(dft)
start_time <- Sys.time()
tmp <- t(sapply(tmp, function(y) coef(lm(unlist(dft[y, ]) ~ c(1:12))) ))
end_time <- Sys.time()
cat("Calculation time of Linear Fit:", format(end_time-start_time), "\n")

#dim(tmp)
summary(tmp)

dft <- df
dft$ANTALL_INTERCEPT <- tmp[, 1]
dft$ANTALL_SLOPE <- tmp[, 2]
df <- dft

#Keep results in temporary variable
df.total <- df
#df <- df.total


#=========================================
#Convert DATA_CLOSED
#sindex <- df$IS_OPEN=="0"

#dft <- df[as.Date("31/12/2017", format='%d/%m/%Y')-df$DAYS_START_DATO < 0, ]

tDate <-as.Date(ifelse(df$CLOSED_DATE!="", df$CLOSED_DATE, "31/12/2017"), format='%d/%m/%Y')
#tDate <-as.Date(df$CLOSED_DATE, format='%d/%m/%Y')

df$DAYS_BEING_CUSTOMER <- as.integer(tDate - as.Date("01/01/1970", format='%d/%m/%Y')) - df$DAYS_START_DATO
#df$DAYS_BEING_OPEN <- as.integer(tDate - as.Date("01/01/1970", format='%d/%m/%Y')) - df$DAYS_START_DATO
# dfw$DAYS_CLOSED_DATE <- as.integer(tDate - as.Date("01/01/1970", format='%d/%m/%Y'))
# dfw$DAYS_BEING_OPEN <- dfw$DAYS_CLOSED_DATE - dfw$DAYS_START_DATO

sindex <- df$DAYS_BEING_CUSTOMER <=0
cat("Removing items with negative DAYS_BEING_CUSTOMER:", sum(sindex), "\n")
dft <- df[!sindex, ]
df <- dft

#dfw$IS_LESS_365 <- as.factor(ifelse(dfw$DAYS_BEING_OPEN < 365,"1", "0"))
# hist(df$DAYS_BEING_CUSTOMER[df$DAYS_BEING_CUSTOMER <= 365 & df$IS_OPEN=="0"], breaks = 30.5*(0:12) ,
#      xlab = "Month", main = "Histogram of DAYS_BEING_CUSTOMER")

ggplot(data=df[df$IS_OPEN=="0", ], aes(DAYS_BEING_CUSTOMER)) + 
  geom_histogram(aes(fill=..count..), breaks=30.5*(0:12), colour = "gray")

# ggplot(data=df[df$IS_OPEN=="1", ], aes(DAYS_BEING_CUSTOMER)) + 
#   geom_histogram(aes(y =..density..), breaks=30.5*(0:12), colour = "gray", alpha = 0.5) +
#   geom_density(colour = "red") + xlim(0, 366)

ggplot(data=df[df$IS_OPEN=="1", ], aes(DAYS_BEING_CUSTOMER)) + 
  geom_histogram(aes(y =..density..), colour = "gray", alpha = 0.5) +
  geom_density(colour = "red") + labs(title="Histogram for OPEN")

ggplot(data=df[df$IS_OPEN=="0", ], aes(DAYS_BEING_CUSTOMER)) + 
  geom_histogram(aes(y =..density..), colour = "gray", alpha = 0.5) +
  geom_density(colour = "red") + labs(title="Histogram for CLOSED")


#Save to a file
#=========================================
flname <- "dataframe_lm.rds"
flname <- "dataframe_full_lm.rds"
flname <- "dataframe_full2_lm.rds"
#flname <- "dataframe_full_abs_lm.rds"
#flname <- "dataframe_full_tran.rds"
saveRDS(df, file=flname)
#=========================================



################################################################################################
################################################################################################
################################################################################################
rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")

#==========================================
aName <- paste0("ANTALL_", 1:12)
sName <- paste0("SUM_", 1:12)
tName <- paste0("TRAN_", 1:12)
#==========================================


#Load file
flname <- "dataframe_full_lm.rds"
flname <- "dataframe_full2_lm.rds"
flname <- "dataframe_full_tran.rds"
#Import data.farme as RDS-file
#==========================================
df <- readRDS(flname)

#Remove IS_DEAD
#==========================================
df <- subset(df, IS_DEAD=="0")
df$IS_DEAD <- NULL
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
#==========================================
df <- SetTRAN(df, as_abs = TRUE)

# #Set ABS(TRAN)
# #------------------------------------------
# df[ ,tName] <- abs(df[ ,tName])
# #------------------------------------------


#Set SUM as ABS(SUM) and find DELTA - Linear Fit
###################
#===========================================
#df[ ,sName] <- abs(df[ ,sName])
df$DELTA_SUM <- df$SUM_12-df$SUM_11
df$DELTA_ANTALL <- df$ANTALL_12-df$ANTALL_11
df$DELTA_TRAN <- df$TRAN_12-df$TRAN_11


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

  # aName <- paste0("ANTALL_", 1:aLength)
  # sName <- paste0("SUM_", 1:aLength)
  # tName <- paste0("TRAN_", 1:aLength)
  # #New names of INTERCEPT and SLOPE
  # aFName <- c("ANTALL_WINDOW_INTERCEPT", "ANTALL_WINDOW_SLOPE")
  # sFName <- c("SUM_WINDOW_INTERCEPT", "SUM_WINDOW_SLOPE")
  # tFName <- c("TRAN_WINDOW_INTERCEPT", "TRAN_WINDOW_SLOPE")
  # if (FitWindow==aLength) {
  #   aFName <- c("ANTALL_INTERCEPT", "ANTALL_SLOPE")
  #   sFName <- c("SUM_INTERCEPT", "SUM_SLOPE")
  #   tFName <- c("TRAN_INTERCEPT", "TRAN_SLOPE")
  # }
  # 
  # xFitWindow <- 0:(FitWindow-1)
  # FitWindow <- seq(from = aLength-FitWindow+1, length.out = FitWindow)
  # 
  # #For ANTALL
  # dft <- df[ , aName[FitWindow]] #For Fit Window
  # tmp <- 1:nrow(dft)
  # tmp <- t(sapply(tmp, function(y) coef(lm(unlist(dft[y, ]) ~ xFitWindow)) ))
  # 
  # # df$ANTALL_WINDOW_INTERCEPT <- tmp[, 1]
  # # df$ANTALL_WINDOW_SLOPE <- tmp[, 2]
  # for (i in 1:2) {
  #   df[[aFName[i]]] <- tmp[, i]
  # }
  # 
  # #For SUM
  # dft <- df[ , sName[FitWindow]] #For Fit Window
  # tmp <- 1:nrow(dft)
  # tmp <- t(sapply(tmp, function(y) coef(lm(unlist(dft[y, ]) ~ xFitWindow)) ))
  # # df$SUM_WINDOW_INTERCEPT <- tmp[, 1]
  # # df$SUM_WINDOW_SLOPE <- tmp[, 2]
  # for (i in 1:2) {
  #   df[[sFName[i]]] <- tmp[, i]
  # }
  # 
  # #For TRANSACTION
  # if ("TRAN_1" %in% names(df)) {
  #   #dft <- df[ , tName[FitWindow]] #For Fit Window
  #   dft <- abs(df[ , tName[FitWindow]])
  #   tmp <- 1:nrow(dft)
  #   tmp <- t(sapply(tmp, function(y) coef(lm(unlist(dft[y, ]) ~ xFitWindow)) ))
  #   for (i in 1:2) {
  #     df[[tFName[i]]] <- tmp[, i]
  #   }
  # }
#   return(df)
# }

# FitWindow <- 3
# FitWindow <- seq(from = length(aName)-FitWindow+1, length.out = FitWindow)
# dft <- df[ , aName[FitWindow]] #For Fit Window
# tmp <- 1:nrow(dft)
# tmp <- t(sapply(tmp, function(y) coef(lm(unlist(dft[y, ]) ~ FitWindow)) ))
# 
# #dim(tmp)
# summary(tmp)
# 
# df$ANTALL_WINDOW_INTERCEPT <- tmp[, 1]
# df$ANTALL_WINDOW_SLOPE <- tmp[, 2]

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


df.total <- df
#df <- df.total



#===========================================
###################


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



#########################################################
#UNDERSAMPLING
#Reduce size in order to ballance data
#########################################################
sindex <- df$IS_OPEN=="0"
cat("Number of closed accountd:", sum(sindex), "\n")


#dfw <- subset(df, IS_OPEN=="0")
#dft <- rbind(subset(df, sindex), head(subset(df, !sindex), 9000))
dft <- subset(df, !sindex)
set.seed(1)
tmp <- sample(1:nrow(dft), sum(sindex))
dft <- rbind(subset(df, sindex), dft[tmp, ])
#Permutations
set.seed(2)
tmp <- sample(1:nrow(dft))
dft <- dft[tmp, ]

df <- dft
df_mytrain <- df
#Choose train set as undersampling of 70% and test set as 30% of unballanced
#df.train <- df
#df.test <- dfw
df.train <- df_mytrain
df.test <- df_gtest
############################################################



require("DMwR")

######################################################################################
#SMOTE
######################################################################################
#flname <- "dataframe_full_lm.rds"
df <- df_gtrain

sum(df$ALDER==999)
#sum(df$IS_FIRM=="1")
df$AS_FIRM <- ifelse(df$ALDER==999,"1", "0")
#sum(df$AS_FIRM=="1")
df$ALDER[df$ALDER==999] <- 0
#tmp <- subset(df, IS_FIRM=="1")

#Remove non-relevant and calculated columns
#Choose feature-columns
#feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_START_DATO", aName, sName)
#feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_START_DATO", "NUMBER_BK_KONTO_NR", aName, sName,"ANTALL_INTERCEPT", "ANTALL_SLOPE")
feature_num <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName, sName)

#feature_chr <- c("BK_KJONN_KODE", "BK_SIVILSTAND_KODE", "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "IS_FIRM")
feature_chr <- c("BK_KJONN_KODE", "BK_SIVILSTAND_KODE", "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", 
                 "BK_POSTSTED_KODE", "AS_FIRM")
#Convert to factor
for (i in feature_chr) {
  df[[i]] <- as.factor(df[[i]])
}
feature_nm <- c(feature_num, feature_chr)
label_nm <- "IS_OPEN"

#colnames(df)
df <- df[c(feature_nm, label_nm)]
#colnames(df)

# print(table(df$IS_OPEN))
# cat("Relative numbers of IS_OPEN accounts:\n")
# print(prop.table(table(df$IS_OPEN)))

set.seed(21)
#dft <- SMOTE(IS_OPEN ~ ., df, perc.over = 400, perc.under=125, k = 5)
#dft <- SMOTE(IS_OPEN ~ ., df, perc.over = 100, perc.under=200, k = 5)
dft <- SMOTE(IS_OPEN ~ ., df, perc.over = 100, perc.under=1300, k = 5)

print(table(dft$IS_OPEN))
cat("Relative numbers (balanced) of IS_OPEN accounts:\n")
print(prop.table(table(dft$IS_OPEN)))

# tmp <- sapply(dft, is.factor)
# dft[[tmp]] <- lapply(dft[[tmp]], as.character)



#df <- dft
#dft <- df
#cName <- c("ALDER", "ANTALL_BARN", "DAYS_START_DATO", aName)
cName <- c("ALDER", "ANTALL_BARN", "DAYS_BEING_CUSTOMER", aName)
for (i in cName) {
  dft[[i]] <- as.integer(round(dft[[i]]))
}
for (i in sName) {
  dft[[i]] <- round(dft[[i]], digits = 2)
}

#Return ALDER=999 for FIRM back
dft$ALDER[dft$AS_FIRM=="1"] <- 999
dft$AS_FIRM <- NULL

#Check selvconsistent
#-----------------------------------------------
for (i in 1:12) {
  sindex <- dft[[aName[i]]]==0 # & dft[[sName[i]]]!=0
  dft[sindex, sName[i]] <- 0 #Set zero in SUM if ANTALL is zero
}
#tmp <- dft[sindex, c("SUM_12", "ANTALL_12")]


df <- dft
#Find DELTA
###################
#DELTA or Window FIT
#===========================================
df$DELTA_SUM <- df$SUM_12-df$SUM_11
df$DELTA_ANTALL <- df$ANTALL_12-df$ANTALL_11
df <- MyFitWindow(df, FitWindow = 3)
df <- MyFitWindow(df, FitWindow = 12)
#===========================================
###################

#Calculation of Linear Fit
#################################################
dft <- df[ , aName] #For all
tmp <- 1:nrow(dft)
tmp <- t(sapply(tmp, function(y) coef(lm(unlist(dft[y, ]) ~ c(1:12))) ))

dft <- df
dft$ANTALL_INTERCEPT <- tmp[, 1]
dft$ANTALL_SLOPE <- tmp[, 2]
df <- dft
#################################################

#Choose train set as oversampling part of 70% and test set as 30% of unballanced
df_mytrain <- df
df <- df_mytrain

df.train <- df_mytrain
df.test <- df_gtest

#df.train <- df_gtrain
df <- df.train


#Analyse of SUM-trend
###########################################################
###########################################################
require("ggplot2")
require("gridExtra")
df <- df_gtrain

#dfw <- subset(df, IS_OPEN=="1" & ALDER!=999)
if ("IS_DEAD" %in% names(df)) {
  message("Removing IS_DEAD")
  df <- subset(df, IS_DEAD=="0")
  df$IS_DEAD <- NULL
}



# #Make new categorical value for accounts
# df$HOW_LONG_BEING_CUSTOMER <- as.factor(ifelse(df$DAYS_BEING_CUSTOMER <= 30, "1M",
#                                                 ifelse(df$DAYS_BEING_CUSTOMER <= 91, "3M",
#                                                        ifelse(df$DAYS_BEING_CUSTOMER < 183, "6M",
#                                                               ifelse(df$DAYS_BEING_CUSTOMER < 274, "9M",
#                                                                      ifelse(df$DAYS_BEING_CUSTOMER <= 365, "1Y", "MY"))))))

#Make new categorical value for accounts
df$HOW_LONG_BEING_CUSTOMER <- as.factor(ifelse(df$DAYS_BEING_CUSTOMER <= 91, "0-3M",
                                               ifelse(df$DAYS_BEING_CUSTOMER < 183, "3-6M",
                                                      ifelse(df$DAYS_BEING_CUSTOMER < 274, "6-9M",
                                                             ifelse(df$DAYS_BEING_CUSTOMER <= 365, "9M-12M", ">1Y")))))



table(df$HOW_LONG_BEING_CUSTOMER)
print("How long being customer for OPEN accounts:")
table(subset(df, IS_OPEN=="1", select = HOW_LONG_BEING_CUSTOMER))
print("How long being customer for CLOSED accounts:")
table(subset(df, IS_OPEN=="0", select = HOW_LONG_BEING_CUSTOMER))

#dfw <- subset(df, IS_OPEN=="1")
dfw <- df

#==============================================================
MyMovingAverage <- function(x, pref = "ANTALL_", order = 2) {
  df <- x
  cLength <- sum(grepl(paste0("^", pref, "[[:digit:]]"), colnames(df), ignore.case = TRUE))
  if (cLength==0) {
    stop(paste0("Cannot find length of ", pref, " column names"))
  }
  if (order > cLength) {
    stop("Order of filter is bigger than data length")
  }
  cName <- paste0(pref, 1:cLength)
  nName <- paste0(pref, 1:(cLength-order+1))
  #Remove old all names in data frame
  dft <- df[ , !(names(df) %in% cName)]
  #dft <- df[, cName]
  for (i in 1:(cLength-order+1)) {
    tmp <- df[[cName[i]]]
    for (j in 1:order) {
      if (j>1) {
        tmp <- tmp + df[[cName[i+j-1]]]
      }
    }
    dft[[nName[i]]] <- tmp/order
  }
  return(dft)
}

dfw <- MyMovingAverage(dfw, pref = "ANTALL_", order = 2)
dfw <- MyMovingAverage(dfw, pref = "SUM_", order = 2)
#===============================================================
#==========================================
aLength <- sum(grepl("^ANTALL_[[:digit:]]", colnames(dfw), ignore.case = TRUE))
aName <- paste0("ANTALL_", 1:aLength)
sName <- paste0("SUM_", 1:aLength)
#==========================================

#dft <- dfw[, sName]


# boxplot(dft,
#         xlab = "Date", ylab = "Sum",
#         main = "SUM vs. Date"
# )
# plot(sapply(dft, mean), type = "b", main = "Mean SUM") 
# plot(sapply(dft, median), type = "b", main = "Median SUM")
# plot(sapply(dft, sd), type = "b", main = "Standart deviation of SUM")

require("reshape2")

tmp <- melt(dfw, id.vars = "IS_OPEN", measure.vars=aName)
p1 <- ggplot(data = tmp, aes(x=variable, y=value, group = IS_OPEN, colour = IS_OPEN)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("ANTALL vs. Month") +
  xlab("Month") + ylab("ANTALL") +
  #stat_summary(fun.y = mean, geom = 'ribbon', fun.ymax = uci, fun.ymin = lci, alpha = 0.5, fill = 'lightblue') + 
  theme_minimal() 

tmp <- melt(dfw, id.vars = "IS_OPEN", measure.vars=sName)
p2 <- ggplot(data = tmp, aes(x=variable, y=value, group = IS_OPEN, colour = IS_OPEN)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("SUM") +
  theme_minimal()

#grid.arrange(p1, p2, nrow = 2, ncol=1)
grid.arrange(p1, p2, ncol=1)


#tmp <- melt(dfw, id.vars = "IS_OPEN", measure.vars=sName)
p3 <- ggplot(data = tmp, aes(x=variable, y=abs(value), group = IS_OPEN, colour = IS_OPEN)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("ABS(SUM)") +
  theme_minimal()

grid.arrange(p2, p3, ncol=1)


#Box plots
#####################################################################
#-------------------------------------------------
tmp <- melt(dfw, id.vars = "IS_OPEN", measure.vars=aName)
p1 <- ggplot(data = tmp, aes(x=variable, y=abs(value)+1, colour = IS_OPEN)) +
  geom_boxplot(width =0.5, fill = "lightgray") +
  xlab("Month") + ylab("ANTALL") +
  stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
  theme_minimal() + scale_y_log10()


tmp <- melt(dfw, id.vars = "IS_OPEN", measure.vars=sName)
p2 <- ggplot(data = tmp, aes(x=variable, y=abs(value)+1, colour = IS_OPEN)) +
  geom_boxplot(width =0.5, fill = "lightgray") +
  xlab("Month") + ylab("ABS(SUM)") +
  stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
  theme_minimal() + scale_y_log10()

grid.arrange(p1, p2, ncol=1)


#tmp <- melt(dfw, id.vars = "IS_OPEN", measure.vars=sName)
tmp0 <- subset(tmp, value>=0)
p1 <- ggplot(data = tmp0, aes(x=variable, y=value+1, colour = IS_OPEN)) +
  geom_boxplot(width =0.5, fill = "lightgray") +
  xlab("Month") + ylab("SUM > 0") +
  stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
  theme_minimal() + scale_y_log10()

tmp0 <- subset(tmp, value<0)
p2 <- ggplot(data = tmp0, aes(x=variable, y=-value+1, colour = IS_OPEN)) +
  geom_boxplot(width =0.5, fill = "lightgray") +
  xlab("Month") + ylab("SUM <0 ") +
  stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
  theme_minimal() + scale_y_log10()

grid.arrange(p1, p2, ncol=1)

ggplot(data = tmp, aes(x=variable, y=value, colour = IS_OPEN)) +
  geom_boxplot(width =0.5, fill = "lightgray") +
  xlab("Month") + ylab("SUM") +
  stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
  coord_cartesian(ylim = c(-5000,8500)) + theme_minimal()

# tmp0 <- 1-min(tmp$value)
# ggplot(data = tmp, aes(x=variable, y=value+tmp0, colour = IS_OPEN)) +
#   geom_boxplot(width =0.5, fill = "lightgray") +
#   xlab("Month") + ylab("SUM") +
#   stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
#   scale_y_log10() + coord_cartesian(ylim = tmp0+c(-1e5,1e5)) + theme_minimal()

#--------------------------------------------------------------------

#Transactions
dft <- dfw
tName <- paste0("TRAN_", 1:length(aName))
#tName <- paste0("TRAN_", 1:12)
for (i in 1:length(tName)) {
  #dft[[paste0("TRAN_", i)]] <- dft[[sName[i]]]/dft[[aName[i]]]
  dft[[tName[i]]] <- dft[[sName[i]]]/dft[[aName[i]]]
}
# tmp <- melt(dft, id.vars = "IS_OPEN", measure.vars=tName)
# ggplot(data = tmp, aes(x=variable, y=abs(value), group = IS_OPEN, colour = IS_OPEN)) + 
#   stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
#   #ggtitle("SUM vs. Month") +
#   xlab("Month") + ylab("Transaction") +
#   theme_minimal()

for (i in tName) {
  dft[is.na(dft[,i]),i] <- 0
}
tmp <- melt(dft, id.vars = "IS_OPEN", measure.vars=tName)


p1 <- ggplot(data = tmp, aes(x=variable, y=value, group = IS_OPEN, colour = IS_OPEN)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("Transaction") +
  theme_minimal()

p2 <- ggplot(data = tmp, aes(x=variable, y=abs(value), group = IS_OPEN, colour = IS_OPEN)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("ABS(Transaction)") +
  theme_minimal()

grid.arrange(p1, p2, ncol=1)


#Box plot
#-------------------------------------------------------------------
ggplot(data = tmp, aes(x=variable, y=abs(value)+1, colour = IS_OPEN)) +
  geom_boxplot(width =0.5, fill = "lightgray") +
  xlab("Month") + ylab("ABS(Transaction)") +
  stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
  theme_minimal() + scale_y_log10()

ggplot(data = tmp, aes(x=variable, y=value, colour = IS_OPEN)) +
  geom_boxplot(width =0.5, fill = "lightgray") +
  xlab("Month") + ylab("Transaction") +
  stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
  coord_cartesian(ylim = c(-150,250)) + theme_minimal()

# ggplot(data = tmp, aes(x=variable, y=value, colour = IS_OPEN)) +
#   geom_boxplot(width =0.5, fill = "lightgray") +
#   xlab("Month") + ylab("ANTALL") +
#   stat_summary(aes(group=IS_OPEN), fun.y = mean, geom = 'line', size = 1.1) +
#   theme_minimal() #+ scale_y_log10()


# 
# is.nan.data.frame <- function(x) {
#   do.call(cbind, lapply(x, is.nan))
# }
# is.nan.data.frame <- NULL
# stat_smooth(
#   color = "#FC4E07", fill = "#FC4E07",
#   method = "loess"
# )+

#START By Period
##########################################################################################
#------------------------------------------------------------------------------------------
#dfw <- subset(df, IS_OPEN=="1")
dfw <- df

tmp <- melt(dfw, id.vars = c("IS_OPEN", "HOW_LONG_BEING_CUSTOMER"), measure.vars=aName)
#tmp <- melt(dfw, id.vars = "HOW_LONG_BEING_CUSTOMER", measure.vars=aName)
p1 <- ggplot(data = subset(tmp, IS_OPEN=="1"), aes(x=variable, y=value, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("ANTALL vs. Month") +
  xlab("Month") + ylab("ANTALL - OPEN") +
  #stat_summary(fun.y = mean, geom = 'ribbon', fun.ymax = uci, fun.ymin = lci, alpha = 0.5, fill = 'lightblue') + 
  theme_minimal() + theme(axis.text.x = element_blank()) + scale_colour_discrete(name  ="Period")


p2 <- ggplot(data = subset(tmp, IS_OPEN=="0"), aes(x=variable, y=value, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("ANTALL vs. Month") +
  xlab("Month") + ylab("ANTALL - CLOSED") +
  #stat_summary(fun.y = mean, geom = 'ribbon', fun.ymax = uci, fun.ymin = lci, alpha = 0.5, fill = 'lightblue') + 
  theme_minimal() + theme(axis.text.x = element_blank()) + scale_colour_discrete(name  ="Period")

grid.arrange(p1, p2, nrow = 2, ncol=1)



tmp <- melt(dfw, id.vars = c("IS_OPEN", "HOW_LONG_BEING_CUSTOMER"), measure.vars = sName)
p1 <- ggplot(data = subset(tmp, IS_OPEN=="1"), aes(x=variable, y=value, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("SUM - OPEN") +
  theme_minimal() + scale_colour_discrete(name  ="Period")

p2 <- ggplot(data = subset(tmp, IS_OPEN=="0"), aes(x=variable, y=value, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("SUM - CLOSED") +
  theme_minimal() + scale_colour_discrete(name  ="Period")


grid.arrange(p1, p2, nrow = 2)



#tmp <- melt(dfw, id.vars = c("IS_OPEN", "HOW_LONG_BEING_CUSTOMER"), measure.vars = sName)
p1 <- ggplot(data = subset(tmp, IS_OPEN=="1"), aes(x=variable, y=abs(value), group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("ABS(SUM) - OPEN") +
  theme_minimal() + scale_colour_discrete(name  ="Period")

p2 <- ggplot(data = subset(tmp, IS_OPEN=="0"), aes(x=variable, y=abs(value), group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("ABS(SUM) - CLOSED") +
  theme_minimal() + scale_colour_discrete(name  ="Period")


grid.arrange(p1, p2, nrow = 2)


# p1 <- ggplot(data = subset(tmp, IS_OPEN=="1"), aes(x=variable, y=log10(abs(value)+1), group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
#   stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
#   #ggtitle("SUM vs. Month") +
#   xlab("Month") + ylab("ABS(SUM) - OPEN") +
#   theme_minimal() + scale_colour_discrete(name  ="Period") #+ scale_y_log10() #+ coord_trans(y = "log10")
# 
# p2 <- ggplot(data = subset(tmp, IS_OPEN=="0"), aes(x=variable, y=log10(abs(value)+1), group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
#   stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
#   #ggtitle("SUM vs. Month") +
#   xlab("Month") + ylab("ABS(SUM) - CLOSED") +
#   theme_minimal() + scale_colour_discrete(name  ="Period") #+ scale_y_log10() #+ coord_trans(y = "log10")

p1 <- ggplot(data = subset(tmp, IS_OPEN=="1"), aes(x=variable, y=abs(value)+1, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) +
  stat_summary(fun.y = mean, geom = "line", size = 1.1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("ABS(SUM) - OPEN") +
  theme_minimal() + scale_colour_discrete(name  ="Period") + scale_y_log10() #+ coord_trans(y = "log10")

p2 <- ggplot(data = subset(tmp, IS_OPEN=="0"), aes(x=variable, y=abs(value)+1, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) +
  stat_summary(fun.y = mean, geom = "line", size = 1.1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("ABS(SUM) - CLOSED") +
  theme_minimal() + scale_colour_discrete(name  ="Period") + scale_y_log10() #+ coord_trans(y = "log10")

grid.arrange(p1, p2, nrow = 2)



#Transactions
dft <- dfw
for (i in 1:length(tName)) {
  dft[[tName[i]]] <- dft[[sName[i]]]/dft[[aName[i]]]
}
for (i in tName) {
  dft[is.na(dft[,i]),i] <- 0
}
tmp <- melt(dft, id.vars = c("IS_OPEN", "HOW_LONG_BEING_CUSTOMER"), measure.vars=tName)
p1 <- ggplot(data = subset(tmp, IS_OPEN=="1"), aes(x=variable, y=value, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("Transaction - OPEN") +
  theme_minimal() + scale_colour_discrete(name  ="Period")

p2 <- ggplot(data = subset(tmp, IS_OPEN=="0"), aes(x=variable, y=value, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("Transaction - CLOSED") +
  theme_minimal() + scale_colour_discrete(name  ="Period")

grid.arrange(p1, p2, ncol=1)


p1 <- ggplot(data = subset(tmp, IS_OPEN=="1"), aes(x=variable, y=abs(value)+1, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("ABS(Transaction) - OPEN") +
  theme_minimal() + scale_colour_discrete(name  ="Period") + scale_y_log10()

p2 <- ggplot(data = subset(tmp, IS_OPEN=="0"), aes(x=variable, y=abs(value)+1, group = HOW_LONG_BEING_CUSTOMER, colour = HOW_LONG_BEING_CUSTOMER)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  #ggtitle("SUM vs. Month") +
  xlab("Month") + ylab("ABS(Transaction) - CLOSED") +
  theme_minimal() + scale_colour_discrete(name  ="Period") + scale_y_log10()

grid.arrange(p1, p2, ncol=1)

#END of by Period
#################################################################################
#--------------------------------------------------------------------


################################################################################
#df <- df_gtest

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="ANTALL_SLOPE")
#tmp$value <- scale(tmp$value)
p1 <- ggplot(data = tmp, aes(x=variable, y=value, group = IS_OPEN, colour = IS_OPEN)) + 
  geom_boxplot(outlier.shape = NA, fill = 'lightgray') +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  coord_cartesian(ylim = c(-2.5,3.5)) + theme_minimal() +
  theme(plot.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="SUM_SLOPE")
p2 <- ggplot(data = tmp, aes(x=variable, y=value, group = IS_OPEN, colour = IS_OPEN)) + 
  geom_boxplot(outlier.shape = NA, fill = 'lightgray') +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  coord_cartesian(ylim = c(-900,1000)) + theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
p2
print(aggregate(value ~ IS_OPEN, data = tmp, mean))

#grid.arrange(p1, p2, ncol=1)

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="ANTALL_INTERCEPT")
p3 <- ggplot(data = tmp, aes(x=variable, y=value, group = IS_OPEN, colour = IS_OPEN)) + 
  geom_boxplot(outlier.shape = NA, fill = 'lightgray') +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  coord_cartesian(ylim = c(-80,150)) + theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
p3

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="SUM_INTERCEPT")
p4 <- ggplot(data = tmp, aes(x=variable, y=value, group = IS_OPEN, colour = IS_OPEN)) + 
  geom_boxplot(outlier.shape = NA, fill = 'lightgray') +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  coord_cartesian(ylim = c(-15000,27000)) + theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
p4

#grid.arrange(p3, p4, ncol=1)

grid.arrange(p1, p3, ncol=1)
grid.arrange(p2, p4, ncol=1)

#-----------------------------------------------------------------------------

#Historgrams

################################################################################
tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="ANTALL_SLOPE")
#tmp0 <- aggregate(value ~ IS_OPEN, data = tmp, mean)
tmp0 <- merge(aggregate(value ~ IS_OPEN, data = tmp, mean), aggregate(value ~ IS_OPEN, data = tmp, median), by = "IS_OPEN")
p1 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 0.5) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-15, 15)) + 
  #geom_density(alpha=.2) +
  theme_minimal() + xlab(unique(tmp$variable))
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="ANTALL_INTERCEPT")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p2 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 4) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-10, 160)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p2
grid.arrange(p1, p2, ncol=1)



tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="SUM_SLOPE")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p1 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 50) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-1500, 1500)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="SUM_INTERCEPT")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p2 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 400) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-10000, 15000)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p2
grid.arrange(p1, p2, ncol=1)


tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="TRAN_SLOPE")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p1 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 5) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-150, 150)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="TRAN_INTERCEPT")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p2 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 50) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-1000, 1000)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p2
grid.arrange(p1, p2, ncol=1)

#Window
#----------------------------------------------------
tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="ANTALL_WINDOW_SLOPE")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p1 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 0.5) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-15, 15)) + 
  #geom_density(alpha=.2) +
  theme_minimal() + xlab(unique(tmp$variable))
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="ANTALL_WINDOW_INTERCEPT")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p2 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 3) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-5, 150)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p2
grid.arrange(p1, p2, ncol=1)


tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="SUM_WINDOW_SLOPE")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p1 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 500) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-15000, 15000)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="SUM_WINDOW_INTERCEPT")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p2 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 500) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(-12000, 12000)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p2
grid.arrange(p1, p2, ncol=1)



#SUM antall for 12-month
#-----------------------

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="ANTALL_12")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p1 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 3) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(0, 150)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="SUM_12")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p2 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 500) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  coord_cartesian(xlim = c(0, 25000)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p2

grid.arrange(p1, p2, ncol=1)


#Transaction
#----------------------------------------
tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="TRAN_12")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p1 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 30) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  #coord_cartesian(xlim = c(0, 2700)) + 
  coord_cartesian(xlim = c(-1000, 1000)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="TRAN_11")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p2 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 20) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  #coord_cartesian(xlim = c(0, 1500)) + 
  coord_cartesian(xlim = c(-800, 800)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p2

#DELTA
tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="DELTA_ANTALL")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p1 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 5) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  #coord_cartesian(xlim = c(0, 2700)) + 
  coord_cartesian(xlim = c(-100, 100)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p1

tmp <- melt(df, id.vars = "IS_OPEN", measure.vars="DELTA_SUM")
tmp0 <- merge(aggregate(value ~ IS_OPEN, tmp, mean), aggregate(value ~ IS_OPEN, tmp, median), by = "IS_OPEN")
p2 <- ggplot(data = tmp, aes(x=value, group = IS_OPEN, colour = IS_OPEN, fill = IS_OPEN)) + 
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.1, binwidth = 400) +
  geom_vline(data = tmp0, aes(xintercept=value.x, colour = IS_OPEN), linetype="dashed", size=1) +
  geom_vline(data = tmp0, aes(xintercept=value.y, colour = IS_OPEN), linetype="solid", size=0.5, alpha = 0.7) +
  #coord_cartesian(xlim = c(0, 2700)) + 
  coord_cartesian(xlim = c(-5000, 9000)) + 
  theme_minimal() + xlab(unique(tmp$variable))
p2

###################################################################################################

tmp <- melt(dft)
ggplot(data = tmp, aes(x=variable, y=value)) + 
  stat_summary(aes(group=1),fun.y = mean, geom = "line", colour = 'blue', size = 1.1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", col = "darkgray", width = 0.2) +
  #stat_summary(fun.y = mean, geom = 'ribbon', fun.ymax = uci, fun.ymin = lci, alpha = 0.5, fill = 'lightblue') + 
  theme_minimal()


# ggplot(data = tmp, aes(x=variable, y=log10(abs(value)))) + 
#   geom_boxplot(aes(fill=variable)) +
#   stat_summary(aes(group=1), fun.y = mean, geom = 'line', colour = 'blue') + 
#   theme_minimal() 
#   #+ coord_trans(y="log10") 
# 
# 
# ggplot(data = tmp, aes(x=variable, y=value)) + 
#   #geom_boxplot(aes(fill=variable), outlier.shape = NA) +
#   stat_summary(aes(group=1), fun.y = mean, geom = 'line') 
# 
# 
# tmp <- melt(abs(dft))
# ggplot(data = tmp, aes(x=variable, y=value)) + 
#   geom_boxplot(aes(fill=variable), outlier.shape = NA) +
#   stat_summary(aes(group=1), fun.y = mean, geom = 'line', colour = 'lightblue') 







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
# 
# #========================================
# dft <- data.frame(Normalize.Tanh(dfw[, sName]))
# boxplot(dft,
#         xlab = "Date", ylab = "Sum",
#         main = "Tanh - SUM vs. Date"
# )
# plot(sapply(dft, mean), type = "b", main = "Mean SUM")
# summary(dft)
# 
# 
# tmp <- melt(dft)
# 
# ggplot(data = tmp, aes(x=variable, y=value)) + 
#   geom_boxplot(aes(fill=variable)) +
#   stat_summary(aes(group=1), fun.y = mean, geom = 'line', colour = 'blue') + 
#   theme_minimal()











# ggplot(melt(dft), aes(x = SUM_1)) +
#   geom_boxplot(colour = "blue", outlier.colour="red")

require("forecast")
#tmp <- as.ts(abs(dft[1,]), frequency = 12)
tsData <- ts(dft[1, ], frequency = 1)

tmp <- ma(tsData[1:12], order = 2, centre = TRUE)

plot.ts(cbind(tsData[1:12], tmp))

# ggplot(tsData, aes(data = tsData)) + 
#   geom_line(size = 1)
