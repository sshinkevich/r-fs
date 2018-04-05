rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "dataframe.rds"
flname <- "dataframe_lm.rds"
#flname <- "dataframe_undersampling.rds"
#flname <- "dataframe-normalized.rds"

#Import data.farme as RDS-file
#==========================================
df <- readRDS(flname)

#==========================================
aName <- paste0("ANTALL_", 1:12)
sName <- paste0("SUM_", 1:12)
#==========================================


# dfw <- subset(df, STATUS!="CLOSED" & ALDER < 150)
# dfw <- subset(df, STATUS!="CLOSED" & IS_FIRM=="0")
# dfw <- subset(df, STATUS!="CLOSED")

dfw <- subset(df, IS_OPEN=="1" & ALDER!=999)
dfw <- subset(df, IS_OPEN=="1")
hist(dfw$ALDER)

dfw <- subset(df, IS_OPEN=="0" & ALDER!=999)
dfw <- subset(df, IS_OPEN=="0")
#hist(dfw$ALDER)


#Sum
dft <- dfw[, sName]

dft <- abs(dfw[, sName])
sindex <- dft == 0
dft[sindex] <- 0.01
dft <- log10(dft)

#Antall
#dft <- sapply(dfw, function(x) x[, sName]/x[, aName])
dft <- dfw[, sName]


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
dft <- dfw[ , aName]
dft <- df[ , aName]
tmp <- 1:nrow(dft)
start_time <- Sys.time()
tmp <- t(sapply(tmp, function(y) coef(lm(unlist(dft[y, ]) ~ c(1:12))) ))
end_time <- Sys.time()
cat("Calculation of Linear Fit time:", format(end_time-start_time), "\n")

dim(tmp)
summary(tmp)

dft <- df
dft$ANTALL_INTERCEPT <- tmp[, 1]
dft$ANTALL_SLOPE <- tmp[, 2]
df <- dft

df.total <- df
#df <- df.total
#=========================================
flname <- "dataframe_lm.rds"
saveRDS(df, file=flname)
#=========================================


## Data Slicing
#==========================================
set.seed(12)
sindex <- createDataPartition(y = df$STATUS, p= 0.7, list = FALSE)
dfw <- df[-sindex,]
df <- df[sindex,]
#==========================================

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

#Choose train set as undersampling of 70% and test set as 30% of unballanced
#df.train <- df
#df.test <- dfw

############################################################




