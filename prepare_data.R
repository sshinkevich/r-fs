rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "churn_preloaded.csv"

#Import txt-file
#==========================================
df <- read.csv2(flname, header=TRUE, stringsAsFactors=FALSE)

#colnames(df)


#Convert to Char
#==========================================
ConToChar <- function(x, vName = c(#"BK_KONTO_NR","KUNDE_NR", 
  "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "IS_FIRM") 
  ) {
  for (t in vName) {
    #x[[t]][is.na(x[[t]])] <- ""
    x[[t]] <- as.character(x[[t]])
  }
  return(x)
}
dft <- ConToChar(df)
df <- dft
#print(str(df))
#==========================================


# Remove DØDSBO
#==========================================
#Remove all DØDSBO
sindex <- df$IS_DEAD
cat("Removed number of DØDSBO:", sum(sindex))
dft <- subset(df, !sindex)
dft$IS_DEAD <- NULL
df <- dft
#==========================================


# Remove IS_FIRM
#==========================================
sindex <- df$IS_FIRM=="1"
cat("Removed number of firms:", sum(sindex))
dft <- subset(df, !sindex)
dft$IS_FIRM <- NULL
df <- dft
#==========================================


# #Convert Date field
# #==========================================
# ConvToDate <- function(x, vName = c("CLOSED_DATE","KUNDE_START_DATO") ) {
#   for (t in vName) {
#     #x[[t]] <- as.Date(x[[t]], format='%d/%m/%Y')
#     tmp <- trimws(x[[t]])
#     tmp <- tmp != ""
#     #tmp <- tmp != "" & tmp !="01/01/1970" #If we want to remove undefined start date
#     #x[[t]][tmp] <- as.Date(x[[t]][tmp], format='%d/%m/%Y')
#     tmp1 <- rep(NA, length(x[[t]])) #Set NA - vector for future DATE
#     tmp1[tmp] <- as.Date(x[[t]][tmp], format='%d/%m/%Y')
#     x[[paste0("RDATE_", t)]] <- tmp1 #Add column with R-date format in the data.frame
#   }
#   return(x)
# }
# dft <- ConvToDate(df)
# df <- dft
# #==========================================


# Analyse 
#=============================================
#dft <- df[1:2]
#dft$KUNDE_NR <- as.factor(dft$KUNDE_NR)
# tmp <- aggregate(dft$BK_KONTO_NR, list(KUNDE = dft$KUNDE_NR), length)
# hist(tmp$x)

dft <- aggregate(BK_KONTO_NR ~ KUNDE_NR, data=df, length)
names(dft)[names(dft)=="BK_KONTO_NR"] <- "NUMBER_BK_KONTO_NR" #Rename to NUMER of aggregated values
#head(dft)
hist(dft$NUMBER_BK_KONTO_NR, freq = TRUE, col = "blue")
#hist(dft$NUMBER_BK_KONTO_NR, freq = FALSE, col = "blue")

dft <- aggregate(KUNDE_NR ~ NUMBER_BK_KONTO_NR, data=dft, length)
names(dft)[names(dft)=="KUNDE_NR"] <- "NUMBER_KUNDE_NR" #Rename to NUMER of aggregated values
print(dft)
#plot(dft$NUMBER_BK_KONTO_NR, dft$NUMBER_KUNDE_NR, type = "o", col = "red")
#==============================================


# Write number of accounts for a customer
#==============================================
dft <- aggregate(BK_KONTO_NR ~ KUNDE_NR, data=df, length)
names(dft)[names(dft)=="BK_KONTO_NR"] <- "NUMBER_BK_KONTO_NR"
dft <- merge(df, dft, by="KUNDE_NR")
cat("Is there any NA in NUMBER_BK_KONTO_NR?:", anyNA(dft$NUMBER_BK_KONTO_NR), "\n")
sort(unique(dft$NUMBER_BK_KONTO_NR))
df <- dft
#==============================================



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
# head(unique(sort(tmp)))
#dft$DAYS_START_DATO1 <- as.integer(tmp)
dft$DAYS_START_DATO <- as.integer(tmp - as.Date("01/01/1970", format='%d/%m/%Y'))
df <- dft

cat("Number of items don't having correct START DATE:", sum(df$DAYS_START_DATO==0 & df$ALDER<=37), "\n")

#dft <- aggregate(BK_KONTO_NR ~ KUNDE_START_DATO, data=df, length)
dft <- aggregate(BK_KONTO_NR ~ DAYS_START_DATO, data=df, length)
head(dft)



# dft <- aggregate(BK_KONTO_NR ~ ALDER, data=df, length)
# head(dft)
# 
# cat("Number of items don't having FODSELS_DATO:", sum(df$FODSELS_DATO==""), "\n")
# cat("Number of items don't having ADER:", sum(df$ALDER==""), "\n")
# 
# #sum(df$IS_FIRM==1)
# tmp <- df[ df$IS_FIRM, ]
# dft <- aggregate(BK_KONTO_NR ~ FODSELS_DATO, data=tmp, length)
# head(dft)
# 
# dft <- aggregate(BK_KONTO_NR ~ ALDER, data=tmp, length)
# head(dft)
# 
# tmp <- as.Date(df$FODSELS_DATO, format='%d/%m/%Y')

#==============================================

#Tips from Shoresh
#==============================================
RemoveDataClosedDate <- function(x) {
  df <- x
  sindex <-  toupper(df$STATUS) == "CLOSED"
  #dft <- subset(df, sindex)
  dft <- df[sindex, ]
  #Check that all items have CLOSED_DATE
  cat("Number of closed items don't having CLOSED DATE:", sum(dft$CLOSED_DATE==""), "\n")
  #dft <- head(dft, 30)
  
  tDate <- as.Date(dft$CLOSED_DATE, format='%d/%m/%Y')
  #toupper(format(tDate, format = "%b_%Y"))
  cName <- FormColName(tDate, pref = "")
  cName1 <- paste0("ANTALL_", cName)
  cName <- paste0("SUM_", cName)
  for(i in 1:length(cName)) {
    #print(dft[i, cName[i]], na.print = "NA")
    #cat(row.names(dft)[i], ":", dft$CLOSED_DATE[i],":", cName[i], ": ", dft[i, cName[i]],"\n")
    dft[i, cName1[i]] <- NA
    dft[i, cName[i]] <- NA
  }
  df[sindex, ] <- dft
  return(df)
}
dft <- RemoveDataClosedDate(df)
df <- dft
#==============================================



#Check NA values in items
#==========================================
if (anyNA(df)) {
  for (t in colnames(df)) {
    tmp <- sum(is.na(df[[t]]))
    if (tmp>0) cat("Number of NA in", t, ": ", tmp, "\n")
    #if (tmp>0 & is.numeric(df[[t]]))
}
} else {
  cat("Is there any NA?:", anyNA(df), "\n")
}
#==========================================


# Set zeros instead of NA
#===============================================
dft <- df
dft[is.na.data.frame(dft)] <- 0
df <- dft
cat("Is there any NA?:", anyNA(df), "\n")
#===============================================

# mx <- max(df[, aName])
# summary(df[, aName])
# 
# sum(df$IS_FIRM)
# sum(df$IS_FIRM==1 && df$STATUS=="CLOSED")
# 
# 
# dft <- df[, aName]
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "ANTALL vs. Date"
# )

# MinMax - normalization
#========================================
Normalize.MinMax<- function(x)
{
  ####normalize to [0,1]
  dft <- x
  mx <- max(dft)
  mn <- min(dft)
  dft <- (dft-mn)/(mx-mn)
  return(dft)
}
# mx <- max(df[, aName])
# mn <- 0 #min(df[, aName])
# dft <- df[, aName]
# dft <- (dft-mn)/(mx-mn)
#========================================


dft <- Normalize.MinMax(df[, aName])
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "MinMax - ANTALL vs. Date"
# )
#summary(dft)
df[, aName] <- dft
#========================================


#Normalize items by MinMax
#=========================================
cName <- c("ALDER", "ANTALL_BARN", "NUMBER_BK_KONTO_NR", "DAYS_START_DATO")
dft <- df[, cName]
#dft <- sapply(dft[, cName], Normalize.MinMax)
for (t in cName) {
  dft[[t]] <- Normalize.MinMax(df[[t]])
}
df[, cName] <- dft
#=========================================


# dft <- Normalize.MinMax(df[, sName])
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "MinMax - SUM vs. Date"
# )
# summary(dft)

#summary(df[, sName])
#hist(df[, aName[12]], probability=TRUE)


#Zscore - normalization
#========================================
Normalize.Zscore<- function(x)
{
  ####normalize to [0,1]
  dft <- x
  tmp <- unlist(dft, recursive = TRUE, use.names = FALSE)
  md <- mean(tmp)
  sdv <- sd(tmp)
  dft <- (dft-md)/sdv
  return(dft)
}
# dft <- Normalize.Zscore(df[, aName])
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "Zscore - ANTALL vs. Date"
# )
# summary(dft[, aName])

#========================================
dft <- Normalize.Zscore(df[, sName])
# boxplot(dft,
#         xlab = "Date", ylab = "SUM",
#         main = "Zscore - SUM vs. Date"
# )
# summary(dft)
df[, sName] <- dft






#Logistic - normalization
#========================================
dft[, aName] <- 1/(1+exp(-df[, aName]))
boxplot(dft,
        xlab = "Date", ylab = "Antall",
        main = "Logistic - ANTALL vs. Date"
)
#========================================

#summary(dft[, aName])

#Tanh
#========================================
Normalize.Tanh <- function(x)
{
  ####normalize to [0,1]
  dft <- x
  tmp <- unlist(dft, recursive = TRUE, use.names = FALSE)
  md <- mean(tmp)
  sdv <- sd(tmp)
  dft <- (dft-md)/sdv
  dft <- sapply(dft, function(x) tanh(x/2))
  return(dft)
}
# dft <- scale(df[, aName])
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "Scale - ANTALL vs. Date"
# )

# tmp <- unlist(df[, aName], recursive = TRUE, use.names = FALSE)
# md <- mean(tmp)
# sdv <- sd(tmp)
# dft <- df[, aName]
# dft <- (dft-md)/sdv
# dft <- sapply(dft, function(x) tanh(x/2))

dft <- Normalize.Tanh(df[, aName])
boxplot(dft,
        xlab = "Date", ylab = "Antall",
        main = "Tanh - ANTALL vs. Date"
)
summary(dft)
#========================================
# dft <- Normalize.Tanh(df[, sName])
# boxplot(dft,
#         xlab = "Date", ylab = "Antall",
#         main = "Tanh - SUM vs. Date"
# )
# summary(dft)



# tmp <- sapply(df[, aName], mean)
# plot(tmp, type="b")

#plot(tanh(seq(0, 10, by=0.1)/2), type="b")


#Convert to factor
#==========================================
ConToFactor <- function(x, vName = c("STATUS","BK_KJONN_KODE", "BK_LAND_KODE", "BK_SIVILSTAND_KODE"#, "IS_FIRM"
                                     ) ) {
  for (t in vName) {
    #x[[t]][is.na(x[[t]])] <- ""
    x[[t]] <- as.factor(x[[t]])
  }
  return(x)
}
dft <- ConToFactor(df)
df <- dft
#print(str(df))
#==========================================



#Remove columns
#==========================================
dft <- df
vName = c("KUNDE_NR", "BK_KONTO_NR", "BK_GEOGRAFI_KODE","KUNDE_START_DATO", #"IS_FIRM",
          "FODSELS_DATO", "CLOSED_DATE" )
for (t in vName) {
  dft[[t]] <- NULL
}
df <- dft
#==========================================

#dft <- subset(df, IS_FIRM == "1")
#cat("Number of items don't having correct START DATE:", sum(df$DAYS_START_DATO==0 & df$ALDER<=37), "\n")
#dft <- subset(df, DAYS_START_DATO==0 & ALDER<=37)

#=========================================
flname <- "dataframe.rds"

flname <- "dataframe-normalized.rds"
saveRDS(df, file=flname)
