rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "churn_preloaded.csv"

#Import txt-file
#==========================================
df <- read.csv2(flname, header=TRUE, stringsAsFactors=FALSE)

#colnames(df)

#Convert to factor
#==========================================
ConToFactor <- function(x, vName = c("STATUS","BK_KJONN_KODE", "BK_LAND_KODE", "BK_SIVILSTAND_KODE") ) {
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


# Remove DØDSBO
#==========================================
#Remove all DØDSBO
sindex <- df$IS_DEAD
cat("Remove number of DØDSBO:", sum(sindex))
dft <- subset(df, !sindex)
dft$IS_DEAD <- NULL
df <- dft
#==========================================



#Convert Date field
#==========================================
ConToDate <- function(x, vName = c("CLOSED_DATE","KUNDE_START_DATO") ) {
  for (t in vName) {
    #x[[t]] <- as.Date(x[[t]], format='%d/%m/%Y')
    tmp <- trimws(x[[t]])
    tmp <- tmp != ""
    #tmp <- tmp != "" & tmp !="01/01/1970" #If we want to remove undefined start date
    #x[[t]][tmp] <- as.Date(x[[t]][tmp], format='%d/%m/%Y')
    tmp1 <- rep(NA, length(x[[t]])) #Set NA - vector for future DATE
    tmp1[tmp] <- as.Date(x[[t]][tmp], format='%d/%m/%Y')
    x[[paste0("RDATE_", t)]] <- tmp1 #Add column with R-date format in the data.frame
  }
  return(x)
}
dft <- ConToDate(df)
df <- dft
#==========================================


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
