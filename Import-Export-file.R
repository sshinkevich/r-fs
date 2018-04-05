rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "churn_version2.txt"

#Import txt-file
#==========================================
df <- read.csv2(flname, header=TRUE, stringsAsFactors=FALSE)



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
#==========================================

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
#tmp <- subset(df, is.na(BK_LAND_KODE))


# Set zeros instead of NA
#===============================================
dft <- df
dft[is.na.data.frame(dft)] <- 0
df <- dft
cat("Is there any NA?:", anyNA(df), "\n")
#===============================================



#Convert to integer
#==========================================
#print(sort(unique(df$ALDER)))
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
#dft <- subset(df, sindex)
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
  "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "IS_OPEN", "IS_FIRM", "IS_DEAD") 
) {
  for (t in vName) {
    x[[t]] <- as.character(x[[t]])
  }
  return(x)
}
dft <- ConToChar(df)
df <- dft
#==========================================




#Function format column name
#==============================================
MyMonthsListName <- c("JAN","FEB","MARS",
              "APRIL","MAI","JUNI",
              "JULI","AUG","SEPT",
              "OKT","NOV","DES")
FormColName <- function(d, pref="") {
  mNa <- as.integer(format(d, format = "%m"))
  tmp <- paste0(pref, MyMonthsListName[mNa], "_", format(d, format = "%Y"))
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
#tDate <- seq.Date(as.Date("01/01/2017", format='%d/%m/%Y'), by = "month", length.out = 12)
tDate <- seq.Date(as.Date("01/01/2016", format='%d/%m/%Y'), by = "month", length.out = 12)#We use 2016 year to be sure in behaviour of open accounts
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



# #Remove columns
# #==========================================
# dft <- df
# vName = c("SK_KUNDE_SCD_ID","ETTERNAVN", "FORNAVN", "ADRESSELINJE_1", "POSTSTED_NAVN", "BK_POSTSTED_KODE"
#           #, "FODSELS_DATO"
# )
# for (t in vName) {
#   dft[[t]] <- NULL
# }
# df <- dft
# #==========================================



#Reorder columns
#==========================================
vName = c("KUNDE_NR", "BK_KONTO_NR", "ALDER", "FODSELS_DATO", "BK_KJONN_KODE", "BK_SIVILSTAND_KODE", 
          "ANTALL_BARN", "BK_LAND_KODE", "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "KUNDE_START_DATO",
          "ETTERNAVN", "FORNAVN", "ADRESSELINJE_1", "POSTSTED_NAVN", "BK_POSTSTED_KODE")
ReoderCol <- function(xdf, vName) {
  all.nm <- colnames(xdf)
  tmp <- sapply(vName, function(x){grep(paste0("^" ,x , "$"), all.nm, ignore.case = TRUE)}, simplify = TRUE, USE.NAMES = FALSE)
  if (!is.vector(tmp, mode = "integer")) {
    print("Error in COLNAMES!")
    #print(is.vector(tmp, mode = "integer"))
    print(tmp)
    return(NULL)}
  dft <- cbind(xdf[, tmp], xdf[, -tmp])
  return(dft)
}
dft <- ReoderCol(df, vName)
if (!is.null(dft)) df <- dft
#str(dft)
#summary(dft)
#==========================================

#Export file
#==========================================
flname <- "data_import-export.csv"
write.csv2(df, file = flname, row.names=FALSE) #, na="")