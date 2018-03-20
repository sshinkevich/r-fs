rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "churn_version2.txt"

#Import txt-file
#==========================================
df <- read.csv2(flname, header=TRUE, stringsAsFactors=FALSE)

#print(head(df, n=2 ))
#print(str(df))


#Check empty fields for STATUS
#==========================================
sindex <- toupper(df$STATUS) %in% c("OPEN","CLOSED")
cat("Number of non-correct STATUS-items:", sum(!sindex), "\n")
df <- subset(df, sindex)
cat("%% of OPEN vs. CLOSED accounts: ", sum(df$STATUS=="OPEN")/length(df$STATUS), "vs.", 
    sum(df$STATUS=="CLOSED")/length(df$STATUS) )
#==========================================



#unique(df$BK_KJONN_KODE)    ##"K" "M" "-"&
#sindex <- df$BK_KJONN_KODE == "-"
#cat("Number of non-defined BK_KJONN_KODE:", sum(sindex), "\n")
#==========================================

#unique(df$BK_LAND_KODE)
#unique(df["BK_LAND_KODE"])
#print(sum(df$BK_LAND_KODE=="RU", na.rm = TRUE))

#print(sum(is.na(df$BK_LAND_KODE)))

#df$BK_LAND_KODE[is.na(df$BK_LAND_KODE)]
#df[["BK_LAND_KODE"]][is.na(df["BK_LAND_KODE"])]
#df[[t]] <- as.numeric(df[[t]])


#Check NA values in items
#==========================================
all.nm <- names(df)
for (t in all.nm) {
  tmp <- sum(is.na(df[[t]]))
  if (tmp>0) cat("Number of NA in", t, ": ", tmp, "\n")
  #if (tmp>0 & is.numeric(df[[t]])) 
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

#Convert to factor
#==========================================
# ConToFactor <- function(x, vName = c("STATUS","BK_KJONN_KODE", "BK_LAND_KODE", "BK_SIVILSTAND_KODE", 
#                                      "BK_ANSVARSTED_KODE") ) {
#   for (t in vName) {
#     #x[[t]][is.na(x[[t]])] <- ""
#     x[[t]] <- as.factor(x[[t]])
#   }
#   return(x)
# }
# dft <- ConToFactor(df)
# df <- dft
# #print(str(df))
#==========================================


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
#cat("Number of companies (ALDER > 150):", sum(df$ALDER >= 150), "\n")
#dft <- subset(df, ALDER >= 150 & BK_KJONN_KODE == "-")

sindex <- df$ALDER >= 150 | trimws(df$FORNAVN) == ""
#sindex <- trimws(df$FORNAVN) == ""
#sum(xor(df$ALDER >= 150, trimws(df$FORNAVN) == "")) #ALDER is better feature then FORNAVN
cat("Number of companies (ALDER):", sum(sindex), "\n")

#Setting flag as IsFirm
df$IS_FIRM <- as.integer(sindex)

tmp <- paste(df$ETTERNAVN, df$FORNAVN, df$ADRESSELINJE_1)
#sindex <- grepl("^AS | AS$", tmp, ignore.case = TRUE) && !df$IS_FIRM
sindex <- grepl(" AS$|^AS ", tmp, ignore.case = TRUE) & df$IS_FIRM == 0
cat("Number of AS in ADRESSE:", sum(sindex))
#dft <- subset(df, sindex)

#Setting flag as IsFirm
df$IS_FIRM <- as.integer(sindex)

#dft <- df[sample(nrow(dft), 500), ]
#==========================================


#DØDSBO
#==========================================
#sindex <- grepl("DØDSBO", df$ETTERNAVN, ignore.case = TRUE)
#sindex <- grepl("DØDSBO", df$FORNAVN, ignore.case = TRUE)
#sindex <- grepl("DØDSBO", df$ADRESSELINJE_1, ignore.case = TRUE)

tmp <- paste(df$ETTERNAVN, df$FORNAVN, df$ADRESSELINJE_1)
sindex <- grepl("DØDSBO", tmp, ignore.case = TRUE)
cat("Number of DØDSBO:", sum(sindex))

#dft <- subset(df, sindex)
#summary(dft)
#Setting flag as DØDSBO
df$IS_DEAD <- as.integer(sindex)

# DØDSBO Remove -----------
#Remove all DØDSBO
# dft <- subset(df, !tmp)
# df <- dft
#==========================================

#Check and remove SK_KUNDE_SCD_ID
#==========================================
cat("NB: Number of non-unique accounts:", length(df$BK_KONTO_NR)-length(unique(df$BK_KONTO_NR)), "\n")
#cat("NB: Number of non-unique accounts:", sum(duplicated(df$BK_KONTO_NR)), "\n")
cat("NB: Number of non-unique users:", length(df$KUNDE_NR)-length(unique(df$KUNDE_NR)), "\n")
#cat("NB: Number of non-unique users:", sum(duplicated(df$KUNDE_NR)), "\n")
cat("NB: Number of non-unique SK_KUNDE_SCD_ID:", length(df$SK_KUNDE_SCD_ID)-length(unique(df$SK_KUNDE_SCD_ID)), "\n")

# tmp <- paste0(df$KUNDE_NR, df$SK_KUNDE_SCD_ID)
# cat("NB: Number of non-unique KUNDE_NR + SK_KUNDE_SCD_ID:", length(tmp)-length(unique(tmp)), "\n")

tmp <- duplicated(df$KUNDE_NR)
t <- duplicated(df$SK_KUNDE_SCD_ID)
#If XOR = 0 then we have complet correlation
cat("NB: XOR of dublicated KUNDE_NR and SK_KUNDE_SCD_ID:", sum(xor(tmp, t)), "\n") 


cat("NB: Number of unique accounts vs. users:", length(unique(df$BK_KONTO_NR)), "vs.", length(unique(df$KUNDE_NR)), "\n")

#dft <- subset(df, tmp)



#Remove columns
#==========================================
dft <- df
vName = c("SK_KUNDE_SCD_ID","ETTERNAVN", "FORNAVN", "ADRESSELINJE_1", "POSTSTED_NAVN", "BK_POSTSTED_KODE"
          #, "FODSELS_DATO"
          )
for (t in vName) {
  dft[[t]] <- NULL
}
df <- dft
#==========================================

#Reorder columns
#==========================================
vName = c("BK_KONTO_NR", "KUNDE_NR", "ALDER", "BK_KJONN_KODE", "BK_SIVILSTAND_KODE", 
          "BK_LAND_KODE", "BK_ANSVARSTED_KODE", "BK_GEOGRAFI_KODE", "ANTALL_BARN", "KUNDE_START_DATO")
ReoderCol <- function(xdf, vName) {
  all.nm <- colnames(xdf)
  tmp <- sapply(vName, function(x){grep(x, all.nm, ignore.case = TRUE)}, simplify = TRUE, USE.NAMES = FALSE)
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
flname <- "churn_preloaded.csv"
write.csv2(df, file = flname, row.names=FALSE) #, na="")

