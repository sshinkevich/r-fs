###########################################################################
# LOAD R-file
###########################################################################
rm(list = ls())
getwd()
setwd("C:/Projects/FS-handelbanken/FS-R/Data/")
flname <- "dataframe_full.rds"
flname <- "dataframe_full_lm.rds"

#Import data.farme as RDS-file
#==========================================
df <- readRDS(flname)


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

dfw <- subset(df, IS_OPEN=="1")
#==============================================
tDate <-seq(as.Date("01/01/2017", format='%d/%m/%Y'), as.Date("01/12/2017", format='%d/%m/%Y'), by = "month")
aName <- FormColName(tDate, pref = "ANTALL_")
sName <- FormColName(tDate, pref = "SUM_")
#==============================================
#==============================================
aNameN <- paste0("ANTALL_", 1:12)
sNameN <- paste0("SUM_", 1:12)
#==============================================
dft <- dfw[, aNameN]-dfw[, aName]
cat("Sum of residuals for ANTALL (OPEN):", sum(abs(dft)), "\n")
dft <- dfw[, sNameN]-dfw[, sName]
cat("Sum of residuals for SUM (OPEN):", sum(abs(dft)), "\n")

############################################################################################



# #Analys of CLOSED
# ############################################################################################
# #dfw <- subset(df, IS_OPEN=="0" & CLOSED_DATE!="")
# dfw <- subset(df, IS_OPEN=="0")
# 
# tDate <- as.Date(dfw$CLOSED_DATE, format='%d/%m/%Y')
# dfw$DAYS_BEING_OPEN <- as.integer(tDate - as.Date("01/01/1970", format='%d/%m/%Y')) - dfw$DAYS_START_DATO
# # dfw$DAYS_CLOSED_DATE <- as.integer(tDate - as.Date("01/01/1970", format='%d/%m/%Y'))
# # dfw$DAYS_BEING_OPEN <- dfw$DAYS_CLOSED_DATE - dfw$DAYS_START_DATO
# 
# 
# dfw$IS_LESS_365 <- as.factor(ifelse(dfw$DAYS_BEING_OPEN < 365,"1", "0"))
# hist(dfw$DAYS_BEING_OPEN[dfw$IS_LESS_365=="1"], breaks = 30.5*(0:12) )
# ############################################################################################


#Update 



dfw <- subset(df, IS_OPEN=="0" & grepl("/2016$", CLOSED_DATE, ignore.case = TRUE))
#==============================================
tDate <-seq(as.Date("01/01/2017", format='%d/%m/%Y'), as.Date("01/12/2017", format='%d/%m/%Y'), by = "month")
aName <- FormColName(tDate, pref = "ANTALL_")
sName <- FormColName(tDate, pref = "SUM_")
#==============================================
dft <- dfw[, c(aName, "CLOSED_DATE", aNameN)]

#sindex <- dfw[,aName]!=0
#dft <- dfw[sindex, c(aName, "CLOSED_DATE")]

#sum(dft[, aName])

dft <- dfw[,aName]
#sum(dft)
sindex <- dft!=0
cat("Number of still alive accounts in 2017 which where closed in 2016:", sum(sindex), "\n")

tmp <- dfw[sindex[,1], ]

#tmp <- grepl("/2016$", df$CLOSED_DATE, ignore.case = TRUE)


###############################################################################################
#Export to CSV for AZURE
###############################################################################################

#==============================================
aNameN <- paste0("ANTALL_", 1:12)
sNameN <- paste0("SUM_", 1:12)
#==============================================
cName <- c("ALDER", "BK_KJONN_KODE", "BK_SIVILSTAND_KODE", "ANTALL_BARN", "BK_ANSVARSTED_KODE",
           "BK_GEOGRAFI_KODE", "BK_POSTSTED_KODE", "NUMBER_BK_KONTO_NR", "IS_FIRM", "IS_OPEN", 
           "DAYS_BEING_CUSTOMER", 
           aNameN, sNameN, "ANTALL_INTERCEPT", "ANTALL_SLOPE")
#Export without IS_DEAD
dft <- subset(df, IS_DEAD=="0", select = cName)

#str(dft)
#summary(dft)
#Export file
#==========================================
flname <- "data_azure.csv"
write.csv(dft, file = flname, row.names=FALSE)
