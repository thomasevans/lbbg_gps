#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.

#' Data from MoveBank for two vairables
#' Here we parse the CSV file to output to a new
#' DB table with variables
#' - date_time
#' - device_info_serial
#' - ecwf_dew_point
#' - ecwf_ppt



setwd("D:/Dropbox/LBBG_Movebank/Data_downloaded/MoveBank_data_all")                            

n <- count.fields("ECMWF_ppt_dew_point.csv", sep = ",")
head(n)
col.num <- max(n)


mycols <- rep("NULL", col.num)
mycols[c(3, 9, col.num -1, col.num)] <- NA
data.com <- read.table("ECMWF_ppt_dew_point.csv", sep = ",", colClasses = mycols, skip = 1, row.names = NULL)
# ?read.table
names(data.com) <- c("date_time", "device_info_serial",
                     "ecwf_ppt", "ecwf_dew_point")

head(data.com)


# Get datetime to correct format
data.com$datetime <- as.POSIXct(as.character(data.com$datetime), tz = "UTC")

str(data.com)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


data.com$ecwf_ppt <- as.numeric.factor(data.com$ecwf_ppt)
data.com$ecwf_dew_point <- as.numeric.factor(data.com$ecwf_dew_point)

str(data.com)
# hist(data.com$ecwf_dew_point)
# hist(data.com$ecwf_ppt)

# connection to DB
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Output to new table in the database. ----
sqlSave(gps.db, data.com, tablename = "move_bank_ppt_dew_ECMWF",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime"))
