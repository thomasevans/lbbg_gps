# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to extract data from RAW text files produced
# by igotu2gpx. Parsing the text and then outputint to file.
# It uses the functions created in 'parse_igotu2gpx_txt.R' to do this.
# Further as data is extracted from each file the parent file name
# and device ID will be added as columns


# First list files from directory matching some file naming criteria
# These uses regular expressions (pattern option) to search for
# file names matching criteria - confusing how this works, but 
# following gets files with g???txt, where ? is anything.
# This will return the files such as g01.txt
files <- list.files(path = 
                      "D:/Dropbox/guillemot_2014_data/igotu2gpx_files",
                    pattern = "g...txt",
                    all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE)

# Vector of device IDs
fun.wrap <- function(x){
  strsplit(x, split = ".txt" )[[1]][1]}
devices <- sapply(X = files, FUN = fun.wrap)
names(devices) <- NULL


# Source functions
source("parse_igotu2gpx_txt.R")

# Parse all files
n <- length(files)

parse.list <- list()

for(i in 1:n){
  x <- parse.file(paste(
    "D:/Dropbox/guillemot_2014_data/igotu2gpx_files/",
    files[i], sep = ""))
  x <- cbind(x,devices[i],files[i])
  parse.list[[i]] <- x
}

# Combine all into single data frame
points.df <- do.call("rbind", parse.list)

# Rename some fields
names(points.df)[12] <- "device_info_serial"
names(points.df)[13] <- "file_name"

# str(points.df)

# Write to database
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

# Remove points that don't have a date_time
points.df.f <- points.df[!is.na(points.df$date_time),]

# Get data in order
points.df.f <- points.df.f[order(points.df.f$device_info_serial,
                                 points.df.f$date_time),]

# Re-order df columns
cols <- names(points.df.f)
first.cols <- c("device_info_serial",
                "date_time")
f <- cols %in% first.cols
points.df.f.ordered <- points.df.f[,c(first.cols,cols[!f])]
row.names(points.df.f.ordered) <- NULL

#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, points.df.f.ordered,
        tablename = "guillemots_gps_points_igu",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
        )

