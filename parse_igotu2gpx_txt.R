# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to extract data from RAW text files produced
# by igotu2gpx. Parsing the text and then outputint to file.

text.samp <- readLines("D:/Dropbox/guillemot_2014_data/igotu2gpx_files/g01.txt",encoding="UTF-8")

head(text.samp)


# Get an index for lines including 'Record'
index <- grep("Record", text.samp)

# View first 10 lines indexed
text.samp[index[1:10]]


# Get data for one record
sub.text  <-  text.samp[index[1]:(index[2]-1)]

# Get record number -----
rec.num <- strsplit(sub.text[1], split = "Record " )[[1]][2]
rec.num <- as.numeric(rec.num)


# Get date_time   ------
# Get index for line in sub.text with date_time
i.date <- grep("Date", sub.text)
# Just date line
date.txt <- sub.text[i.date]
# Date only
date.only <- strsplit(date.txt, split = "  Date " )[[1]][2]
# Split into date and time
date.only.split <- strsplit(date.only, split = "T" )[[1]]
# Time only part without "Z"
time.only <- strsplit(date.only.split[2], split = "Z" )[[1]][1]
# Put date and time back together into single character unit vector
date.time.only <- paste(date.only.split[1],time.only)
# Change to date_time object with UTC time-zone
date_time <- as.POSIXct(date.time.only, tz = "UTC")

# If you wanted character with full date_time including
# fractional seconds
# x <- strftime(date_time,'%Y-%m-%d %H:%M:%OS3')
# str(x)


# Get latitude ----
# Get index for line in sub.text with latitude
i.lat <- grep("Latitude", sub.text)
# Just lat line
lat.txt <- sub.text[i.lat]
# lat only
lat.only <- strsplit(lat.txt, split = "  Latitude " )[[1]][2]
lat <- as.numeric(lat.only)


# Get longitude ----
# Get index for line in sub.text with longitude
i.long <- grep("Longitude", sub.text)
# Just long line
long.txt <- sub.text[i.long]
# long only
long.only <- strsplit(long.txt, split = "  Longitude " )[[1]][2]
long <- as.numeric(long.only)


# Get elevation ----
# Get index for line in sub.text with elevation
i.elev <- grep("Elevation", sub.text)
# Just elev line
elev.txt <- sub.text[i.elev]
# elev only
elev.only <- strsplit(elev.txt, split = "  Elevation " )[[1]][2]
elev.only <- strsplit(elev.only, split = " m" )[[1]][1]
elev <- as.numeric(elev.only)



# Get speed ----
# Get index for line in sub.text with speedation
i.speed <- grep("Speed", sub.text)
# Just speed line
speed.txt <- sub.text[i.speed]
# speed only
speed.only <- strsplit(speed.txt, split = "  Speed " )[[1]][2]
speed.only <- strsplit(speed.only, split = " km/h" )[[1]][1]
speed <- as.numeric(speed.only)


# Get course ----
# Get index for line in sub.text with course
i.course <- grep("Course", sub.text)
# Just course line
course.txt <- sub.text[i.course]
# course only
course.only <- strsplit(course.txt, split = "  Course " )[[1]][2]
course.only <- strsplit(course.only, split = " degrees" )[[1]][1]
course <- as.numeric(course.only)


# Get ehpe ----
# Get index for line in sub.text with ehpe
i.ehpe <- grep("EHPE", sub.text)
# Just ehpe line
ehpe.txt <- sub.text[i.ehpe]
# ehpe only
ehpe.only <- strsplit(ehpe.txt, split = "  EHPE " )[[1]][2]
ehpe.only <- strsplit(ehpe.only, split = " m" )[[1]][1]
ehpe <- as.numeric(ehpe.only)


# Get Timeout -----
# Possibly can get times of diving by looking at this, if timeout is
# 12s with no fix appears to probably be diving.
# Get index for line in sub.text with timeout
i.timeout <- grep("Timeout", sub.text)
# Just timeout line
timeout.txt <- sub.text[i.timeout]
# timeout only
timeout.only <- strsplit(timeout.txt, split = "  Timeout " )[[1]][2]
timeout.only <- strsplit(timeout.only, split = " s" )[[1]][1]
timeout <- as.numeric(timeout.only)


# Get MSVs_QCN ----
# Get index for line in sub.text with ehpe
i.MSVs_QCN <- grep("MSVs_QCN", sub.text)
# Just MSVs_QCN line
MSVs_QCN.txt <- sub.text[i.MSVs_QCN]
# MSVs_QCN only
MSVs_QCN.only <- strsplit(MSVs_QCN.txt, split = "  MSVs_QCN " )[[1]][2]
MSVs_QCN <- as.numeric(MSVs_QCN.only)


# Satellites:



