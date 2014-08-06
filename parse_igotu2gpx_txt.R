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




