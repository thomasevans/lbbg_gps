# Analysis of dive-bouts from 2009 guillemot GPS + TDR study

setwd("D:/Dropbox/R_projects/lbbg_gps")

# Import dive_bout files ------
# Set working-directory to file location
setwd("D:/Documents/Work/Karlso/2009_diving_analysis/dive_files/fixed_bottom/bouts_only")

headers = c("id","start_date","start_time","n_dives","x","x1","x2","x3","x4", "dive_freq", "bout_duration", "bottom_time","tsb.boutdur","duration_dive_max","duration_dive_mean","depth_max","depth_max_mean", "depth_mean", "bottom_time_mean","dive_time_total","integral", "int.dur_bout","dive_eff","x5","x6","x7","x8", "descent_rate.mean","descent_rate.mean2","ascent_rate.mean","ascent_rate.mean2","x9","x10")
# length(headers)

aak966 <- read.table("AAK966_flg_Bout.csv", header=F, sep = ",", skip=2,col.names= headers)
aak959 <- read.table("AAK959edit_flg_Bout.csv", header=F, sep = ",", skip=2,col.names= headers)
aak961 <- read.table("AAK961_flg_Bout_final.csv", header=F, sep = ",", skip=2,col.names= headers)
aak962 <- read.table("AAK962_flg_Bout.csv", header=F, sep = ",", skip=2,col.names= headers)
aak965 <- read.table("AAK965_flg_Bout.csv", header=F, sep = ",", skip=2,col.names= headers)
aak960 <- read.table("AAK960_flg_Bout.csv", header=F, sep = ",", skip=2,col.names= headers)
# Note that this bird exceeded it's tag depth limit
aak963 <- read.table("AAK963_flg_Bout.csv", header=F, sep = ",", skip=2,col.names= headers)

# Re-set working directory
setwd("D:/Dropbox/R_projects/lbbg_gps")

# Combine into single table -----
# For each add columns with device_info_serial, and ring_number
aak966 <- cbind(aak966,"AAK966")
aak959 <- cbind(aak959,"AAK959")
aak961 <- cbind(aak961,"AAK961")
aak962 <- cbind(aak962,"AAK962")
aak965 <- cbind(aak965,"AAK965")
aak960 <- cbind(aak960,"AAK960")
aak963 <- cbind(aak963,"AAK963")

n <- length(names(aak966))
name.vector <- c(names(aak966)[-n],
                 "ring_number")

names(aak966) <- name.vector
names(aak959) <- name.vector
names(aak961) <- name.vector
names(aak962) <- name.vector
names(aak965) <- name.vector
names(aak960) <- name.vector
names(aak963) <- name.vector


# Combine all into a single dataframe
all.dive.bouts <- rbind(aak966,
                    aak959,
                    aak961,
                    aak962,
                    aak965,
                    aak960,
                    aak963)


# Rebuild corrected table -------
# Fix date-times etc.


# Date_time
# combine date and time into a character vector
date_time <- paste(all.dive.bouts$start_date,
                   all.dive.bouts$start_time, sep = " ")
# Convert to date-time object
date_time_start <-  as.POSIXct(strptime(date_time,
                                  format = "%d/%m/%Y %H:%M:%S",
                                  tz = "UTC"))

date_time_end <- date_time_start + all.dive.bouts$bout_duration



# Combine to new data.frame
bouts.export.tab <- cbind.data.frame(
  all.dive.bouts$ring_number,
  date_time_start,
  date_time_end,
  all.dive.bouts$n_dives,
  all.dive.bouts[,10:23],
  all.dive.bouts[,28:31]
)

nam <- names(bouts.export.tab)
nam[1] <- "ring_number"
nam[4] <- "n_dives"
nam[8] <- "tsb_boutdur"
nam[19:22] <- c("descent_rate_mean", "descent_rate_mean2",
                "ascent_rate_mean","ascent_rate_mean2" )

names(bouts.export.tab) <- nam


# Remove duplicates
# See number of duplicates:
summary(duplicated(bouts.export.tab))

# Remove them
bouts.export.tab <- bouts.export.tab[!duplicated(bouts.export.tab[,1:2]),]



# Export table to database table -----

# Write to database
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, bouts.export.tab,
        tablename = "guillemots_dive_bouts_2009",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time_start = "datetime",
                      date_time_end = "datetime")
)

