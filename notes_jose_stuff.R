test <- read.table ("C:/Users/Tom/Dropbox/jose_shearwaters/test10min.txt", head = T)

length (test)

length (test$Latitude)



new15min <- test[seq(1, (length (test$Latitude)), by = 3), ] ### remuestreo que pasa de 5 a 15 minutos

new10min <- test[seq(1, (length (test$Latitude)), by = 2), ] ### remuestreo que pasa de 5 a 10 minutos





test10min <- read.table ("D:\\Dropbox\\TRACKING_DDBB\\test folder\\test10min.txt", head = T)



test10min <- subset (GPS_movevar, Tracking_event == "VE6140497_14072011_24072011_c")


#Load package
library(adehabitatLT)

#Get test data
test10min <- read.table ("C:/Users/Tom/Dropbox/jose_shearwaters/test10min.txt", head = T)

#Make vector with combined date and time. This will be character formatted
test_date_time_txt <- paste(test10min$Date, " ", test10min$Time, sep = "")
#View first 10 lines to check if it looks sensible.
test_date_time_txt[1:10]

#Convert character formated date-time to POSIXct format (as requested by as.ltraj function. 
test_date_time_posix <- as.POSIXct(test_date_time_txt, format = "%d/%m/%Y %H:%M:%S", tz = "gmt")
#View first 10 lines
test_date_time_posix[1:10]

#View help information for function
?as.ltraj

#Create ltraj object. Requires an id column, this can be the identity of the animal. I have just set as '1'.
#I also swapped the order of Latitude and Longitude. It should be X-Y, X should be Longitude, and Y Latitude.
test10min.traj <- as.ltraj (xy = test10min [, c( "Longitude","Latitude")], date = test_date_time_posix, id = 1 )
#Several warning messages are recieved. I'm not sure whether these can be ignored.


#View structure of created object
str(test10min.traj)

#View summary
summary(test10min.traj)


#Use the newly created ltraj object to make some calculates. Here first-passage time.
#Help for function
?fpt
#Calculated fpt for 4 radii. Uncertain of units here - I assume it's km.
test_fpt <- fpt(test10min.traj, radii = c(1, 10, 100, 1000))
#View structure of created object
str(test_fpt)
#Make a histogramme of FPT values for 100 km (assume it's km - could be metres perhaps!?)
hist(test_fpt[[1]]$r4)
hist(log(test_fpt[[1]]$r3))
plot(test_fpt, scale = 500, warn = FALSE)


toto <- meanfpt(test_fpt)
toto

test_fpt_var <- varlogfpt(test_fpt)
test_fpt_var

