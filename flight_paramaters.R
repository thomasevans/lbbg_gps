#'A script to analyse each flight, with various paramaters, such as maximum altitude.
#'First the database will be queried too pull out the data columns we needd for our analysis.
#'Second. Various paramateerrs and information about the flights will be calculated.
#'Third. This will be ouput to a new database table specifially for flights.

#'***********
#'Owing to some error in making an ODBC connection to the remote PostgreSQL Amsterdam 'flysafe' database with my Windows 8 installation, I am instead working with the binary datafile of object 'gps' produced previouly in 'flight_details'.

#save(gps, file = "gps.RData")
#'load in previousl saved GPS data (R data object)
load("gps.RData")

#'Inspect this dataframe
str(gps)

#*****************************In the eventual version, I want to do this using the database connection instead. In the mean time I use the previously saved GPS data.


