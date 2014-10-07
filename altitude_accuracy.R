
#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Get a copy of the flights DB table.
alts <- sqlQuery(gps.db, query="SELECT DISTINCT g.v_accuracy, g.altitude, g.h_accuracy, g.positiondop
FROM gps_uva_tracking_speed_3d_limited AS g
                    ;",as.is=TRUE)




str(alts)
par(mfrow=c(1,1))
sort(alts$v_accuracy)[1:100]
rev(sort(alts$v_accuracy))[1:100]
hist(alts$v_accuracy[alts$v_accuracy < 1000])
hist(alts$v_accuracy[alts$v_accuracy < 100])
hist(alts$v_accuracy[alts$v_accuracy < 50])
sort((alts$altitude[alts$v_accuracy < 10]))[1:100]
rev(sort((alts$altitude[alts$v_accuracy < 10])))[1:100]

hist(alts$v_accuracy[alts$h_accuracy < 50])
hist(alts$positiondop[alts$h_accuracy < 50])


hist(alts$altitude[alts$v_accuracy < 20])
sort(alts$altitude[alts$v_accuracy < 20])[1:100]
rev(sort(alts$altitude[alts$v_accuracy < 20]))[1:100]

summary(alts$v_accuracy < 30)

min(alts$v_accuracy)





