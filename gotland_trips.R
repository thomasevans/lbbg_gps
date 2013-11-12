# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Looking at trips performned to Gotland - proportion of trips by period
# Number of trips etc.



#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")

names(trips)
summary(as.factor(trips$gotland))
str(trips)

#Keep only trips where the minimum interval between GPS fixes is 800s

trips  <-  subset(trips,interval_min < 800)
trips  <-  subset(trips,dist_max > 2)   # Only include trips where maximum distance is at least 2 km

trips.g  <-  subset(trips,gotland ==1)   # Only include trips where maximum distance is at least 2 km


summary(trips.g$duration_s < 12*60*60)
summary(trips$duration_s < 12*60*60)

hist(trips.g$duration_s[trips.g$duration_s < 72*60*60])
# hist(trips$duration_s[trips$duration_s < 72*60*60])
# 100000/60/24/60


plot(trips$start_time,trips$duration_s, col = "red")
points(trips.g$start_time)

#Get month numbers
months <- as.factor(as.numeric(format(trips$start_time,format='%m')))
summary(months)
years <- as.factor(as.numeric(format(trips$start_time,format='%Y')))
summary(years)

days <- as.factor(as.numeric(format(trips$start_time,format='%j')))


got.month <- summary(months[trips$gotland == 1])
all.month <- summary(months)

# Proportion of all.trips to Gotland by month
prop <- as.vector(got.month)/as.vector(all.month)
prop

round((prop*100),1)



got.day<- summary(days[trips$gotland == 1])
all.day <- summary(days)

min(as.numeric(as.character(days)))
max(as.numeric(as.character(days)))

daysn <- as.numeric(as.character(days))
x <- NULL
y <- NULL
t <- NULL
# i <- 1
for(i in 1:140){
  x[i] <- sum((daysn[trips$gotland == 1] > ((i*1) + 90)) &  (daysn[trips$gotland == 1] < ((i*1 )+ 100)))
  y[i] <- sum((daysn > ((i*1) + 90)) &  (daysn < ((i*1) + 100)))
  t[i] <- ((i*1) + 90)
}
pro.ten <- 100*(x/y)

# Plot graph of proportion of foraging trips to Gotland by day (ten day window)
plot(t,pro.ten, ylim = c(0,100), xaxt= "n", xlab = "", ylab="% of trips to Gotland", main = "Gotland foraging trips", type="l")
d <- as.Date(c("2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01"))
d <- as.numeric(format(d,format='%j'))
d.lab <- c("April","May","June","July","August")
axis(1,at =d, labels = d.lab)
# segments(t,pro.ten)

# Proportion of all.trips to Gotland by day
prop.day <- as.vector(got.day)/as.vector(all.day)
prop.day

day.prop <- round((prop.day*100),1)
plot(day.prop)


