#a place to collect ideas, before using them




#some data visualisation ideas

#mapping data
library(maps)
map("world", xlim=range(gps$longitude, na.rm=T), ylim=range(gps$latitude, na.rm=T), bg='blue', col='green', fill=T)
points(gps$longitude, gps$latitude, col='red', pch=19, cex=.5)
?map
hist(gps$temperature)


library(iplots)
iplot(gps$longitude, gps$latitude)

bird_602[1:100]
bird_602 <- gps$device_info_serial == 602
plot(gps$date_time[bird_602],gc_dist[bird_602])