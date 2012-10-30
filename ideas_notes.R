#a place to collect ideas, before using them


#have a look at speeds, could say speeds over about 2.5 ms-1 represent flight
hist(gps$inst_ground_speed[gps$inst_ground_speed < 100],xlim=c(0,20),breaks=100)
axis(1,at=seq(0,20,by=1))





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