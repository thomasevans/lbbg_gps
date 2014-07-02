# getwd()
data_803 <- read.csv("803_june.csv")
# ?read.csv
# names(data_803)

data_803$latitude <- as.numeric(as.character(data_803$latitude))
data_803$longitude <- as.numeric(as.character(data_803$longitude))
str(data_803)
# test[1:10]

filter_nest <- data_803$latitude >  57.279202 & data_803$latitude <   57.280490
filter_nest2 <- data_803$longitude >   17.971527 & data_803$longitude <    17.974330

hist(data_803$latitude[filter_nest], breaks = 40)
mean(data_803$latitude[filter_nest])
median(data_803$latitude[filter_nest])



hist(data_803$longitude[filter_nest2], breaks = 40)
mean(data_803$longitude[filter_nest2])

plot(data_803$longitude[filter_nest2 & filter_nest],data_803$latitude[filter_nest2& filter_nest], xlim = c(17.97305,17.97320), ylim = c(57.2798,57.2800))
