# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.


# Summarise info from 'lund_flight_points_wind_par' for commuting flight classification



#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Get a copy of the flights DB table.
flights <- sqlQuery(gps.db, query = "SELECT DISTINCT f.*
                    FROM lund_flights_commuting AS f
                    ORDER BY f.flight_id ASC;")
# str(flights)

#Get a copy of the lund_flight_points_wind_par DB table.
points_par <- sqlQuery(gps.db, query = "SELECT DISTINCT f.*, t.air_2m, t.air_2m_sd, g.altitude, w.cloud_cover_low_altitude, w.cloud_cover_total, w.significant_wave_height, w.sun_shine_duration_day, w.surface_roughness, w.temperature_2m
                    FROM lund_flight_points_wind_par_ecmwf AS f, lund_flights_com_points_weather AS t, gps_uva_tracking_speed_3d_limited AS g, move_bank_variables_all as w
                    WHERE f.device_info_serial = t.device_info_serial
                    AND f.date_time = t.date_time
                    AND f.device_info_serial = g.device_info_serial
                    AND f.date_time = g.date_time
                    AND f.device_info_serial = w.device_info_serial
                    AND f.date_time = w.date_time
                    ORDER BY f.device_info_serial ASC, f.date_time ASC;")
# str(points_par)


# Calculate summary statistics -----

# i <- 8

# hist(points_par$alpha, na.rm = TRUE)

get.stats <- function(i, points_par = points_par, flights = flights){
  # Get flight points
  require(CircStats)
  sub.points <- subset(points_par,
                       device_info_serial == flights$device_info_serial[i] &
                       date_time >= flights$start_time[i] &
                       date_time <= flights$end_time[i] &
                       head_speed_ecmwf > 5 &
                        ground_speed > 2   
                         )
  # str(points_par)
  
  # sub.points <- sub.points
  
  # calculate paramaters:
  wind_side_mean <- mean(sub.points$wind_side, na.rm = TRUE)
  wind_side_median <- median(sub.points$wind_side, na.rm = TRUE)
  
  wind_head_tail_mean <- mean(sub.points$wind_head_tail, na.rm = TRUE)
  wind_head_tail_median <- median(sub.points$wind_head_tail, na.rm = TRUE)
  
  wind_side_mean_10 <- mean(sub.points$wind_side_10, na.rm = TRUE)
  wind_side_median_10 <- median(sub.points$wind_side_10, na.rm = TRUE)
  
  wind_head_tail_mean_10 <- mean(sub.points$wind_head_tail_10, na.rm = TRUE)
  wind_head_tail_median_10 <- median(sub.points$wind_head_tail_10, na.rm = TRUE)

  wind_sc_mean <- mean(sub.points$wind_speed_flt_ht_ecmwf, na.rm = TRUE)
  wind_sc_median <- median(sub.points$wind_speed_flt_ht_ecmwf, na.rm = TRUE)
  
  head_speed_mean <- mean(sub.points$head_speed_ecmwf, na.rm = TRUE)
  head_speed_median <- median(sub.points$head_speed_ecmwf, na.rm = TRUE)
  
  ground_speed_mean <- mean(sub.points$ground_speed, na.rm = TRUE)
  ground_speed_median <- median(sub.points$ground_speed, na.rm = TRUE)
  
  
  # Remove negative altitude values
  rep.neg.alt <- function(x){
    if(is.na(x)) return(x) else{
      if(x < -20) return(NA) else{
        if(x < 0.1){
          return(0.1)} else {
            return(x)}
      }
    }
  }
  
  alt.new <- sapply(sub.points$altitude, rep.neg.alt)
  
  
  alt_new_mean <- mean(alt.new, na.rm = TRUE)
  alt_new_median <- median(sub.points$altitude, na.rm = TRUE)
  
  if.neg <- function(x){
    if(x < 0) return(360 + x)
    else return(x)
  }
  
  alpha_old_mean <- if.neg(deg(circ.mean(rad(sub.points$alphaold))))
  alpha_old_rho <- est.rho(rad(sub.points$alphaold))
#   circ.disp(rad(sub.points$alpha))
  
  alpha_mean <- (deg(circ.mean(rad(sub.points$alpha))))
  alpha_rho <- est.rho(rad(sub.points$alpha))
  
  #   ?circ.disp
  beta_old_mean <- if.neg(deg(circ.mean(rad(sub.points$betaold))))
  beta_old_rho <- est.rho(rad(sub.points$betaold))

  
  
  
  wind_dir_track_mean <- if.neg(deg(circ.mean(rad(sub.points$wind_dir_track))))
  wind_dir_track_rho <- est.rho(rad(sub.points$wind_dir_track))
  wind_dir_track_circvar <- circ.disp(rad(sub.points$wind_dir_track))[,4]
  
  ground_dir_mean <- if.neg(deg(circ.mean(rad(sub.points$ground_heading))))
  ground_dir_rho <- est.rho(rad(sub.points$ground_heading))
  ground_dir_circvar <- circ.disp(rad(sub.points$ground_heading))[,4]

  head_dir_mean <- if.neg(deg(circ.mean(rad(sub.points$head_dir_ecmwf))))
  head_dir_rho <- est.rho(rad(sub.points$head_dir_ecmwf))
  head_dir_circvar <- circ.disp(rad(sub.points$head_dir_ecmwf))[,4]
  
#   est.rho(rad(c(270,0,90)))
  
  wind_dir_deg_mean <- if.neg(deg(circ.mean(rad(sub.points$wind_dir_ecmwf))))
  wind_dir_deg_rho <- est.rho(rad(sub.points$wind_dir_ecmwf))
  
  
  air_2m_mean <- mean(sub.points$air_2m, na.rm = TRUE)
  
  
  

  
  cloud_cover_total.mean <- mean(sub.points$cloud_cover_total, na.rm = TRUE)
  significant_wave_height.mean <- mean(sub.points$significant_wave_height, na.rm = TRUE)
  sun_shine_duration_day.mean <- mean(sub.points$sun_shine_duration_day, na.rm = TRUE)
  surface_roughness.mean <- mean(sub.points$surface_roughness, na.rm = TRUE) 
  temperature_2m.mean <- mean(sub.points$temperature_2m, na.rm = TRUE) 
  cloud_cover_low_altitude.mean <- mean(sub.points$cloud_cover_low_altitude, na.rm = TRUE)

  
  
  flight_id <- flights$flight_id[i]
  
  n <- length(sub.points$wind_dir_deg)
  
  calc.par <- c(flights$flight_id[i], n,
                wind_side_mean, wind_side_median,
                wind_head_tail_mean, wind_head_tail_median,
                wind_sc_mean, wind_sc_median,
                alt_new_mean, alt_new_median,
                alpha_mean, alpha_rho,
                alpha_old_mean, alpha_old_rho,
                beta_old_mean, beta_old_rho,
                wind_side_mean_10, wind_side_median_10,
                wind_head_tail_mean_10,
                wind_head_tail_median_10,
                wind_dir_track_mean,
                wind_dir_track_rho,
                wind_dir_track_circvar,
                wind_dir_deg_mean,
                wind_dir_deg_rho,
                head_dir_mean,
                head_dir_rho,
                head_dir_circvar,
                head_speed_mean,
                head_speed_median,
                ground_speed_mean,
                ground_speed_median,
                ground_dir_mean,
                ground_dir_rho,
                ground_dir_circvar,
                air_2m_mean,
                cloud_cover_total.mean,
                significant_wave_height.mean,
                sun_shine_duration_day.mean,
                surface_roughness.mean,
                temperature_2m.mean,
                cloud_cover_low_altitude.mean
                )
  
  return(calc.par)
}


require(foreach)
require(doParallel)

#Make cluster of number of devices instances
cl <- makeCluster(detectCores())
# ?makeCluster

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  

#export needed data
clusterExport(cl, c("flights","points_par","get.stats"))  

#make a list object to recieve the data
lst <- list()
f <- length(flights$flight_id)
system.time({lst <- foreach(i = 1:f ) %dopar%{
  get.stats(i, flights = flights, points_par = points_par)
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)

# Merge to dataframe
flights.par <- do.call(rbind , lst)
flights.par <- as.data.frame(flights.par)
names(flights.par) <- c("flight_id", "n_points",
                        "wind_side_mean", "wind_side_median",
                        "wind_head_tail_mean", "wind_head_tail_median",
                        "wind_sc_mean", "wind_sc_median",
                        "alt_new_mean", "alt_new_median",
                        "alpha_mean", "alpha_rho",
                        "alpha_old_mean", "alpha_old_rho",
                        "beta_old_mean", "beta_old_rho",
                        "wind_side_mean_10", "wind_side_median_10",
                        "wind_head_tail_mean_10",
                        "wind_head_tail_median_10",
                        "wind_dir_track_mean",
                        "wind_dir_track_rho",
                        "wind_dir_track_circvar",
                        "wind_dir_deg_mean",
                        "wind_dir_deg_rho",
                        "head_dir_mean",
                        "head_dir_rho",
                        "head_dir_circvar",
                        "head_speed_mean",
                        "head_speed_median",
                        "ground_speed_mean",
                        "ground_speed_median",
                        "ground_dir_mean",
                        "ground_dir_rho",
                        "ground_dir_circvar",
                        "air_2m_mean",
                        "cloud_cover_total.mean",
                        "significant_wave_height.mean",
                        "sun_shine_duration_day.mean",
                        "surface_roughness.mean",
                        "temperature_2m.mean",
                        "cloud_cover_low_altitude.mean"
                        )

# head(flights.par)
# str(flights.par)

# Save data to database -------
odbcCloseAll()

gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
# names(gps.data.par)

#Output flight wind par data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, flights.par, tablename = "lund_flight_com_wind_par_ecmwf",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL)

odbcCloseAll()


beep <- function(n = 9){
  x <- c(1,1,3,1,1,3,1,1,3,1,1)
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(x[i])
  }
}
beep()


