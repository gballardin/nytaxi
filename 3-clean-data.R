clean_fare_data <- function(df) {
  names(df) <- str_trim(names(df))
  # cast date time correctly
  df$pickup_datetime <- cast_to_date_time(df$pickup_datetime)
  # recalculate total amount, since it is incorrect in 1-2% of the cases
  df$total_amount - (df$fare_amount + df$surcharge + df$mta_tax + df$tip_amount + df$tolls_amount)
  df
}

clean_trip_data <- function(df) {
  # cast date time correctly
  df$pickup_datetime <- cast_to_date_time(df$pickup_datetime)
  df$dropoff_datetime <- cast_to_date_time(df$dropoff_datetime)
  # recalculate trip duration (not always correct in the original data)
  df$trip_time_in_secs <- as.integer(df$dropoff_datetime) - as.integer(df$pickup_datetime)
  # if distance is less than Euclidian distance, plug in Euclidean distance
  # build function in utils.R script first
  # calculate trip distance also in the cases in which the distaince is 
  df <- dplyr::filter(df, trip_time_in_secs != 0 & pickup_longitude != 0 & pickup_latitude != 0 & dropoff_longitude != 0 & dropoff_latitude != 0)
  df
}

clean_joined_dataset <- function(df) {#bkup <- df
  #remove duplicate columns
  df$i.vendor_id <- NULL
  #cast to correct data type
  df$pickup_datetime <- cast_to_date_time(df$pickup_datetime)
  df$dropoff_datetime <- cast_to_date_time(df$dropoff_datetime)
  df[, c(1:3, 5)] <- lapply(df[, c(1:3, 5)], as.factor)
  df$rate_code <- as.factor(df$rate_code)
  df$store_and_fwd_flag <- as.factor(df$store_and_fwd_flag)
  #correct some of the variables
  df$trip_time_in_secs <- as.integer(df$dropoff_datetime) - as.integer(df$pickup_datetime)
  df$total_amount <- df$fare_amount + df$surcharge + df$mta_tax + df$tip_amount + df$tolls_amount
  #remove data that is not in the proximity of NY City
  lat_long_limits <- sapply(df[, grep('.*_l[ong|lat].*', names(df), value = TRUE)], quantile, c(.02, .98), na.rm = TRUE)
  for (i in colnames(lat_long_limits)) {df <- df[df[i] > lat_long_limits[1, i] & df[i] < lat_long_limits[2, i],]}
  trip_time_in_secs_limits <- quantile(df$trip_time_in_secs, c(.005, .995), na.rm = TRUE)
  df <- df[df$trip_time_in_secs > trip_time_in_secs_limits[1] & df$trip_time_in_secs < trip_time_in_secs_limits[2],]
  trip_distance_limits <- quantile(df$trip_distance, c(.01, .99), na.rm = TRUE)
  df <- df[df$trip_distance > trip_distance_limits[1] & df$trip_distance < trip_distance_limits[2],]
  df <- subset(df, trip_time_in_secs > 0 & trip_distance > 0)
  df[complete.cases(df), ]
}