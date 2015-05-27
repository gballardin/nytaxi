#### Download and unzip data
download <- function(i) {for (i in 1:12) {
  print(paste('step', i, 'of 12', sep = ' '))
  download.file(url = paste0('http://nyctaxitrips.blob.core.windows.net/data/trip_data_', i, '.csv.zip'), destfile = paste0(getwd(), '/data/', 'trip_data_', i, '.csv.zip'))
  download.file(url = paste0('http://nyctaxitrips.blob.core.windows.net/data/trip_fare_', i, '.csv.zip'), destfile = paste0(getwd(), '/data/', 'trip_fare_', i, '.csv.zip'))
}
}

# unzip: 
unzip_data <- function(i) {
  print(paste('step', i, 'of 12', sep = ' '))
  unzip(paste0(getwd(), '/trip_fare_', i, '.csv.zip')) #, exdir = '/home/gballardin/nytaxi/data/')
  unzip(paste0(getwd(), '/trip_data_', i, '.csv.zip')) #, exdir = '/home/gballardin/nytaxi/data/')
}


pull_fare_data <- function() {for (i in 1:4) {
  print(i)
  dt <- fread(input = paste0(getwd(), '/data/trip_fare_', i,'.csv'))
  df <- as.data.frame(dt[sample(nrow(dt), round(.001 * nrow(dt)))])
  fare_ls[[i]] <<- df
  rm(dt)
  rm(df)
  gc()
}
}

pull_trip_data <- function() {for (i in 1:4) {
  print(i)
  dt <- fread(input = paste0(getwd(), '/data/trip_data_', i,'.csv'))
  df <- as.data.frame(dt[sample(nrow(dt), round(.001 * nrow(dt)))])
  trip_ls[[i]] <<- df
  rm(dt)
  rm(df)
  gc()
}
}

