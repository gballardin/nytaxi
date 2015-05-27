source(paste0(getwd(), '/00-utils.R'))
source(paste0(getwd(), '/0-required-packages.R'))
source(paste0(getwd(), '/1-pull-data.R'))
source(paste0(getwd(), '/2-clean-data.R'))


#pull a dn clean data
# download()
# sapply(1:12, unzip_data)
# 
# fare_ls <- list()
# pull_fare_data()
# fare_df <- clean_fare_data(do.call(rbind, fare_ls))
# 
# trip_ls <- list()
# pull_trip_data()
# for (i in 1:4) {
#   names(trip_ls[[i]]) <- str_trim(names(trip_ls[[i]]))
# }
# trip_df <- clean_trip_data(do.call(rbind, trip_ls)) 
# 
# load joined data
df <- readRDS('jandf.RDS') %>% clean_joined_dataset()
df <- readRDS('df.RDS')
