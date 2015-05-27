####
# Step 1: Kmeans cluster calculation
####
# partition pickup location space in order to calculate transition matrix (from cluster i to j) 
df <- readRDS('df.RDS')
set.seed(1234)
df_kmeans <- na.omit(df[, c('pickup_longitude', 'pickup_latitude')])
df_kmeans <- scale(df_kmeans) # standardize variables

# determine number of clusters
# wss <- (nrow(df_kmeans)-1)*sum(apply(df_kmeans,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(df_kmeans, 
#                                      centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

# fit K-Means
fit <- kmeans(df_kmeans, 6)
# append cluster assignment
df_kmeans <- df
#do by hour eventually
df_kmeans$pickup_date <- as.Date(df_kmeans$pickup_date)
# label cluster based on assignment
df_kmeans$cluster_pickup <- as.factor(fit$cluster)

# ggplot
#df_kmeans <- readRDS('df_kmeans.RDS')
ggplot(df_kmeans[sample(nrow(df_kmeans), 1e5),], aes(x = pickup_longitude, y = pickup_latitude, color = cluster_pickup, group = cluster_pickup)) +
  geom_point() + coord_equal() + scale_color_brewer(palette = 'Set1')

# on a map
ny = c(lon = mean(range(df$pickup_longitude)), lat =  mean(df$pickup_latitude))
ny.map = get_map(location = ny, zoom = 11, color = "bw")
cluster_map <- ggmap(ny.map, extent = "device") + geom_point(data = df_kmeans[1:5e5,], aes(x = pickup_longitude, y = pickup_latitude, color = cluster_pickup, group = cluster_pickup)) + #
  scale_color_brewer(palette = 'Set1')
saveRDS(cluster_map, './pres/cluster_map.RDS')

####
# Step 2: calculate Keans boudaries with SVM in order to calulate the which cluster
# the dropoff locations belongs to
####
# working on a small (1e5 records dataset) in order to increase computationo speed
# classification accuracy is very high even with such a small dataset
df_svm <- df_kmeans[1:1e5,]
svm_fit <- svm(formula = cluster_pickup ~ pickup_longitude + pickup_latitude, data = df_svm)
#saveRDS(svm_fit, 'svm_fit.RDS')

# score dropoff locations
data_to_score <- select(df_kmeans, dropoff_latitude, dropoff_longitude) %>%
dplyr::rename(., pickup_latitude = dropoff_latitude, pickup_longitude = dropoff_longitude)
pred_svm <- predict(object = svm_fit, data_to_score)
## check precision, recall, accuracy
table(pred_svm, df_svm$cluster_pickup)

#saveRDS(pred_svm, 'pred_svm.RDS')
# append scored data to the base dataset 
df_kmeans$cluster_dropoff <- pred_svm 
saveRDS(df_kmeans, 'df_kmeans.RDS')
# pre-SVM calculation (random sampling just to check that the code ran)
# df_kmeans$cluster_dropoff <- as.factor(sample(1:6, size = nrow(df_kmeans), replace = TRUE))


####
# Step 3: Routing optimization algo
####
# given last timeframe (day now, eventually hour), and where you would like to be last
# back calculate best route to get there
# the top two parameters will be user-defined

## Time-binned version ##
# calculate optimal route on a daily basis
# coarse granularity: originally wrote as a proof of concept of
# the continuous time version

# calculate daily transition matrix first
df_aggr <- ddply(df_kmeans[1:1e4, ], c('pickup_date', 'cluster_pickup', 'cluster_dropoff'), function(x) nrow(x))
names(df_aggr)[4] <- 'combination'
df_sum <- ddply(df_aggr, 'pickup_date', function(x) sum(x$combination))
names(df_sum)[2] <- 'sum'
df_tran <- merge(df_aggr, df_sum)
df_tran$weight <- df_tran$combination/df_tran$sum

df_fare_mean <- ddply(df_kmeans[1:1e4, ], c('pickup_date', 'cluster_pickup', 'cluster_dropoff'), function(x) mean(x$total_amount))
names(df_fare_mean)[4] <- 'mean_fare'

#optimization data
df_trans_opt <- merge(select(df_tran, pickup_date, cluster_pickup, cluster_dropoff, weight), df_fare_mean)
df_trans_opt$expected_revenue <- df_trans_opt$weight * df_trans_opt$mean_fare


last_time_window <- max(df_trans_opt$pickup_date)
home_cluster <- 4
out <- data.frame(date = numeric(), opt_cluster = numeric())

opt_previous_step <- function(df, time_window, home_cluster = home_cluster) {
 opt_subset <- df[df$pickup_date == last_time_window & df$cluster_dropoff == home_cluster, ]
 as.numeric(as.character(opt_subset[which.max(opt_subset$expected_revenue), 'cluster_pickup']))
}
    
for (i in last_time_window:min(df_trans_opt$pickup_date)) {
  if (i == last_time_window) {out <- rbind(out, data.frame(date = i
  , opt_cluster = opt_previous_step(df = df_trans_opt, time_window = i, home_cluster = home_cluster)))
  } else {out <- rbind(out, data.frame(date = i
  , opt_cluster = opt_previous_step(df = df_trans_opt, time_window = i, home_cluster = out[which.max(out$date), 'opt_cluster'])))
  }
}


## Continuous time version ##

df <- readRDS('df_kmeans.RDS')
beg_of_day <- as.POSIXct(strptime('2013-01-17 07:00:00', format = "%Y-%m-%d %H:%M:%S"))
end_of_day <- as.POSIXct(strptime('2013-01-17 10:00:00', format = "%Y-%m-%d %H:%M:%S"))
minute_range <- 5 # tolerance in the arrival time
home_cluster <- 6 # desired destination, i.e. where the taxi drive wants to end her day
first_iter <- TRUE

opt_route_unbinned <- function(input_data, end_of_day, minute_range, home_cluster) {
  #transition matrix calculation
  input_data_aggr <- ddply(input_data, c('pickup_date', 'cluster_pickup', 'cluster_dropoff'), function(x) nrow(x))
  names(input_data_aggr)[4] <- 'combination'
  input_data_sum <- ddply(input_data_aggr, 'pickup_date', function(x) sum(x$combination))
  names(input_data_sum)[2] <- 'sum'
  input_data_tran <- merge(input_data_aggr, input_data_sum)
  input_data_tran$weight <- input_data_tran$combination/input_data_tran$sum
  
  input_data_fare_mean <- ddply(input_data, c('pickup_date', 'cluster_pickup', 'cluster_dropoff'), function(x) mean(x$total_amount))
  names(input_data_fare_mean)[4] <- 'mean_fare'
  
  #optimization data
  input_data_trans_opt <- merge(select(input_data_tran, pickup_date, cluster_pickup, cluster_dropoff, weight), input_data_fare_mean)
  input_data_trans_opt$expected_revenue <- input_data_trans_opt$weight * input_data_trans_opt$mean_fare
  opt_pickup_cluster <- input_data_trans_opt[which.max(input_data_trans_opt$expected_revenue), 'cluster_pickup'] 
  mean_travel_time <- mean(input_data[input_data$cluster_pickup == as.numeric(as.character(opt_pickup_cluster)), 'trip_time_in_secs'])
  trip_beg_time <- end_of_day - mean_travel_time
  data.frame(orig_cluster = opt_pickup_cluster
             , dest_cluster = home_cluster
             , trip_end_time = end_of_day
             , mean_travel_time = mean_travel_time
             , trip_beg_time = trip_beg_time)
}

opt_results <- data.frame(orig_cluster = numeric()
                          , dest_cluster = numeric()
                          , trip_end_time = numeric()
                          , mean_travel_time = numeric()
                          , trip_beg_time = numeric())

# first iteration
df_temp <- subset(df, dropoff_datetime < (end_of_day + 60 * minute_range)
                  & dropoff_datetime > (end_of_day - 60 * minute_range)
                  & cluster_dropoff == home_cluster)
opt_results <- rbind(opt_results, opt_route_unbinned(input_data = df_temp, end_of_day = end_of_day, minute_range = minute_range, home_cluster = home_cluster))

# following iterations
repeat { #beg_of_day < opt_results[which.max(opt_results$trip_beg_time), 'trip_beg_time']
  if (beg_of_day > opt_results[which.min(opt_results$trip_beg_time), 'trip_beg_time']) break();
  target_end_time <- opt_results[which.min(opt_results$trip_beg_time), 'trip_beg_time'];
  dest_cluster <- as.numeric(opt_results[which.min(opt_results$trip_beg_time), 'orig_cluster']);
  df_temp <- subset(df, dropoff_datetime < (target_end_time + 60 * minute_range)
                         & dropoff_datetime > (target_end_time - 60 * minute_range)
                         & cluster_dropoff == dest_cluster);
  #opt_route_unbinned(input_data = df_temp, end_of_day = target_end_time, minute_range = minute_range, home_cluster = dest_cluster)
  opt_results <- rbind(opt_results, opt_route_unbinned(input_data = df_temp, end_of_day = target_end_time, minute_range = minute_range, home_cluster = dest_cluster));
}
opt_results
saveRDS(opt_results, './pres/opt_results.RDS')
