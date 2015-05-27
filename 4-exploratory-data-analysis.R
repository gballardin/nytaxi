#### 
# ggmap
#### 
#df <- readRDS('df.RDS')
ny = c(lon = mean(df$pickup_longitude), lat =  mean(df$pickup_latitude))
ny.map = get_map(location = ny, zoom = 13, color = "bw")

#ggmap(ny.map, extent = "device") + geom_point(aes(x = pickup_longitude, y = pickup_latitude), colour = "red", alpha = 0.1, size = 2, data = fare_df)
# ggmap(ny.map, extent = "device") + geom_density2d(data = fare_df, aes(x = pickup_longitude, y = pickup_latitude), size = 0.3) + 
#   stat_density2d(data = fare_df, aes(x = pickup_longitude, y = pickup_latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
#   scale_fill_gradient(low = "green", high = "red", guide = FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)

ggmap(ny.map, extent = "device") + geom_density2d(data = df[1:1e5,], aes(x = pickup_longitude, y = pickup_latitude), size = 0.3) + 
  stat_density2d(data = df[1:1e5,], aes(x = pickup_longitude, y = pickup_latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", guide = FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)

#### 
# ggplot
#### 

ggplot(df[1:1e5,], aes(y = pickup_longitude, x = pickup_latitude)) + geom_density2d() + coord_equal()

# kmeans
mydata <- na.omit(df[1:1e3, c('pickup_longitude', 'pickup_latitude')]) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 6)
# get cluster means 
#aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)
mydata$fit.cluster <- as.factor(mydata$fit.cluster)
ggplot(mydata, aes(y = pickup_longitude, x = pickup_latitude, color = fit.cluster, group = fit.cluster)) +
  geom_point() + coord_equal() + scale_color_brewer(palette = 'Set1')
