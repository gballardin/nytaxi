library(ggplot2)
require(plyr)

dat<-read.csv("~/data/sick_days_per_day.txt",header=TRUE,sep="\t")</pre>
  colnames(dat) <- c("date", "count", "user_cnt")

# normalize data by number of users on each date
dat$norm_count <- dat$count / dat$user_cnt

# facet by year ~ month, and each subgraph will show week-of-month versus weekday the year is simple
dat$year<-as.numeric(as.POSIXlt(dat$date)$year+1900)
dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)

# turn months into ordered facors to control the appearance/ordering in the presentation
dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)

# the day of week is again easily found
dat$weekday = as.numeric(format(as.POSIXlt(dat$date),"%u"))

# again turn into factors to control appearance/abbreviation and ordering
# I use the reverse function rev here to order the week top down in the graph
# you can cut it out to reverse week order
dat$weekdayf<-factor(dat$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)

# the monthweek part is a bit trickier - first a factor which cuts the data into month chunks
dat$yearmonth<-as.yearmon(dat$date)
dat$yearmonthf<-factor(dat$yearmonth)

# then find the "week of year" for each day
dat$week <- as.numeric(format(as.POSIXlt(dat$date),"%W"))

# and now for each monthblock we normalize the week to start at 1
dat<-ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))

# Now for the plot
P<- ggplot(dat, aes(monthweek, weekdayf, fill = dat$norm_count)) +
  geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="green", high="red") +
  opts(title = "Time-Series Calendar Heatmap - HeiaHeia.com sick days logged") + xlab("Week of Month") + ylab("") + labs(fill="per user per day")
P