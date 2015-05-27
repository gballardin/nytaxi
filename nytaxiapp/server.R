library("dplyr")
library("DT")
library("ggplot2")
library("htmltools")
library("leaflet")
library("RColorBrewer")
library("shiny")
library("stringr")
library("zoo")
library("lubridate")

# set.seed(123)
# smpl_size <- 1e3
# nytaxi_df <- readRDS("~/nytaxi/df.RDS")
# nytaxi_df <- nytaxi_df[sample(1:nrow(nytaxi_df), smpl_size),]
# saveRDS(nytaxi_df, 'nytaxi_df.RDS')
nytaxi_df <- readRDS('nytaxi_df.RDS')
str(nytaxi_df)

# summary plot munging
nytaxi_df$a_date <- as.Date(nytaxi_df$pickup_datetime)
nytaxi_df$a_time_hr <- hour(nytaxi_df$pickup_datetime)
nytaxi_df$trip_time_in_mins <- round(nytaxi_df$trip_time_in_secs/60, 0)
nytaxi_df <- subset(nytaxi_df, passenger_count <= 24 & (payment_type == 'CRD' | payment_type == 'CSH'))
nytaxi_df$payment_type <- factor(nytaxi_df$payment_type)

accident_desc <- function(row) 
  with(as.list(row), paste0(strong(
    format.Date(a_date, "%a %d %B %Y"), ": "), "A taxi ride that left at ",
    if (hour(pickup_datetime)<12) paste0(hour(pickup_datetime), 'am') else paste0(hour(pickup_datetime) - 12, 'pm'), ", lasted ",
    trip_time_in_mins, " minutes, carried  ", passenger_count, 
    " passengers, costed $", total_amount, " and was paid with",
    if(payment_type == 'CRD') " credit card." else " cash."))

strs <- apply(nytaxi_df, 1, accident_desc)
strs <- str_wrap(strs, width = 10) 


d2 <- nytaxi_df %>% group_by(a_date) %>%
  dplyr::summarise(n=n())
colnames(d2) <- c("a_date", "n")

# colour paletters
pal <- brewer.pal(3, "Set1")
cont_pal_time <- colorRampPalette(c('black', 'yellow', 'black'))(24)
cont_pal_pass <- colorRampPalette(c('yellow', 'red'))(6)

# clean table for dt
clean <- nytaxi_df[,c('a_date', 'passenger_count', 'trip_distance', 'total_amount')]
rownames(clean) <- NULL

shinyServer(function(input, output, session) {

  output$mymap <- renderLeaflet({
    fillv <- if(input$color == "passenger_count") cont_pal_pass[nytaxi_df$passenger_count] else 
      if(input$color == "payment_type") pal[as.factor(nytaxi_df$payment_type)] else
          if(input$color == "Time") cont_pal_time[nytaxi_df$a_time_hr]
            
    
      l <- leaflet(data = subset(nytaxi_df, a_date >= input$dates[[1]] & a_date <= input$dates[[2]])) %>% 
        addTiles(urlTemplate="http://openmapsurfer.uni-hd.de/tiles/roadsg/x={x}&y={y}&z={z}") %>%
        addTiles('http://{s}.tile.openstreetmap.se/hydda/roads_and_labels/{z}/{x}/{y}.png', 
          attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
        setView(lng = -73.97771, lat = 40.75177, zoom = 13) %>%
        addCircleMarkers(~pickup_longitude, ~pickup_latitude, radius = ~total_amount, fillOpacity=input$alpha,
          color=NA, popup=strs, weight=2, fillColor = fillv)
      
      session$sendCustomMessage(type = "map_done", "done")
      
      l
  })
  
  output$monthTotals <- renderPlot({
    ggplot(d2, aes(x=a_date, y=n)) + 
      geom_area() + theme_minimal() + 
      labs(x="", y="Taxi rides\nper day") +
      scale_y_continuous(expand=c(0,0))
  })

  output$table <- DT::renderDataTable({
    DT::datatable(clean, filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE))
  })
  
})
