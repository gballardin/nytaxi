library("dplyr")
library("DT")
library("shiny")
library("leaflet")


shinyUI(navbarPage("New York taxi dataset", id="nav",
  tabPanel("Map",
    div(class="outer",
      
      tags$head(
          includeScript("analytics.js"),
          includeCSS("styles.css"),
          includeScript("spin.min.js")
      ),
      
      leafletOutput("mymap", width="100%", height="100%"),
      tags$script("
var spinner = new Spinner().spin();
$( 'div#mymap' ).append(spinner.el);"),
      
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
        width = 380, height = "auto",
        
        h2(),
        p(strong("New York taxi dataset"), "shows taxi ride in New York City.",
          "Data is made available by Chris Whong",
          "via a FOIL (The Freedom of Information Law) request. ",
          "Download data ",
          a("here", 
            href="http://www.andresmh.com/nyctaxitrips/")),

        hr(),
        
        h4("Controls"),
        
        dateRangeInput('dates',
          label = 'Occurred between:',
          start = as.Date("2012-12-31"), end = as.Date("2013-01-31")),
        
        selectInput("color", "Color by:", 
          choices=c("passenger_count", "payment_type", "Time")),
        
        sliderInput("alpha", "Opacity:",
          min=0, max=1, value=.4),
        
        hr(),
        h4("Summary plots"),
        plotOutput("monthTotals", height = "120px"),
        
        hr(),
        p("Under active development by Giorgio Ballardin", 
        "code available on ",
        a("github", href="https://github.com/blmoore/blackspot"), #ADD public repo here
          "(original Shiny code adapted from",
        a("Blackspot", href="https://github.com/blmoore/blackspot"), 
          "by Benjamin Moore)."),
      
      tags$script('
  Shiny.addCustomMessageHandler("map_done",
        function(s) {
          spinner.stop();
          $( "div#mymap" ).remove(spinner);
        });')
        
      )
    )
  ), tabPanel("Table", DT::dataTableOutput("table"))
)
)