library('shiny') 
library('rgdal') # This library allows us to read and plot shapefiles
library('leaflet') # Interactivity! 
library('data.table')

source('R/data_load.R')
source('R/map_classes.R')
source('R/map_class_utilities.R')

moz.p <- MapAndChloroData()
match.data <- as.data.table(read.csv("gis/data/Match.csv", header=T, fileEncoding="utf-8"))

map.meta.data <- ConstructMapDataObjects(chloro.raw, moz.p, match.data)
domain <- GetFillDomain(map.meta.data)

map.options <- names(map.meta.data)

# A dumb palette we will probably want to change
pal <- colorNumeric(
  palette = "Reds",
  domain = domain
)

matchpal <- colorFactor(
  palette = "Reds",
  domain = levels(match.data$MatchCat)
)


ui <- shinyUI(fluidPage(
  titlePanel(
    HTML("<header><center>
         <h2> <strong>Percentage of Students Speaking Portugues/Elomwe/Emakua in Mozambique</strong> 
         <p> <font size= 5> by District </font> </p> 
         </h2> </center>
         </header>"
    )),
  selectInput("language", "Language",
              map.options
  ),
  mainPanel(
    leafletOutput("moz.map")
  )
))

server <- shinyServer(function(input, output) {
  
  output$moz.map <- renderLeaflet({
    
    # Initialize leaflet 
    leaf <- leaflet()
    
    # Add static polygons, the entire map without any special fill
    leaf <- addPolygons(leaf, data=moz.p, weight=1, group="static", color = "#000000", fillOpacity=0.4)
    
    # Add all polygons and hide them 
    for(language in map.options) leaf <- DrawMap(map.meta.data[[language]], leaf)
    hideGroup(leaf, map.options)
    
  })
  
  observe({
    choice <- input$language
    new.leaf <- leafletProxy('moz.map')
    
    # Defensively hide all groups
    hideGroup(new.leaf, map.options)
    # Show selected groups
    showGroup(new.leaf, choice)
  })
})

shinyApp(ui=ui, server=server)