library('shiny') 
library('rgdal') # This library allows us to read and plot shapefiles
library('leaflet') # Interactivity! 
library('data.table')

moz.p <- readOGR(dsn="data/MOZ_adm_shp", "MOZ_adm3")
moz.p$rn <- row.names(moz.p)
#data <- geojson_json(moz.p)
moz.data <- as.data.table(moz.p@data)
csv.data <- as.data.table(read.csv("data/Maps_9-1.csv", header=T))

csv.data <- csv.data[, .(gadmPA, zScore, zpor, zelo, zemk, zech)]
setnames(csv.data, "gadmPA", "NAME_3")

setkey(csv.data, NAME_3)
setkey(moz.data, NAME_3)

moz.data <- csv.data[moz.data]

setkey(moz.data, rn)
moz.p@data <- moz.data[row.names(moz.p)]

languages <- c('Overall', 'Portuguese', 'Elomwe', 'Emakhua', 'Echuwabo')

# DRY way of segmenting the map
colnames <- c('zScore', 'zpor', 'zelo', 'zemk', 'zech')
for(colname in colnames) {
  
  sel <- moz.p@data[, !is.na(get(colname))]
  assign(paste0('moz.', colname), moz.p[sel, ])
  
}

# A dumb palette we will probably want to change
pal <- colorNumeric(
  palette = "Reds",
  domain = unique(c(csv.data$zpor, csv.data$zelo, csv.data$zemk, csv.data$zech, csv.data$zScore))
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
              languages
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
    
    # Add all polygons and hide them (We'll want to DRY this up as well)
    leaf <- addPolygons(leaf, data=moz.zScore, weight=1, group='Overall', color = "#000000", fillColor=~pal(zScore), fillOpacity=0.4)
    leaf <- addPolygons(leaf, data=moz.zpor, weight=1, group='Portuguese', color = "#000000", fillColor=~pal(zpor), fillOpacity=0.4)
    leaf <- addPolygons(leaf, data=moz.zelo, weight=1, group='Elomwe', color = "#000000", fillColor=~pal(zelo), fillOpacity=0.4)
    leaf <- addPolygons(leaf, data=moz.zemk, weight=1, group='Emakhua', color = "#000000", fillColor=~pal(zemk), fillOpacity=0.4)
    leaf <- addPolygons(leaf, data=moz.zech, weight=1, group='Echuwabo', color = "#000000", fillColor=~pal(zech), fillOpacity=0.4)
    hideGroup(leaf, languages)
    
  })
  
  observe({
    choice <- input$language
    new.leaf <- leafletProxy('moz.map')
    
    # Defensively hide all groups
    hideGroup(new.leaf, languages)
    # Show selected groups
    showGroup(new.leaf, choice)
  })
})

shinyApp(ui=ui, server=server)