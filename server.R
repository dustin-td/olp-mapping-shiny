library('shiny') 
library('rgdal') # This library allows us to read and plot shapefiles
library('leaflet') # Interactivity! 

# setwd('~/moz_shiny/olp-mapping-shiny')

# Possibly crufty coloring code...
fills <- c('not_highlightable', 'highlightable', 'highlighted')
fills <- factor(fills, levels=fills)
factpal <- colorFactor(c('white', 'blue', 'red'), fills)

shinyServer(function(input, output) {

  output$moz.map <- renderLeaflet({
    
    # Read in the shapefile
    moz <- readOGR(dsn=path.expand('gis/MOZ_adm1.shp'))
    
    # Find the provinces of interest
    sel <- moz$NAME_1 %in% c('Zambezia', 'Nampula')
    
    moz@data$default_fill <- fills[1]
    moz@data[sel, ]$default_fill <- fills[2]
    
    # Split the layers into dynamic and non-dynamic shapes
    static.portion <- moz[!sel, ]
    dynamic.portion <- moz[sel, ]
    
    # Initialize leaflet 
    leaf <- leaflet()
    
    # Add static polygons
    leaf <- addPolygons(leaf, data=static.portion, weight=1, color = "#000000", fillColor=~factpal(default_fill), fillOpacity=0.4)
    
    
    # Add dynamic polygons
    leaf <- addPolygons(leaf, data=dynamic.portion, weight=1, color = "#000000", fillColor=~factpal(default_fill), fillOpacity=0.4, highlightOptions = highlightOptions(
      color='#000000', opacity = 1, weight = 1, fillColor = "#FF0000", fillOpacity = 1))
    
    # In the above code, we'll probably factor out the graphical parameters as this file grows in complexity

  })
    
})


