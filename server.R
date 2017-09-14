library('shiny') 
library('rgdal') # This library allows us to read and plot shapefiles
library('leaflet') # Interactivity! 

# setwd('~/moz_shiny/olp-mapping-shiny')

# Possibly crufty coloring code...
fills <- c('not_highlightable', 'highlightable', 'highlighted')
fills <- factor(fills, levels=fills)
factpal <- colorFactor(c('white', 'blue', 'red'), fills)

# Provinces
kProvinces <- c('Zambezia', 'Nampula')

shinyServer(function(input, output) {

  # Read in the shapefiles
  moz <- readOGR(dsn='gis/MOZ_adm1.shp')
  
  # Find the provinces of interest
  sel <- moz$NAME_1 %in% c('Zambezia', 'Nampula')
  
  moz@data$default_fill <- fills[1]
  moz@data[sel, ]$default_fill <- fills[2]

  # Bounding boxes...
  # Hardcode the first one.
  kBounds <- c(21.5332, -26.94166, 49.52637, -10.35815)
  views <- list()
  views[[1]] <- list(lng = (kBounds[1]+kBounds[3])/2, lat=(kBounds[2]+kBounds[4])/2, zoom=5)
  
  for(province in kProvinces) {
    
    box <- bbox(moz[moz$NAME_1 == province, ])
    lng1 <- box['x', 'min']
    lat1 <- box['y', 'min']
    lng2 <- box['x', 'max']
    lat2 <- box['y', 'max']
  
    views[[length(views) + 1]] <- list(lng=(lng1 + lng2)/2, lat=(lat1+lat2)/2, zoom=7)
  }
  names(views) <- c('Global', kProvinces)
  
  # Split the layers into dynamic and non-dynamic shapes
  static.portion <- moz[!sel, ]
  dynamic.portion <- moz[sel, ]
  
  moz.adm <- readOGR(dsn=('gis/MOZ_adm2.shp'))
  moz.adm <- moz.adm[moz.adm@data$NAME_1 %in% kProvinces, ]
  moz.adm@data$group <- paste0(moz.adm@data$NAME_1, '_adm')
  
  output$moz.map <- renderLeaflet({
    
    # Initialize leaflet 
    leaf <- leaflet()
    
    # Add static polygons
    leaf <- addPolygons(leaf, data=static.portion, weight=1, group="static", color = "#000000", fillColor=~factpal(default_fill), fillOpacity=0.4)
    
    # Add dynamic polygons
    leaf <- addPolygons(leaf, data=dynamic.portion, layer = ~NAME_1, group = ~NAME_1, weight=1, color = "#000000", fillColor=~factpal(default_fill), fillOpacity=0.4, highlightOptions = highlightOptions(
      color='#000000', opacity = 1, weight = 1, fillColor = "#FF0000", fillOpacity = 1))
    
    # Add adm2 polygons with clever group names...
    leaf <- addPolygons(leaf, data=moz.adm, group = ~group, weight=1, color="#000000", fillColor='blue', fillOpacity = 0.4)
    hideGroup(leaf, paste0(kProvinces, '_adm'))
    
    # In the above code, we'll probably factor out the graphical parameters as this file grows in complexity
  })
  
  
  # Take a crack at handling clicks.  This clicks into a province
  observe({
    click <- input$moz.map_shape_click
    
    if(is.null(click)) return()

    if(!is.null(click$id)) {

      new.leaf <- leafletProxy('moz.map')
      showGroup(new.leaf, paste0(click$id, '_adm'))
      hideGroup(new.leaf, c('static', kProvinces))
      view <- views[[click$id]]
      new.leaf <- setView(new.leaf, lng=view$lng, lat=view$lat, zoom=view$zoom)
      
    }
    
  })

  # Click back to the full map
  observe({
    click <- input$moz.map_click
    if(is.null(click)) return()

    new.leaf <- leafletProxy('moz.map')
    #setView(new.leaf, lng=(30.21741 + 40.83931) / 2, lat=(-26.86869 + -10.47125)/2, zoom=4)
    
    new.leaf <- hideGroup(new.leaf, paste0(kProvinces, '_adm'))
    new.leaf <- showGroup(new.leaf, c('static', kProvinces))
    view <- views[['Global']]
    new.leaf <- setView(new.leaf, lng=view$lng, lat=view$lat, zoom=view$zoom)
  })

})


