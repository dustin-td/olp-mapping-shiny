library('shiny') 
library('rgdal') # This library allows us to read and plot shapefiles
library('rgeos') # This library simplifies the plotting for speed

# setwd('~/mozambique_repo/olp-mapping-shiny')

shinyServer(function(input, output) {

  output$moz.map <- renderPlot({
    
    # Read in the shapefile
    moz <- readOGR(dsn=path.expand('gis/MOZ_adm1.shp'))
    
    # Find the provinces of interest
    sel <- moz$NAME_1 %in% c('Zambezia', 'Nampula')
    
    # Simplify contours
    moz <- gSimplify(moz, tol=0.01, topologyPreserve=TRUE)
    
    # Plot base map in light grey
    plot(moz, col='lightgrey')
    
    # Layer on the provinces of interest in turquoise
    plot(moz[ sel,], col = 'turquoise', add = TRUE)
    
  })
    
})


