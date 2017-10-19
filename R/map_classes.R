ChloroplethData <- function(x, spatial.data) {
  
  label <- x$label
  colname <- x$colname
  
  sel <- spatial.data@data[, !is.na(get(colname))]
  geo.subset <- spatial.data[sel, ]
  
  geo.subset@data$fill <- geo.subset@data[, colname, with=F]
  data <- list(label=label,
               colname=colname,
               geo.subset=geo.subset)
  
  structure(data, class="chloropleth")
  
}

CircleData <- function(label, spatial.data) {
  
  data <- list(label=label,
               spatial.data=spatial.data)
  
  structure(data, class="circles")
  
}

DrawMap <- function(data, leaf) UseMethod("DrawMap")

DrawMap.chloropleth <- function(data, leaf) addPolygons(leaf, data=data$geo.subset, weight=1, group=data$label, color = "#000000", fillColor=~pal(fill), fillOpacity=0.4)
DrawMap.circles <- function(data, leaf) addCircles(leaf, data=data$spatial.data, lng=~Longitude, lat=~Latitude, group=data$label, radius=10000, fillOpacity=1, color=~matchpal(MatchCat))
