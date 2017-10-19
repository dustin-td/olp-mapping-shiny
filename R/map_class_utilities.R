NameListElements <- function(map.meta.data) {
  
  # Turns an unnamed list into a list where elements
  # are accesible by name elements
  
  for (index in 1:length(map.meta.data)) {
    
    label <- map.meta.data[[index]]$label
    names(map.meta.data)[index] <- label
  }
  
  return(map.meta.data)
}

ConstructMapDataObjects <- function(chloro.raw, moz.p, match.data) {
  
  chloro.list <- lapply(chloro.raw, ChloroplethData, spatial.data=moz.p)
  circles <- CircleData('LOI Match', match.data)
  map.meta.data <- NameListElements(c(chloro.list, list(circles)))
  
  return(map.meta.data)
}

GetFillDomain <- function(map.data) {
  
  # Operates on map data list to return the domain
  # Of all possible z scores to create a consistent
  # Color palette so the key will not shift
  
  if(class(map.data) != 'chloropleth') return(NULL)
  return(unique(map.data$fill))
  
}
