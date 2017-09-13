# Server

library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(reshape)

setwd('G:/Mozambique/Analysis/test_2')


# dt  <- read.csv("Analysis/district_level_data.csv", stringsAsFactors = FALSE)

# Reading Our Survey data 
dt  <- read.csv("data/district_level_data.csv", stringsAsFactors = FALSE)

# Simple cleaning 
dt$province <- gsub("[[:punct:]]", "", dt$province)
dt$district <- gsub("[[:punct:]]", "", dt$district)
head(dt)


# Subseting Survey data by Student's spoken language 
#########################################################
dt_p <-  subset(dt, student_lang=="Portuguees")
dt_el <-  subset(dt, student_lang=="Elomwe")
dt_em <-  subset(dt, student_lang=="Emakua")


# Reading Mozambique Shapefile
#########################################################
# shapeData <- readOGR("G:/Mozambique/mozshapefile" , 'MOZ_adm2')
shapeData <- readOGR("data/MOZ_adm2" , 'MOZ_adm2')
getClass("SpatialPolygonsDataFrame")

# Simple cleaning
shapeData$NAME_1 <- gsub("[[:punct:]]", "", shapeData$NAME_1)
shapeData$NAME_2 <- gsub("[[:punct:]]", "", shapeData$NAME_2)

names(shapeData)[names(shapeData) == "NAME_1"] <- "province"
names(shapeData)[names(shapeData) == "NAME_2"] <- "district"
names(shapeData)

# Setting up Color palettes 
getPalette = colorRampPalette(brewer.pal(9, "Set1"))


# 1) Merging Portuguees Data Set with Moz Shapefile 
######################################################
shapeData_p <- merge(shapeData, dt_p, by= c("province","district" ) )
colourCount_p = len <- length(unique(shapeData_p$prop))
labels_p = sprintf("<strong>%s</strong><br/>%s ",
                   shapeData_p$district ,  paste(round(shapeData_p$prop, digit=2), "%")) %>% lapply(htmltools::HTML)
pal_p = colorNumeric(palette = "Blues", domain = shapeData_p$prop)



# 2) Merging Elomwe Data Set with Moz Shapefile 
######################################################
shapeData_el <- merge(shapeData, dt_el, by= c("province","district" ) )
colourCount_el = len <- length(unique(shapeData_el$prop))
labels_el = sprintf("<strong>%s</strong><br/>%s ",
                   shapeData_el$district ,  paste(round(shapeData_el$prop, digit=2), "%")) %>% lapply(htmltools::HTML)
pal_el = colorNumeric(palette = "Greens", domain = shapeData_el$prop)



# 3) Merging Emakua Data Set with Moza Shapefile
######################################################
shapeData_em <- merge(shapeData, dt_em, by= c("province","district" ) )
colourCount_em = len <- length(unique(shapeData_em$prop))
labels_em = sprintf("<strong>%s</strong><br/>%s ",
                    shapeData_em$district ,  paste(round(shapeData_em$prop, digit=2), "%")) %>% lapply(htmltools::HTML)
pal_em = colorNumeric(palette = "Purples", domain = shapeData_em$prop)




#Render leaflet and Shiny 
shinyServer(
  function(input, output){
    output$mymap <- renderLeaflet({
      leaflet(shapeData_p) %>%
        addPolygons(fillColor = ~pal_p(prop) ,
                    stroke = FALSE , 
                    smoothFactor = 0.2 ,
                    weight = 0.5, 
                    opacity = 1,
                    color = "black", 
                    dashArray = "3" ,
                    fillOpacity = 0.7,
                    label = labels_p,
                    labelOptions  = labelOptions(direction = "right" ,
                                                 style = list( "font-size" = "12px")),
                    highlight = highlightOptions(
                      color = "#666",
                      weight = 5,
                      opacity = 1,
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE)) 
      })
    output$mymap_2 <- renderLeaflet({
      leaflet(shapeData_el) %>%
        addPolygons(fillColor = ~pal_el(prop) ,
                    smoothFactor = 0.2 ,
                    weight = 0.5, 
                    opacity = 1,
                    color = "black", 
                    dashArray = "3" ,
                    fillOpacity = 0.7,
                    label = labels_el,
                    labelOptions  = labelOptions(direction = "right" ,
                                                 style = list( "font-size" = "12px")),
                    highlight = highlightOptions(
                      color = "#666",
                      weight = 5,
                      opacity = 1,
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE)) 
    })
    output$mymap_3 <- renderLeaflet({
      leaflet(shapeData_em) %>%
        addPolygons(fillColor = ~pal_em(prop) ,
                    weight = 1, 
                    opacity = 1,
                    color = "black", 
                    dashArray = "3" ,
                    fillOpacity = 0.7,
                    label = labels_em,
                    labelOptions  = labelOptions(direction = "right" ,
                                                 style = list( "font-size" = "12px")),
                    highlight = highlightOptions(
                      color = "#666",
                      weight = 5,
                      opacity = 1,
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE)) 
    })
})




