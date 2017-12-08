library(leaflet)
library(magrittr)
library(sp)
library(rgdal)
library(data.table)
library(ggplot2)
library(svglite)
library(shiny)
library(shinyjs)

#setwd('~/../Dropbox/Mozambique Visualization/app')

moz3 <- readOGR(dsn='data/GADM', 'MOZ_adm3')
moz3$rn <- row.names(moz3)
mapdata <- as.data.table(moz3@data)

# Z-Scores ---------------------------------------------------------------------

data <- as.data.table(read.csv("data/Maps_9-1.csv",
                               header=T,
                               fileEncoding="UTF-8"))

setnames(data, "gadmPA", "NAME_3")
setkey(data,NAME_3)
setkey(mapdata,NAME_3)

mapdata <- data[mapdata]

setkey(mapdata, rn)
moz3@data <- mapdata[row.names(moz3)]

nz <- moz3[moz3@data$i.NAME_1 == "Nampula" | moz3@data$i.NAME_1 == "Zambezia", ]

pal <- colorNumeric("RdBu",
                    c(-1.5,1.5),
                    na.color = "#242426")

pa.popup <- paste0("<b>Post Admin: </b>",
                   nz@data$NAME_3,
                   "<br><b>Semantic Fluency z-score: </b>",
                   nz@data$zScore)

for (x in c("zScore", "zpor", "zelo", "zech", "zemk")) {
  assign(paste0("popup", x), paste0("<b>Post Admin: </b>",
                    nz@data$NAME_3,
                    "<br><b>Semantic Fluency z-score: </b>",
                    round(nz@data[[x]], digits = 2)))
}



m <- leaflet(nz) %>% addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(group = "Overall", 
              popup = popupzScore,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = ~colorNumeric("RdBu",
                                        c(-1.5,1.5),
                                        na.color = "#242426")(get("zScore")),
              highlightOptions = highlightOptions(color = "cyan",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(group = "Portuguese", 
              popup = popupzpor,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = ~colorNumeric("RdBu",
                                        c(-1.5,1.5),
                                        na.color = "#242426")(zpor),
              highlightOptions = highlightOptions(color = "cyan",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(group = "Elomwe", 
              popup = popupzelo,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = ~colorNumeric("RdBu",
                                        c(-1.5,1.5),
                                        na.color = "#242426")(zelo),
              highlightOptions = highlightOptions(color = "cyan",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(group = "Emakhua", 
              popup = popupzemk,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = ~colorNumeric("RdBu",
                                        c(-1.5,1.5),
                                        na.color = "#242426")(zemk),
              highlightOptions = highlightOptions(color = "cyan",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(group = "Echuwabo", 
              popup = popupzech,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = ~colorNumeric("RdBu",
                                        c(-1.5,1.5),
                                        na.color = "#242426")(zech),
              highlightOptions = highlightOptions(color = "cyan",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft",
            pal = pal,
            values = c(-1.5,1.5),
            title = "Semantic<br>Fluency<br>Z-Score",
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Overall", "Portuguese", "Emakhua", "Elomwe", "Echuwabo"),
    options = layersControlOptions(collapsed = FALSE)
  )

m

# Bilingualism -------------------------------------------------

bm.pal <- colorNumeric("Greens",
                    c(0,0.6),
                    na.color = "#242426")

bm.popup <- paste0("<b>Post Admin: </b>",
                   nz@data$NAME_3,
                   "<br><b>Percentage Bilingual: </b>",
                   round(nz@data$Lingualism, digits = 2) * 100,
                   "%")

bm <- leaflet(nz) %>% addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(group = "Bilingualism", 
              popup = bm.popup,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = ~colorNumeric("Greens",
                                        c(0,0.6),
                                        na.color = "#242426")(Lingualism),
              highlightOptions = highlightOptions(color = "cyan",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft",
            pal = bm.pal,
            values = c(0,0.6),
            title = "Proportion Bilingual",
            opacity = 0.5)
bm

# Census ----------------------------------------------------

ipums <- readOGR(dsn='data/IPUMS', 'geo2_mz2007')
ipums$rn <- row.names(ipums)
ipums.md <- as.data.table(ipums@data)

census <- as.data.table(read.csv("data/Census_IPUMS_Districts.csv",
                               header=T,
                               fileEncoding="UTF-8"))
census[, IPUM2007 := as.factor(IPUM2007)]
census[, IPUM2007 := paste0("00", IPUM2007)]

setkey(census, IPUM2007)
setkey(ipums.md, IPUM2007)

ipums.md <- census[ipums.md]

setkey(ipums.md, rn)
ipums@data <- ipums.md[row.names(ipums)]

ipums.nz <- ipums[ipums@data$PARENT == "003" | ipums@data$PARENT == "004", ]

factpal <- colorFactor(rainbow(6), ipums.nz@data$which_max1)
cm.popup <- paste0("<b>District: </b>",
                   ipums.nz@data$District,
                   "<br><b>Percentage Speaking ",
                   ipums.nz@data$which_max1,
                   ": </b>",
                   round(ipums.nz@data$max1, digits = 2)*100,
                   "%")
cm.popup2 <- paste0("<b>District: </b>",
                   ipums.nz@data$District,
                   "<br><b>Percentage Speaking ",
                   ipums.nz@data$which_max2,
                   ": </b>",
                   round(ipums.nz@data$max2, digits = 2)*100,
                   "%")
cm <- leaflet(ipums.nz) %>% addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(group = "First Language",
              popup = cm.popup,
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              #fillOpacity = ipums.nz@data$max1,
              fillOpacity = 0.5,
              color = ~factpal(which_max1),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(group = "Language Used Most Frequently",
              popup = cm.popup2,
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              #fillOpacity = ipums.nz@data$max1,
              fillOpacity = 0.5,
              color = ~factpal(which_max2),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft",
            pal = factpal,
            values = ~which_max1,
            title = "Census<br>Languages",
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("First Language", "Language Used Most Frequently"),
    options = layersControlOptions(collapsed = FALSE)
  )

cm

# Scores --------------------------------------------------------

scores <- as.data.table(read.csv("data/Maps_12-6.csv",
                               header=T,
                               fileEncoding="UTF-8"))

scores[, fillScore := MainScore / max(MainScore)]

moz3 <- readOGR(dsn='data/GADM', 'MOZ_adm3')
moz3$rn <- row.names(moz3)

mapdata <- as.data.table(moz3@data)

setnames(scores, "gadmPA", "NAME_3")
setkey(scores,NAME_3)
setkey(mapdata,NAME_3)

mapdata <- scores[mapdata]

mapdata[is.na(which_max), fillScore := 0.5]

setkey(mapdata, rn)
moz3@data <- mapdata[row.names(moz3)]

nz <- moz3[moz3@data$i.NAME_1 == "Nampula" | moz3@data$i.NAME_1 == "Zambezia", ]


lpal <- colorFactor(rainbow(4), scores$which_max, na.color = "#242426")
score.popup <- paste0("<b>Post Admin: </b>",
                   nz@data$NAME_3,
                   "<br><b>Percentage Speaking ",
                   nz@data$which_max,
                   ": </b>",
                   round(nz@data$max, digits = 2)*100,
                   "%",
                   "<br><b>Mean Score: </b>",
                   round(nz@data$MainScore, digits = 2))

sm <- leaflet(nz) %>% addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(group = "Scores",
              popup = score.popup,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = ~fillScore,
              fillColor = ~lpal(which_max),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft",
            pal = lpal,
            values = ~which_max,
            title = "Languages<br>Darker -> lower score",
            opacity = 0.5)
sm

# LOI Match -----------------------------------------------------

loi.match <- as.data.table(read.csv("data/Match.csv",
                                 header=T,
                                 fileEncoding="UTF-8"))
loi.popup <- paste0("<b>Percentage LOI Match: </b>",
                    round(loi.match$Match, digits = 2) * 100,
                    "%")

loipal <- colorFactor(rainbow(5), loi.match$MatchCat, na.color = "#242426")
loim <- leaflet(loi.match) %>% addProviderTiles("CartoDB.DarkMatter") %>%
  addCircles(lng = ~Longitude,
             lat = ~Latitude,
             weight = 1,
             radius = 5000,
             popup = loi.popup,
             color = ~loipal(MatchCat)) %>%
  addLegend(position = "bottomleft",
            pal = loipal,
            values = ~MatchCat,
            title = "Percentage of<br>Students Matching<br>Language of Instruction",
            opacity = 0.5)
loim

# App -----------------------------------------------------------

server <- function(input, output) {
  output$map <- renderLeaflet({
    sm
  })
  
  output$map2 <- renderLeaflet({
    cm
  })
  
  output$map3 <- renderLeaflet({
    bm
  })
  
  output$map4 <- renderLeaflet({
    m
  })
  
  observeEvent(input$n2, {
    outputOptions(output, "map2", suspendWhenHidden = FALSE)
  })
  
  observeEvent(input$n4, {
    outputOptions(output, "map3", suspendWhenHidden = FALSE)
  })
  
  observeEvent(input$n5, {
    outputOptions(output, "map4", suspendWhenHidden = FALSE)
  })
  
  addResourcePath('panes', './panes')
}

shinyApp(ui=htmlTemplate("template.html"), server)
