#ui
library(shiny)
library(leaflet)
library(reshape)
library(rgdal)

shinyUI(fluidPage(
  titlePanel(
    HTML("<header><center>
         <h2> <strong>Percentage of Students Speaking Portugees/Elomwe/Emakua in Mozambique</strong> 
         <p> <font size= 5> by District </font> </p> 
         </h2> </center>
         </header>"
    )),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(HTML("
                                .selectize-input, .selectize-dropdown {
                                font-size: 100%;
                                }
                                "))), 
      
      tags$head(tags$style(HTML("
                                .has-items, .item {
                                font-size: 100%;
                                color: #00008B; 
                                }
                                "))), 
      
      tags$head(tags$style(HTML("s
                                li , .data-value {
                                font-size: 200%;
                                
                                }
                                "))), 
      tags$head(tags$style(HTML("
                                .well , control-label {
                                font-size: 100%;
                                color: #00008B; 
                                }
                                "))),
      tags$head(tags$style(HTML(".leaflet-container { background: #FFF; }"))), 

      
      selectInput("student_lang",
                  label = "Choose Language to Display",
                  choices = c("Portuguees","Elomwe", "Emakua"),
                  selected = "Portuguees") ,  width = 2
      ),
    
    mainPanel(
      tabsetPanel(id = "tabs" , selected = "Portuguees", 
                  tags$style(type = "text/css", "#mymap, #mymap_2, #mymap_3  {height: calc(100vh - 80px) !important;}"),
                  
                  tabPanel("Portuguees", 
                           leafletOutput("mymap"), 
                           p() ), 
                  tabPanel("Elomwe", 
                     leafletOutput("mymap_2"), 
                     p() ), 
                  tabPanel("Emakua", 
                     leafletOutput("mymap_3"), 
                     p() )
                )
            )
        )
    )
)




