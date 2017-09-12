library('shiny')

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Mozambique Wireframe"),
  
  sidebarPanel(),
  
  mainPanel(
    plotOutput("moz.map")
  )
))