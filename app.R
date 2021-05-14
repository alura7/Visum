#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(shiny)
library(maps)
library(sf)
library(glue)
library(htmltools)
library(htmltools)
library(rsconnect)

file <- unzip("World_Countries_(Generalized).zip")

countries_map <- st_read("World_Countries__Generalized_.shp")

# traveling to Germany do I need a visa?
visum_data <- readRDS("visum_data.rds")

#with German passport do  you need a visa?
visum_data_ger <- readRDS("visum_data_ger.rds")

#Covid-19 update
visum_data_en <- readRDS("visum_data_en.rds")


# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$head(
    
  navbarPage("Visum?", id="main",
  
   # Application title
   tabPanel("Traveling to Germany", 
          div(class = "outer",
            theme = shinythemes::shinytheme("simplex"),
             # browsable(
             #   tagList(list(
               #  tags$head(
                  tags$style(
                    ".leaflet-tooltip{ width: 150px; white-space: normal; }"
                ),
                leafletOutput('map',  height =700),
               
          )
   
   ),
   tabPanel("Traveling from Germany",
            tags$style(
              ".leaflet-tooltip{ width: 150px; white-space: normal; }"
            ),   
            leafletOutput('map2',  height = 700)
),
tabPanel("About",  
         verbatimTextOutput("About"))
)
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  colnames(visum_data)
  content <- paste(sep = "<br/>",visum_data$no_vis_num)
  
  label_map <- glue('<strong> <u> {visum_data$COUNTRYAFF}</u></strong> <br /> 
                     <strong> <u> Visum required?</u></strong> : <br/>
                    {visum_data$`yes/no`} <br />
                     {visum_data$no_vis_num}') %>%  lapply(htmltools::HTML)
  
   output$map <- leaflet::renderLeaflet({
      leaflet() %>% 
       addProviderTiles("CartoDB.Positron") %>% 
       setView(13.76134, 52.675499, zoom = 2) %>% 
       addPolygons(
         data = visum_data,
         label = label_map,
         labelOptions = labelOptions(style = list( "font-family"= "Helvetica")),
                                    
         popup = content,
         smoothFactor = 0.5,
         weight = 0.5,
         fillOpacity = 0.01,
         color = "#444444",
         highlightOptions = highlightOptions(color = "white", weight = 2,
                                             bringToFront = TRUE)
       )
   })
   
   
   label_map2 <- glue('<strong> <u> {visum_data_ger$COUNTRYAFF}</u></strong> <br /> 
                     <strong> <u> Visum required?</u></strong> : <br/>
                     {visum_data_ger$visum} <br/>
                      <br/>
                      <u>Covid-19 Update 2021</u>: <br/>
                      {visum_data_en$visum}') %>%  lapply(htmltools::HTML)
   
   output$map2 <- leaflet::renderLeaflet({
        leaflet() %>% 
       addProviderTiles("CartoDB.Positron") %>% 
       setView(13.76134, 52.675499, zoom = 2) %>% 
       addPolygons(
         data = visum_data,
         label = label_map2,
         labelOptions = labelOptions(style = list( "font-family"= "Helvetica")),

         smoothFactor = 0.5,
         weight = 0.5,
         fillOpacity = 0.01,
         color = "#444444",
         highlightOptions = highlightOptions(color = "white", weight = 2,
                                             bringToFront = TRUE
                                             )
       )
   })
   
   
   output$About <- renderText({ 
     glue(
      'Source/Quelle: 
       passportindex.org & https://www.auswaertiges-amt.de/en/einreiseundaufenthalt/-/231148 
       https://www.solo-urlaub.de/alle-visafreien-laender-im-ueberblick/

       As of January 2019 / Information without guarantee.
       Stand: Januar 2019 / Angaben ohne Gewähr.

       Covid-19 update - 2021 - passportindex.org
       Information without guarantee
       ') %>%  lapply(htmltools::HTML) %>% paste0()
     
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

