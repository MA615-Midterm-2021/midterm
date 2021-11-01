library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sp)
library(sf)
library(tidyverse)
library(magrittr)
library(maps)
data(state.fips)
mapstate=st_as_sf(map('state',plot = F,fill = T))
pop <- paste("State:",toupper(mapstate$ID))

mymap<-leaflet(mapstate)%>%
  addTiles()%>%
  addPolygons(data = mapstate %>% filter(ID %in% c("california","florida","new york","north carolina")),
              stroke = T,fillOpacity = 0.2,weight = 0.5,
              color = "red") %>%
  addPolygons(data=mapstate,label=pop,color='white',stroke = T,fillOpacity = 0,weight = 0.5)  
# mymap %>% addProviderTiles(providers$HikeBike.HikeBike)
# mymap %>% addProviderTiles(providers$Esri.DeLorme)


# Firstly get the tidy data set and know how many states are there in the data set.
source('test1025.R')
info <- combine()
states <- distinct(info, State)
total <- nrow(states)

# Return the organic proportion in a given state and year.
organicP <- function(name, year) {
  temp <- dplyr::filter(info, Year==year)
  temp <- dplyr::filter(temp, State == name)
  total <- nrow(temp)
  matches <- grepl(c('ORGANIC'), temp$items)
  o_number <- length(matches[matches==T])
  return(o_number / total)
}

# Return the organic value in a given state and year.
organicV <- function(name, year) {
  temp <- dplyr::filter(info, Year==year)
  temp <- dplyr::filter(temp, State == name)
  temp <- dplyr::filter(temp, Value != ' (D)')
  matches <- grepl(c('ORGANIC'), temp$items)
  ovector <- as.numeric((gsub(",", "", temp$Value[matches])))
  return(sum(ovector))
}

# Plot the organic proportion in a given year.
plot_p <- function(year){
  oratio <- data.frame('States'=states, 'Ratio'=rep(0, length(states)))
  for(i in 1:total) {
    oratio[i, 2] <- organicP(oratio[i, 1], year)
  }
  ggplot(data=oratio) + geom_bar(mapping = aes(x=State, y=Ratio), stat = 'identity', fill = 'green') + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0)) + ggtitle('Organic proportion in each state')
}

# Plot the organic value in a given year
plot_v <- function(year){
  ovalue <- data.frame('States'=states, 'Value'=rep(0, length(states)))
  for(i in 1:total) {
    ovalue[i, 2] <- organicV(ovalue[i, 1], year)
  }
  ggplot(data=ovalue) + geom_bar(mapping = aes(x=State, y=Value), stat = 'identity', fill = 'green') + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0)) + ggtitle('Organic value in each state')
}


#https://post.healthline.com/wp-content/uploads/2020/08/strawberries-1200x628-facebook.20180419205234528-1200x628.jpg
#https://www.bing.com/images/search?view=detailV2&ccid=UFTDjx%2fJ&id=FE80ECCBC548846EACCF294A20EB1386D8198681&thid=OIP.UFTDjx_JY7oF4lzUKhe7nAHaHa&mediaurl=https%3a%2f%2fth.bing.com%2fth%2fid%2fR.5054c38f1fc963ba05e25cd42a17bb9c%3frik%3dgYYZ2IYT6yBKKQ%26riu%3dhttp%253a%252f%252fclipart-library.com%252fimg%252f2082953.jpg%26ehk%3doyGHrXIhE1S03m5xiDxNvNlgRqXlnjvADfGkZUqhMsA%253d%26risl%3d%26pid%3dImgRaw%26r%3d0&exph=1252&expw=1252&q=Cute+Animated+Strawberry&simid=608025162911208268&FORM=IRPRST&ck=AE3AFE67B255B828FDD5068D5014DC81&selectedIndex=12

ui <- dashboardPage(
  dashboardHeader(title='Strawberry'),skin = "red",
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("EDA", tabName = "EDA", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
      fluidRow(
        setBackgroundImage(
          src = "straw_back2.png",shinydashboard=T
        ),
        tabBox(width = 8,
               tabPanel("Esri.NatGeoWorldMap",
                        leafletOutput("map1",height = 650,width=1200)),
               tabPanel("Esri.DeLorme",
                        leafletOutput("map2",height = 650,width=1200))
               ),column(12,
                        
                 
                 
               )
        )
      ),
      
      # Second tab content
      tabItem(tabName = "EDA",
              fluidPage(
                titlePanel("Tabsets"),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    selectInput("Year", "The Type of Plot:",
                                c("2016" = 2016,
                                  "2019" = 2019,
                                  "Compare" = Compare)),
                    radioButtons("rb", "Choose one:",
                                 choiceNames = list("Proportion","Value"),
                                 choiceValues = list("Proportion","Value")
                    )
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot", plotOutput("plot")), 
                      tabPanel("Summary", verbatimTextOutput("summary")), 
                      tabPanel("Table", tableOutput("table"))
                    )
                  )
                )
              )
      )
    )
  ),
  tags$head(tags$style(HTML("
    .skin-blue .main-sidebar {
        background-color:  yellow;
                            }")))
)

server <- function(input, output) { 
  # map_theme <- reactive({
  #   addProviderTiles(providers$Esri.NatGeoWorldMap)
  # })
  output$map1 <- renderLeaflet({
    # Use leaflet() plot
    mymap %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
  })
  output$map2 <- renderLeaflet({
    # Use leaflet() plot
    mymap %>% addProviderTiles(providers$Esri.DeLorme)
  })
  output$plot <- renderPlot({
    if (input$rb=='Proportion'){plot_p(input$Year) %>% print()}
    if (input$rb=='Value'){plot_v(input$Year) %>% print()}
  
  })

  
  }

shinyApp(ui, server)