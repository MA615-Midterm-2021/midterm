library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sp)
library(sf)
library(tidyverse)
library(magrittr)
library(maps)
library(ggpubr)
library(rgdal)
data(state.fips)
mapstate=st_as_sf(map('state',plot = F,fill = T))
pop <- paste("State:",toupper(mapstate$ID))

sub_state=mapstate %>% filter(ID %in% c("california","florida","oregon","washington"))
sub_state$item=c(100,200,300,400)


mymap<-leaflet(mapstate)%>%
  addTiles()%>%
  addPolygons(data = sub_state,
              stroke = T,fillOpacity = 0.2,weight = 0.5,
              color = "red") %>%
  addPolygons(data=mapstate,label=pop,color='white',stroke = T,fillOpacity = 0,weight = 0.5) %>%  addMiniMap()
# mymap %>% addProviderTiles(providers$HikeBike.HikeBike)
# mymap %>% addProviderTiles(providers$Esri.DeLorme)

#colorBin("Greens", domain = 0:100)
pal <- colorNumeric(
  palette = "RdYlBu",
  domain = sub_state$item
)
pop_modi<-pop
modi<-paste0(pop[which(mapstate$ID %in% c("california","florida","oregon","washington"))],' ',c(100,200,300,400))
pop_modi[which(mapstate$ID %in% c("california","florida","oregon","washington"))]<-modi

map2<-leaflet(mapstate) %>% addTiles() %>% 
  addPolygons(data=sub_state,stroke = FALSE, smoothFactor = 0.2, fillOpacity = .7,
              color = ~pal(sub_state$item)
  ) %>% addPolygons(data=mapstate,label=pop_modi,color='white',stroke = T,fillOpacity = 0,weight = 0.5) %>%  addMiniMap() %>%
  addLegend("bottomright", pal = pal, values = ~sub_state$item,
            title = "??",
            labFormat = labelFormat(prefix = "??"),
            opacity = .7
  )

map2 %>%addProviderTiles(providers$Esri.DeLorme)



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

##Master Li's wode:
df1 = info 
plot_chem <- function(year, dname1, plot_type, state = "California"){
  df1_test <- df1 %>% filter(dname == dname1 & Year == year)
  df1_stat <- df1_test %>% group_by(State, type) %>% summarise(count = n())
  if (plot_type == "bar"){
    p <- df1_stat %>% ggplot() + 
      geom_bar(mapping = 
                 aes(x = as.factor(State), y = count, fill = type), stat = "identity", alpha = 0.5) +
      theme_bw() +
      labs(x = "State", y = "Pesticide")
  } 
  else if (plot_type == "bar_prop"){
    p <- df1_stat %>% ggplot() + 
      geom_bar(mapping = 
                 aes(x = as.factor(State), y = count, fill = type),position = "fill", stat = "identity", alpha = 0.5) +
      theme_bw() +
      labs(x = "State", y = "Pesticide")
    
  }
  
  
  else if (plot_type == "pie"){
    state %<>% toupper()
    df1_stat_state <- df1_stat %>% filter(State == state)
    percentage <- scales::percent(df1_stat_state$count / sum(df1_stat_state$count))
    labs <- paste(df1_stat_state$type, '(', percentage, ')', sep = '') #set the labs
    # p <- df1_stat_state %>% ggplot(aes(x = "", y = count, fill = type)) + 
    #   geom_bar(stat = "identity", width = 1) +
    #   # geom_text(aes(label = labs)) +
    #   # theme_bw() +
    #   # labs(x = "", y = "", title = paste0("Chemical usage percentage in",state)) + 
    #   coord_polar(theta = "y", start = 0, direction = 1)
    p1 <- df1_stat_state %>% 
      ggpie(x = "count", 
            fill = "type", alpha=0.5,
            palette = "jco",
            label = labs, lab.pos = "in", lab.font = c(4, "white"))
    p <- p1 %>% ggpar(title = paste0("Chemical usage percentage in ",state),
                      legend = "right")
  } else {
    stop("Please check the State!!")
  }
  
  p
}

chemical<-filter(df1,dname=="CHEMICAL")
chemical_type<-summarise(group_by(chemical,State),count=n())

ui <- dashboardPage(
  dashboardHeader(title='Strawberry'),skin = "red",
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("EDA",icon = icon("dashboard"),
             menuSubItem("Pesticide vs Organic",tabName = "P_vs_O"),
             menuSubItem("Graphs",tabName = "fundeda"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
      fluidRow(
        setBackgroundImage(
          src = "straw_back2.png",shinydashboard=T
        ),
        tabBox(width = 10,
               tabPanel("Esri.NatGeoWorldMap",
                        leafletOutput("map1",height = 650,width=1200)),
               tabPanel("Esri.DeLorme",
                        leafletOutput("map2",height = 650,width=1200))
               ),
        tabBox(width=2,radioButtons("LI", "Label Information:",
                                    choiceNames = list("State","State"),
                                    choiceValues = list("Unknown","Unknwon")
               ))
        
        
        
        )
      ),
      
      # Second tab content
      tabItem(tabName = "P_vs_O",
              fluidPage(
                titlePanel("Pesticide vs Organic"),
                
                box(width = 4,height=8,
                    plotOutput("DF"),
                    title = "Pesticide Distribution",
                    selectInput("Year_PD", "The Type of Plot:",
                                c("2016" = 2016,
                                  "2019" = 2019)),
                    selectInput("Type_PD", "Select Type:",
                                c("Pie Plot" = "pie",
                                  "Bar Count Plot" = "bar",
                                  "Bar Proportion Plot"="bar_prop"))
                    
                ),
                box(width = 4,height=8,
                    title = "Organic Ratio",    
                    plotOutput("OR"),
                    selectInput("Year", "The Type of Plot:",
                                c("2016" = 2016,
                                  "2019" = 2019,
                                  "Compare" = Compare)),
                    radioButtons("rb", "Choose one:",
                                 choiceNames = list("Proportion","Value"),
                                 choiceValues = list("Proportion","Value")
                    )                ),
                
               

                )
              )
      )
    )
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
  output$OR <- renderPlot({
    if (input$rb=='Proportion'){plot_p(input$Year) %>% print()}
    if (input$rb=='Value'){plot_v(input$Year) %>% print()}
  
  })
  output$DF <- renderPlot({
    plot_chem(input$Year_PD, 'CHEMICAL', input$Type_PD)
    
  })

  
  }

shinyApp(ui, server)