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
library(plotly)
library(DT)
library(rvest)
library(scales)

# The shiny script generally divided into 3 parts. 
# First part prepares the data for the shiny. 
# Second part set the ui for the shiny.
# Third part set the server for the shiny.
# We comment notes in each part and the comments start with '#'

# source('test1025.R')
# df1<-combine()
# write.csv(df1, file = "my_data.csv")
# read data
df1<-read.csv('my_data.csv')
# read map data
data(state.fips)
#convert map data form
mapstate=st_as_sf(maps::map('state',plot = F,fill = T))
# upper the character
pop <- paste("State:",toupper(mapstate$ID))

# Firstly get the tidy data set and know how many states are there in the data set.
info <- df1
states <- distinct(info, State)
total <- nrow(states)
# calculate the organic proportion
organicP <- function(name, year) {
  temp <- dplyr::filter(info, Year==year)
  temp <- dplyr::filter(temp, State == name)
  total <- nrow(temp)
  matches <- grepl(c('ORGANIC'), temp$items)
  o_number <- length(matches[matches==T])
  return(round(o_number / total, 2))
}

# Return the organic price value or CWT value in a given state
# and year. (price unit is 1000$)
organicV <- function(name, year, type) {
  temp <- dplyr::filter(info, Year==year)
  temp <- dplyr::filter(temp, State == name)
  temp <- dplyr::filter(temp, Value != ' (D)')
  if (type == 'price') {
    temp <- dplyr::filter(temp, measurement != 'MEASURED IN $')
  } else if (type == 'CWT') {
    temp <- dplyr::filter(temp, measurement != 'MEASURED IN CWT')
  } else stop('The type input should be either price or CWT, try again')
  matches <- grepl(c('ORGANIC'), temp$items)
  ovector <- as.numeric((gsub(",", "", temp$Value[matches])))
  return(round(sum(ovector)/ 1000))
}


# Plot the organic proportion in a given year.
plot_p <- function(year){
  oratio <- data.frame('States'=states, 'Ratio'=rep(0, length(states)))
  for(i in 1:total) {
    oratio[i, 2] <- organicP(oratio[i, 1], year)
  }
  ggplot(data=oratio) + geom_bar(mapping = aes(x=State, y=Ratio), stat = 'identity', fill = 'Orange') + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0)) + ggtitle('Organic proportion in each state') +
    geom_text(aes(x=State, y=Ratio, label=Ratio)) + ylim(0, 1)+theme_bw()
}

# Plot the organic price value or CWT value in a given year and type(price unit is 1000$)
plot_v <- function(year, type){
  ovalue <- data.frame('States'=states, 'Value'=rep(0, length(states)))
  for(i in 1:total) {
    ovalue[i, 2] <- organicV(ovalue[i, 1], year, type)
  }
  ggplot(data=ovalue) + geom_bar(mapping = aes(x=State, y=Value), stat = 'identity', fill = 'orange') + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0)) + ggtitle('Organic value in each state') +
    geom_text(aes(x=State, y=Value, label=Value))+theme_bw()
}

##Jinyu Li's wode:
df1 = info 
dname1 <- 'CHEMICAL'

#Plot multipile graphs in one plot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#plot chemical plot
plot_chem <- function(df1,year, dname1, plot_type, state = "California"){
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
                 aes(x = as.factor(State), y = count, fill = type), stat = "identity",position='fill', alpha = 0.5) +
      theme_bw() +
      labs(x = "State", y = "Pesticide")
  } 
  else if (plot_type == "pie"){
    state %<>% toupper()
    df1_stat_state <- df1_stat %>% filter(State == state)
    percentage <- scales::percent(df1_stat_state$count / sum(df1_stat_state$count))
    labs <- percentage #set the labs
    # labs <- paste(df1_stat_state$type, '(', percentage, ')', sep = '') #set the labs
    # p <- df1_stat_state %>% ggplot(aes(x = "", y = count, fill = type)) + 
    #   geom_bar(stat = "identity", width = 1) +
    #   # geom_text(aes(label = labs)) +
    #   # theme_bw() +
    #   # labs(x = "", y = "", title = paste0("Chemical usage percentage in",state)) + 
    #   coord_polar(theta = "y", start = 0, direction = 1)
    p1 <- df1_stat_state %>% 
      ggpie(x = "count", 
            fill = "type", 
            palette = "jco",
            label = labs, lab.pos = "in", lab.font = c(4, "white"))
    p <- p1 %>% ggpar(title = paste0("Chemical usage percentage in ",state, " in ", year),
                      legend = "right")
  } else {
    stop("Please check the State!!")
  }
  
  p
}

#get plotly graph
result=info
chemical<-filter(result,dname=="CHEMICAL") 
chemical_type<-summarise(group_by(chemical,State),count=n())
chemical_type1<-summarise(group_by(chemical,type),count=n())
chemical_type2<-summarise(group_by(chemical,Year),count=n())
#screen data in 2019
chemical_19<-filter(chemical,Year=="2019")
FUNGICIDE_19<-summarise(group_by(filter(chemical_19,type=="FUNGICIDE"),State),count=n())
HERBICIDE_19<-summarise(group_by(filter(chemical_19,type=="HERBICIDE"),State),count=n())
INSECTICIDE_19<-summarise(group_by(filter(chemical_19,type=="INSECTICIDE"),State),count=n())
OTHER_19<-summarise(group_by(filter(chemical_19,type=="OTHER"),State),count=n())
CALIFORNIA_19<-summarise(group_by(filter(chemical_19,State=="CALIFORNIA"),type),count=n())
FLORIDA_19<-summarise(group_by(filter(chemical_19,State=="FLORIDA"),type),count=n())
#screen data in 2018
chemical_18<-filter(chemical,Year=="2018")
FUNGICIDE_18<-summarise(group_by(filter(chemical_18,type=="FUNGICIDE"),State),count=n())
HERBICIDE_18<-summarise(group_by(filter(chemical_18,type=="HERBICIDE"),State),count=n())
INSECTICIDE_18<-summarise(group_by(filter(chemical_18,type=="INSECTICIDE"),State),count=n())
OTHER_18<-summarise(group_by(filter(chemical_18,type=="OTHER"),State),count=n())
CALIFORNIA_18<-summarise(group_by(filter(chemical_18,State=="CALIFORNIA"),type),count=n())
#FLORIDA_18<-summarise(group_by(filter(chemical_18,State=="FLORIDA"),type),count=n())

#graph 2019CALIFORNIA
#CALIFORNIA <- data.frame(CALIFORNIA)
o_status = c("FUNGICIDE","HERBICIDE","INSECTICIDE","OTHER")
CALIFORNIA_19<-plot_ly(CALIFORNIA_19,x =~type,y = ~count,color = o_status,alpha = 0.5) %>%
  #add_text(text = 'test') %>%
  #add_markers() %>%
  add_bars() %>%
  layout(
    title = '2019CALIFORIA',
    legend = list(
      borderwidth = 1,
      orientation = 'h'
    ),
    margin = list(
      l = 70,
      r = 80
    )
  ) %>% layout(
    xaxis = list(title = "")
  )
#graph 2019FLORIDA
o_status = c("FUNGICIDE","HERBICIDE","INSECTICIDE","OTHER")
FLORIDA_19<-plot_ly(FLORIDA_19,x =~type,y = ~count,color = o_status,alpha = 0.5) %>%
  #add_text(text = 'test') %>%
  #add_markers() %>%
  add_bars() %>%
  layout(
    title = '2019FLORIDA',
    legend = list(
      borderwidth = 1,
      orientation = 'h'
    ),
    margin = list(
      l = 70,
      r = 80
    )
  ) %>% layout(
    xaxis = list(title = "")
  )
#graph 2018CALIFORNIA
o_status = c("FUNGICIDE","HERBICIDE","INSECTICIDE")
CALIFORNIA_18<-plot_ly(CALIFORNIA_18,x =~type,y = ~count,color = o_status,alpha = 0.5) %>%
  #add_text(text = 'test') %>%
  #add_markers() %>%
  add_bars() %>%
  layout(
    title = '2018CALIFORIA',
    legend = list(
      borderwidth = 1,
      orientation = 'h'
    ),
    margin = list(
      l = 70,
      r = 80
    )
  )

#set ui
ui <- dashboardPage(
  #set header sidebar body
  dashboardHeader(title='Strawberry'),skin = "red", 
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("EDA",icon = icon("bar-chart-o"),
             menuSubItem("Chemical Overview",tabName = "P_vs_O"),
             menuSubItem("Organic vs Chemical",tabName = "organicvspesticides"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
        fluidRow(
        setBackgroundImage( #set background
          src = "straw_back2.png",shinydashboard=T
        ),
        tabBox(width = 10,
               tabPanel("Esri.NatGeoWorldMap",
                        leafletOutput("map1",height = 650,width=1200)),
               tabPanel("Esri.DeLorme",
                        leafletOutput("map2",height = 650,width=1200))
               ),
        tabBox(width=2,  selectInput("sc", "Sample Count:",
                                             c(
                                               '2019'=2019,
                                               '2018'=2018,
                                               '2017'=2017,
                                               '2016'=2016))   ),

            )
        
        
        
        )
      ,
      
      # Second tab content
      tabItem(tabName = "P_vs_O",
              fluidPage(
                titlePanel("Chemical Overview"),
                
                box(width = 4,height=12,
                    plotOutput("DF"),
                    title = "Chemical Overview",
                    selectInput("Year_PD", "The Type of Plot:",
                                c("2016" = 2016,
                                  "2019" = 2019)),
                    selectInput("Type_PD", "Select Type:",
                                c(
                                  "Bar Count Plot" = "bar",
                                  "Bar Proportion Plot"="bar_prop"))
                    
                ),
                box(width = 6,height=12,
                    plotOutput("DF_pie"),
                    title = "Pie Plot in Each State",
                    selectInput("Year_PD2", "The Type of Plot:",
                                c("2016" = 2016,
                                  "2018" = 2018,
                                  "2019" = 2019)),
                    selectInput("Type_State", "Select State:",
                                c("California" = "California",
                                  "Florida" = "Florida",
                                  "Oregon" = "Oregon",
                                  "Washington" = "Washington"))
               

                )
              )),
      # Third tab content
      tabItem(
              tabName = "organicvspesticides",
              fluidPage(
                titlePanel("Organic vs Cheminal"),
                
                box(width = 4,height=12,
                    title = "Organic Proportion and Value",    
                    plotOutput("OR"),
                    selectInput("Year", "The Type of Plot:",
                                c("2016" = 2016,
                                  "2019" = 2019)),
                    radioButtons("rb", "Choose one:",
                                 choiceNames = list("Proportion","Value CWT","Value Price"),
                                 choiceValues = list("Proportion","Value CWT","Value Price")
                    )                ),
                box(width = 8,height=12,
                    title = "Chemical",    
                    plotlyOutput("Chemi"),
                    selectInput("Year_State", "The Type of Plot:",
                                c(
                                  "CALIFORNIA2019" = 'CALIFORNIA2019',
                                  "FLORIDA2019" = 'FLORIDA2019'))
                    )
              )
      )
     
      )
    )
  )

# set server
server <- function(input, output) { 
  pie_map <- reactive({# use try to identify if the proper plot can appear
    if('try-error' %in% class(try(plot_chem(df1,year=input$Year_PD2, 'CHEMICAL', 'pie',state=input$Type_State),silent=TRUE))){
     ggplot(data = data.frame(x = "No data"), aes(x=x))+theme_bw()
    }else{plot_chem(df1,year=input$Year_PD2, 'CHEMICAL', 'pie',state=input$Type_State)
    }
   })
  tmp_map <- reactive({#map reactive
    y<-as.numeric(input$sc)
    tmpdf<-df1 %>% dplyr::filter(Year %in% y )%>% group_by(State) %>% summarise(count = n()) #groupby year
    colnames(tmpdf)[1]="ID"   #change ID
    tmpdf$ID<-tolower(tmpdf$ID) #lower ID 
    sub_state<-inner_join(mapstate,tmpdf,by='ID') #inner join two dataframe
    #set color
    pal <- colorNumeric(
        palette = "Reds",
        domain = sub_state$count
      )
    #change pop text
    pop_modi<-pop
    modi<-paste(pop[which(mapstate$ID %in% sub_state$ID)],"count:",sub_state$count)
    pop_modi[which(mapstate$ID %in% sub_state$ID)]<-modi 
    # leaflet plot add two polygons minimap and legend
    map2<-leaflet(mapstate) %>% addTiles() %>% 
        addPolygons(data=sub_state,stroke = FALSE, smoothFactor = 0.2, fillOpacity = .7,
                    color = ~pal(sub_state$count)) %>% 
        addPolygons(data=mapstate,color='white',label=pop_modi,stroke = T,fillOpacity = 0,weight = 0.5) %>%  addMiniMap() %>%
        addLegend("bottomright", pal = pal, values = ~sub_state$count,
                  title = "Sample Count",
                  
                  opacity = .7
        )
    map2
 
    
  })
  output$map1 <- renderLeaflet(
    # use leaflet plot
    tmp_map() %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
    )
  output$map2 <- renderLeaflet({ 
    # use leaflet plot and use another title
    tmp_map() %>% addProviderTiles(providers$Esri.DeLorme)
  })
  output$OR <- renderPlot({ #use ggplot
    if (input$rb=='Proportion'){plot_p(input$Year) %>% print()}
    #if (input$rb=='Value'){plot_v(input$Year) %>% print()}
    if (input$rb=='Value CWT'){plot_v(input$Year,'CWT') %>% print()}
    if (input$rb=='Value Price'){plot_v(input$Year,'price') %>% print()}
    
  
  })
  output$DF <- renderPlot({ #use ggplot
    plot_chem(df1,input$Year_PD, 'CHEMICAL', input$Type_PD)
    
  })

  output$DF_pie <- renderPlot({ # use ggplot 
    pie_map() 
    #plot_chem(df1,year=input$Year_PD2, 'CHEMICAL', 'pie',state=input$Type_State)
  })
  output$Chemi <- renderPlotly({ #use plotly
    #plot_ly(iris, x = ~get(input$choice), y = ~Sepal.Length, type = 'scatter', mode = 'markers') %>% print()
    if (input$Year_State=='CALIFORNIA2019'){CALIFORNIA_19 %>% print()}
    else if (input$Year_State=='FLORIDA2019'){FLORIDA_19 %>% print()}
  })
  
  }

shinyApp(ui, server)




