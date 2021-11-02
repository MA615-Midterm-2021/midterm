source('test1025.R')

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

# load the data from test1025.R

df1 <- combine()
# dname1 <- 'CHEMICAL'
# year <- 2019

# df1_test <-  df1 %>% filter(dname == dname1 & Year == year)
# df1_stat <- df1_test %>% group_by(State, type) %>% summarise(count = n())
# # df1_stat_name <- df1_test %>% group_by(State, type, Pesticide_old)
#   
# p <- df1_stat %>% ggplot() + geom_bar(mapping = aes(x = as.factor(State), y = count, fill = type), 
#                                       stat = "identity", alpha = 0.5) +
#   theme_bw() + labs(x = "State", y = "Pesticide")

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

plot_chem <- function(year, dname1, plot_type, state = "California"){
  df1_test <- df1 %>% filter(dname == dname1 & Year == year)
  df1_stat <- df1_test %>% group_by(State, type) %>% summarise(count = n())
  if (plot_type == "bar"){
    p <- df1_stat %>% ggplot() + 
      geom_bar(mapping = 
                 aes(x = as.factor(State), y = count, fill = type), stat = "identity", alpha = 0.5) +
      theme_bw() +
      labs(x = "State", y = "Pesticide")
  } else if (plot_type == "pie"){
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
    p <- p1 %>% ggpar(title = paste0("Chemical usage percentage in ",state, " in", year),
                      legend = "right")
  } else {
    stop("Please check the State!!")
  }
  
  p
}
# <demo>
p1 <- plot_chem(2019, 'CHEMICAL', "pie")
p2 <- plot_chem(2018, 'CHEMICAL', "pie")
p3 <- plot_chem(2016, 'CHEMICAL', "pie")

multiplot(p1,p2,p3, cols = 1)


# </demo>



# name tag

# df_distinct_state <- distinct(df1_test, State)
# df1_test_1 <- df1 %>% filter(State == "CALIFORNIA")
# 
# df1_test_1_stat <- df1_test_1 %>% group_by(type, Pesticide_old) %>% summarise(count = n())
# 
# df1_test_1_stat[order(df1_test_1_stat$count, decreasing = T),]
#   
# p1 <- df1_test_1_stat %>% ggplot() + geom_bar(mapping = aes(x = as.factor(type), y = count, fill = Pesticide_old), 
#                                       stat = "identity", alpha = 0.5) +
#   theme_bw() +
#   labs(x = "State", y = "Pesticide_name")
# 
# p2 <- df1_test_1_stat %>% ggplot() + geom_
# 
# plot_pie <- function(year, dname1){
#   
# }
