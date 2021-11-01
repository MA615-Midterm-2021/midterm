source('test1025.R')

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(ggplot2)
library(ggpubr)

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
    labs <- paste(df1_stat_state$type, '(', percentage, ')', sep = '') #set the labs
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
    p <- p1 %>% ggpar(title = paste0("Chemical usage percentage in ",state),
                      legend = "right")
  } else {
    stop("Please check the State!!")
  }
  
  p
}
# <demo>

# plot_chem(2016, 'CHEMICAL', "bar")

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
