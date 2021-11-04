# load the data from test1025.R
source('test1025.R')

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")



df1 <- combine()
# dname1 <- 'CHEMICAL'
# year <- 2019

# df1_test <-  df1 %>% filter(dname == dname1 & Year == year)
# df1_stat <- df1_test %>% group_by(State, type) %>% summarise(count = n())
# df2_stat <- df1 %>% group_by(Year, State) %>% summarise(count = n())

# # df1_stat_name <- df1_test %>% group_by(State, type, Pesticide_old)
#   
# p <- df1_stat %>% ggplot() + geom_bar(mapping = aes(x = as.factor(State), y = count, fill = type), 
#                                       stat = "identity", alpha = 0.5) +
#   theme_bw() + labs(x = "State", y = "Pesticide")

plot_chem <- function(year, dname1, plot_type, state = "California"){
  # plot the Chemical observations in different states and years
  # parameters : year --- select a specific year we want to look into
  #              dname1 --- "ORGANIC"/"CHEMICAL", now we only support "CHEMICAL"
  #             plot_type --- "pie"/"bar", the type of figure we want to plot
  #             state --- select a specific state we want to look into
  df1_test <- df1 %>% filter(dname == dname1 & Year == year)
  df1_stat <- df1_test %>% group_by(State, type) %>% summarise(count = n())
  if (plot_type == "bar"){
    p <- df1_stat %>% ggplot() + 
      geom_bar(mapping = 
                 aes(x = as.factor(State), y = count, fill = type), stat = "identity", alpha = 0.5) +
      theme_bw() +
      labs(x = "State", y = "Pesticide")  # reset the labs
  } else if (plot_type == "pie"){
    state %<>% toupper()
    df1_stat_state <- df1_stat %>% filter(State == state) # select the specific state
    # calculate the percentage of observation amount in one state among the total amount of observations 
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
    p <- p1 %>% ggpar(title = paste0("Chemical usage percentage in ",state, " in ",year), # add the legend
                      legend = "right")
  } else {
    stop("Please check the State!!")
  }
  
  p
}
# <demo>
# p1 <- plot_chem(2019, 'CHEMICAL', "bar")
# p2 <- plot_chem(2016, 'CHEMICAL', "bar")
# p3 <- plot_chem(2019, 'CHEMICAL', "pie")
# p4 <- plot_chem(2019, 'CHEMICAL', "pie", "Florida")
# p5 <- plot_chem(2016, 'CHEMICAL', "pie")
# p6 <- plot_chem(2016, 'CHEMICAL', "pie", "Florida")
# 
# p3
# p4
# p5
# p6
# </demo>

# Below is the html code to create one scroll-box

# <div style="width:150px;height:150px;line-height:3em;overflow:scroll;padding:5px;">
#   This 'div' element uses 'overflow:scroll' to create scrollbars. 
# </div>



