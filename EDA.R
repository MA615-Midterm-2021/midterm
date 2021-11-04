library(ggplot2)
library(plotly)
library(DT)
library(rvest)
library(dplyr)
library(scales)
install.packages("maps")
library(maps)

source('test1025.R')
result<-combine() %>% view()

chemical<-filter(result,dname=="CHEMICAL")
chemical_type<-summarise(group_by(chemical,State),count=n())
chemical_type1<-summarise(group_by(chemical,type),count=n())
chemical_type2<-summarise(group_by(chemical,Year),count=n())
#筛选出19年的数据
chemical_19<-filter(chemical,Year=="2019")
FUNGICIDE_19<-summarise(group_by(filter(chemical_19,type=="FUNGICIDE"),State),count=n())
HERBICIDE_19<-summarise(group_by(filter(chemical_19,type=="HERBICIDE"),State),count=n())
INSECTICIDE_19<-summarise(group_by(filter(chemical_19,type=="INSECTICIDE"),State),count=n())
OTHER_19<-summarise(group_by(filter(chemical_19,type=="OTHER"),State),count=n())
CALIFORNIA_19<-summarise(group_by(filter(chemical_19,State=="CALIFORNIA"),type),count=n())
FLORIDA_19<-summarise(group_by(filter(chemical_19,State=="FLORIDA"),type),count=n())
#筛选出18年的数据
chemical_18<-filter(chemical,Year=="2018")
FUNGICIDE_18<-summarise(group_by(filter(chemical_18,type=="FUNGICIDE"),State),count=n())
HERBICIDE_18<-summarise(group_by(filter(chemical_18,type=="HERBICIDE"),State),count=n())
INSECTICIDE_18<-summarise(group_by(filter(chemical_18,type=="INSECTICIDE"),State),count=n())
OTHER_18<-summarise(group_by(filter(chemical_18,type=="OTHER"),State),count=n())
CALIFORNIA_18<-summarise(group_by(filter(chemical_18,State=="CALIFORNIA"),type),count=n())
#FLORIDA_18<-summarise(group_by(filter(chemical_18,State=="FLORIDA"),type),count=n())

#绘图 2019CALIFORNIA
#CALIFORNIA <- data.frame(CALIFORNIA)
o_status = c("FUNGICIDE","HERBICIDE","INSECTICIDE","OTHER")
plot_ly(CALIFORNIA_19,x=~type,y = ~count,color = o_status,alpha = 0.5) %>%
  #add_text(text = 'test') %>%
  #add_markers() %>%
  add_bars() %>%
  layout(
    title = '2019CALIFORIA',
    legend = list(
      borderwidth = 1,
      orientation = 'h'
    ),
    xaxis = list(title = ""), #去掉X轴名称type 
    margin = list(
      l = 70,
      r = 80
    )
  )

#绘图 2019FLORIDA
o_status = c("FUNGICIDE","HERBICIDE","INSECTICIDE","OTHER")
plot_ly(FLORIDA_19,x =~type,y = ~count,color = o_status,alpha = 0.5) %>%
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
  )
#绘图 2018CALIFORNIA
o_status = c("FUNGICIDE","HERBICIDE","INSECTICIDE")
plot_ly(CALIFORNIA_18,x =~type,y = ~count,color = o_status,alpha = 0.5) %>%
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




