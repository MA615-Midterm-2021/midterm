library(tidyverse)
library(magrittr)

install.packages("data.table")
install.packages("dplyr")
install.packages("formattable")
install.packages("tidyr")
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

source('test1025.R')
result<-combine() %>% view()

#count
Carcinogen<-subset(result,Carcinogen!="NA")
Carcinogen1<-summarise(group_by(Carcinogen,Carcinogen),count=n())
Hormone.Disruptor<-subset(result,Hormone.Disruptor!="NA")
Hormone.Disruptor1<-summarise(group_by(Hormone.Disruptor,Hormone.Disruptor),count=n())
Neurotoxins<-subset(result,Neurotoxins!="NA")
Neurotoxins1<-summarise(group_by(Neurotoxins,Neurotoxins),count=n())
Developmental.or.Reproductive.Toxins<-subset(result,!`Developmental.or.Reproductive.Toxins` %>% is.na())
Developmental.or.Reproductive.Toxins1<-summarise(group_by(Developmental.or.Reproductive.Toxins,Developmental.or.Reproductive.Toxins),count=n())
Bee.Toxins<-subset(result,Bee.Toxins!="NA")
Bee.Toxins1<-summarise(group_by(Bee.Toxins,Bee.Toxins),count=n())


df<-cbind(Carcinogen1,Hormone.Disruptor1,Neurotoxins1,Developmental.or.Reproductive.Toxins1)
colnames(Carcinogen1)=c('condition','Carcinogen')
colnames(Hormone.Disruptor1)=c('condition','Hormone.Disruptor')
colnames(Neurotoxins1)=c('condition','Neurotoxins')
colnames(Developmental.or.Reproductive.Toxins1)=c('condition','Developmental.or.Reproductive.Toxins')
human.toxins<-full_join(Carcinogen1,Hormone.Disruptor1,by='condition') %>% full_join(Neurotoxins1,by='condition') %>% 
  full_join(Developmental.or.Reproductive.Toxins1,by='condition')
human.toxins[human.toxins %>% is.na()]=0
human.toxins
#human.toxins-------------------------------------
#set color
customGreen0="#DeF7E9"
customGreen="#71CA97"
customRed="#ff7f7f"
#view data with the formattable package
formattable(human.toxins)
#add color tile for columnns
formattable(
  human.toxins,align=c("l","c","l","c","l"),list(
    'condition'=color_bar(customRed),
    'Carcinogen'=color_tile(customGreen,customGreen0),
    'Hormone.Disruptor'=color_tile(customGreen,customGreen0),
    'Neurotoxins'=color_tile(customGreen,customGreen0),
    'Developmental.or.Reproductive.Toxins'=color_tile(customGreen,customGreen0)
  )
)

#Bee.Toxins----------------------------------------
#set color
customGreen0="#DeF7E9"
customGreen="#71CA97"
customRed="#ff7f7f"
#view data with the formattable package
#formattable(Bee.Toxins1)
#add color tile for columnns
formattable(
  Bee.Toxins1,align=c("l","c"),list(
    #'Bee.Toxins'=formatter("span",style=~style(color="grey",font.weight="bold"))
    'Bee.Toxins'=color_tile(customGreen,customGreen0),
    'count'=color_bar(customRed)
  )
)






