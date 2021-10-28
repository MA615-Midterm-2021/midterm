library(tidyverse)
library(magrittr)


drop_no_info_cols <- function(df){
  cnames = colnames(df)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

straw_clean<-function(strawb){
  
  strawb <- drop_no_info_cols(strawb)
  strawb %<>% separate(col=Data.Item,
                       into = c("Strawberries", "items", "discription", "units"),
                       sep = ",",
                       fill = "right")
  ## Separte Domain into 2 columns
  strawb %<>%  separate(col=Domain,
                        into = c("dname", "type" ), 
                        sep = ",", 
                        fill = "right")
  
  ## make a copy of Domain.Categoy
  strawb %<>% 
    mutate(Chemicals = Domain.Category) %>% 
    relocate(Chemicals, .after = Domain.Category) 
  
  
  ## vector of logicals for each row with "CHEM" at 
  ## the start of strawb$Chemicals
  bb <- strawb$Chemicals %>% str_detect("CHEM")
  
  ## index 
  ind_C <- (!bb)*(1:dim(strawb)[1])
  r1 <- ind_C[ind_C > 0]
  
  strawb$Chemicals[r1] <- " "
  
  ## now we need a list of chemicals
  Sys.setlocale(category = "LC_CTYPE", locale = "C")
  strawb %<>% separate(col = Chemicals,
                       into = c("title", "details"),
                       sep = ":",
                       fill = "right")
  
  strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )
  #match all the character besides ()
  
  strawb %<>% mutate(type = str_trim(type))
  return(strawb)
}


pest_clean<-function(pest){
  #delete NA rows
  pest %<>% t() %>% data.frame() %>% drop_no_info_cols() %>% t() %>% data.frame()
  #X.dropna(axis=0,how='all')
  #reset index
  #rownames(pest)<-pest$Pesticide
  pest %<>%  na_if("")
  pest$Pesticide <- toupper(pest$Pesticide)
  #select(!Pesticide) %>%
  return(pest)
}

find_TF<-function(x){
  return (grepl(x,straw_df$Pesticide))
}

combine<-function(){
  pest<-read.csv('Pesticides.csv',fileEncoding = "UTF-8-BOM")
  df1<-read.csv('Strawberries.csv',fileEncoding = "ANSI_X3.4-1986")
  straw_df<-straw_clean(df1)
  pest_df<-pest_clean(pest)
  straw_df %<>% separate(col=details,into = c('Pesticide','Number'),sep=' =',fill='right') 
  #straw_df$Pesticide %>% is.na() %>% sum()
  TF_re<-sapply(pest_df$Pesticide,find_TF) 
  straw_df %<>% mutate(Pesticide_old=Pesticide,Pesticide=NA)
  for (i in 1:length(pest_df$Pesticide)){
    #print(i)
    tmpl<-straw_df %>% `[`(TF_re[,i],) %>% `$`(Pesticide) %>% length()
    if ( tmpl >0 ){
      straw_df [TF_re[,i],] <- straw_df %>% `[`(TF_re[,i],) %>% mutate(Pesticide=data.frame(TF_re) %>% colnames() %>% `[`(i) ) 
    }
  }
  result<-left_join(straw_df,pest_df,by='Pesticide')
  #(apply(TF_re,1,sum)==1) %>% sum()
  result$measurement=NA
  for (li in result %>% select(items,units,discription)){
    tmp1<-grepl('MEASURED',li)
    result$measurement[tmp1]=li[tmp1]
  }
  result$measurement = str_trim(result$measurement)
  result %>% relocate(measurement,.after=units)
  return(result)
}








#source('test1025.R')
#combine() %>% view()











