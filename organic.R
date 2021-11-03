library(tidyverse)
library(magrittr)

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
    theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
    ggtitle(paste('Organic proportion of each state in', year)) +
    geom_text(aes(x=State, y=Ratio, label=Ratio)) + ylim(0, 1)
}

# Plot the organic price value or CWT value in a given year and type(price unit is 1000$)
plot_v <- function(year, type){
  ovalue <- data.frame('States'=states, 'Value'=rep(0, length(states)))
  for(i in 1:total) {
    ovalue[i, 2] <- organicV(ovalue[i, 1], year, type)
  }
  graph <- ggplot(data=ovalue) + geom_bar(mapping = aes(x=State, y=Value), stat = 'identity', fill = 'orange') + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0)) + geom_text(aes(x=State, y=Value, label=Value))
  if (type == 'price') {
    graph + ggtitle(paste('Organic price value of each state (with unit 1000) in', year))
  } else {
    graph + ggtitle(paste('Organic CWT value of each state in', year))
  }
}


# Test
plot_p(2016)
plot_p(2019)
plot_v(2016, 'price')
plot_v(2016, 'CWT')
plot_v(2019, 'price')
plot_v(2019, 'CWT')
plot_v(2016, 'Caaaa')
organicV('WASHINGTON', 2019)