library(paletteer)
library(patchwork)
library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(plotly)
library(zoo)

backg<-theme(
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA), 
  legend.background = element_rect(fill = "transparent",colour=NA),
  legend.box.background = element_rect(fill='transparent',colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA)
)

clean_NIPA <- function(dh, years,colname,use_deflator = F){
  names(dh)[1] <- 'row'
  names(dh)[2] <- 'industry'
  # Convert these years to character just once
  year_cols <- as.character(years)
  
  #NIPA industries:
  key <- fread(file.path('data','NIPA','GDP-by-industry','NIPA industries_gross output.csv'))
  key[, industry := trimws(industry)]
  
  #given NIPA data:
  dh[, industry := trimws(industry)]
  #remove all digits:
  dh[, industry := trimws(gsub("\\d", "", industry))]
  
  #Add sector key
  dh <- merge(dh, key, by = 'industry')
  dh <- dh[, c('row', 'id', 'level', 'parent', 'industry', all_of(year_cols)), with = FALSE]  # <- key change
  setorder(dh, id)
  
  #Remove addenda
  dh <- dh[level >= 0]
  
  dh[, (year_cols) := lapply(.SD, as.numeric), .SDcols = year_cols]
  
  #Long format
  dh <- pivot_longer(dh, cols = year_cols, names_to = 'year', values_to = colname) %>% setDT()
  setorder(dh, year, row)
  dh[, c(colname, 'year') := .(as.numeric(get(colname)), as.numeric(year))]
  
  #Use price deflator
  if(use_deflator == T){
   pce <- fread(file.path('data','FRED','PCECTPI.csv')) 
   pce[,year := year(observation_date)]
   pce <- pce[, .(PCECTPI_an = mean(PCECTPI)),by=.(year)]
   dh[,pi := pce[.SD,on=.(year),x.PCECTPI_an]]
   dh[, c(colname) := .(get(colname) / (pi / 100))]
   dh[,pi := NULL]
  }
  
  dh
}


#Function for generic NIPA data. Given y1 and y2 year ranges (y1 <= y2)
join_NIPA <- function(dh,d,y1,y2,colname){
  dh <- clean_NIPA(dh,y1,colname)
  d <- clean_NIPA(d,y2,colname)
  
  #Avoid duplicates: delete from the older dataset any years included in the newer one.
  dh <- dh[year %in% y1[!y1 %in% y2]]
  
  #Historical and modern data:
  d <- rbind(dh,d)
  d[,row:=NULL]
  
  
  #Non-missing data:
  d <- d[!is.na(get(colname))]
  
  # Sector children
  d <- make_children(d)
  
  setorder(d,year,id) 
  
  d
}