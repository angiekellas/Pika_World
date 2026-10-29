#### Packages ####

library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(stringr)
library(foreach)
#### Process ARU recognizer output ####
ARU_CSV <- list.files(file.path("data","raw_aru"),full.names = T)

ARU_summary <- foreach(file = ARU_CSV,.combine = rbind)%do%{
  
  path <- file
  one_site <- fread(path)
  ### the site name must be the first word of the csv file, followd by _
  site_name <- str_extract(path,"raw_aru/[:alpha:]+")
  site_name <- str_remove(site_name,"raw_aru/")
  ### count the number of meeps per recording 
  one_site_summarized <- one_site[,.(meep = sum(PIKA >10)),by= file]
  one_site_summarized[,site := site_name]
  one_site_summarized[,ARU_ID := paste0(site_name,"_",str_extract(file,"PIKARU[:digit:]+"))]
  one_site_summarized[,date := str_extract_all(file,"[:digit:]{8}_[:digit:]{6}.wav")]
  one_site_summarized[,date := str_remove_all(date,".wav")]
  one_site_summarized[,date := ymd_hms(date)]
  one_site_summarized[,year := year(date)]
  one_site_summarized[,month := month(date)]
  one_site_summarized[,day := day(date)]
  one_site_summarized[,doy := yday(date)]
  one_site_summarized[,hour := hour(date)]
  one_site_summarized[,minute := minute(date)]
  one_site_summarized <- one_site_summarized[!is.na(ARU_ID),]
  one_site_summarized <- one_site_summarized[month%in% 6:8]
  one_site_summarized <- one_site_summarized[year>= 2025]
  one_site_summarized
  
}

ARU_summary[,date_simple := floor_date(date,"hour") ]
ARU_hour <- ARU_summary[,.(meep_hour = sum(meep)*(6/.N),n_recording = .N ),by= .(site,ARU_ID,year , month , day, doy,hour,date_simple)]

write.table(ARU_summary,file.path("data","processed_aru","ARU_10min.csv"),row.names = F)
write.table(ARU_hour,file.path("data","processed_aru","ARU_hour.csv"),row.names = F)

#### processing one file for debugging ####
path <- ARU_CSV[2]
one_site <- fread(path)

site_name <- str_extract(path,"raw_aru/[:alpha:]+")
site_name <- str_remove(site_name,"raw_aru/")
one_site_summarized <- one_site[,.(meep = sum(PIKA >10)),by= file]
one_site_summarized[,site := site_name]
one_site_summarized[,ARU_ID := paste0(site_name,"_",str_extract(file,"PIKARU[:digit:]+"))]
one_site_summarized[,date := str_extract_all(file,"[:digit:]{8}_[:digit:]{6}.wav")]
one_site_summarized[,date := str_remove_all(date,".wav")]
one_site_summarized[,date := ymd_hms(date)]
one_site_summarized[,year := year(date)]
one_site_summarized[,month := month(date)]
one_site_summarized[,day := day(date)]
one_site_summarized[,doy := yday(date)]
one_site_summarized[,hour := hour(date)]
one_site_summarized[,minute := minute(date)]
one_site_summarized <- one_site_summarized[!is.na(ARU_ID),]
one_site_summarized <- one_site_summarized[month%in% 6:8]
one_site_summarized <- one_site_summarized[year>= 2025]


ggplot(ARU_hour,aes( x= date_simple, y = meep_hour, color = ARU_ID))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  facet_wrap(~ site,scales = "free_x")


#### ####