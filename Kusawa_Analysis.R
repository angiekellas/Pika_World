#install packages
install.packages("data.table")


#load packages

library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)

#bring in camera data
kusawa_camera_detections <- read_csv("data/kusawa_detections.csv")
View(kusawa_camera_detections)

#change camera id to the camera names

kusawa_camera_detections <- kusawa_camera_detections %>%
  mutate(deployment_id = case_when(
    deployment_id == "1dbd4208-c47b-4e24-9276-cafe5b7bd505" ~ "Kus1",
    deployment_id == "da3c3a42-4c4f-4977-aad0-31b0319d16bc" ~ "Kus2",
    deployment_id == "08b8105e-8cdd-4748-b2e9-0466c87634d7" ~ "Kus3",
    TRUE ~ as.character(deployment_id)
  ))

#make camera data haypile visits per hour and filter out only "Animal" cause those are pikas


hourly_haypiles_kusawa <- kusawa_camera_detections %>%
  filter(common_name == "Animal") %>%
  mutate(
    event_start = ymd_hms(event_start),
    hour = floor_date(event_start, unit = "hour")
  ) %>%
  group_by(deployment_id, hour) %>%
  summarise(
    haypile_visits = n(),
    .groups = "drop"
  )

#save as an rds
saveRDS(hourly_haypiles_kusawa, "hourly_haypiles_kusawa.rds")

# load in huge aru output file 
kusawa_meeps_csv <- file.choose()

# import the CSV
 kusawa_meeps_data<- fread(kusawa_meeps_csv)

# create a filename for the RDS file in the same folder
kusawa_rds_file <- sub("\\.csv$", ".rds", kusawa_meeps_csv, ignore.case = TRUE)

# save as an RDS file
saveRDS(kusawa_meeps_data, kusawa_rds_file)

# check where the RDS file was saved
print(paste("RDS file saved to:", kusawa_rds_file))

# check the dataset 
print(dim(kusawa_meeps_data))

