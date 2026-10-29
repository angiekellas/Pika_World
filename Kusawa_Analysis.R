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

#group regardless of camera 
haypiles_per_hour_all_cams<- hourly_haypiles_kusawa %>%
  group_by(hour) %>%
  summarise(
    haypile_visits = sum(haypile_visits),
    .groups = "drop"
  )


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

#clip so that data only contains meeps with confidence over 10 as per charlottes thesis protocol
kusawa_meeps_clipped <- kusawa_meeps_data %>%
  filter(PIKA > 10)


# Make sure the dataset is a data.table
setDT(kusawa_meeps_clipped)

# Extract the recording start time from the filename
kusawa_meeps_clipped[, recording_start := as.POSIXct(
  sub(".*(\\d{8}_\\d{6})\\.wav$", "\\1", file),
  format = "%Y%m%d_%H%M%S",
  tz = "UTC"
)]

# Calculate the actual time of each meep
kusawa_meeps_clipped[, meep_time := recording_start + start_time]

# Group detections by hour and count the meeps
meeps_per_hour <- kusawa_meeps_clipped[, .(
  meep_count = .N
), by = .(
  hour = as.POSIXct(
    format(meep_time, "%Y-%m-%d %H:00:00", tz = "UTC"),
    tz = "UTC"
  )
)]

# check it out
meeps_per_hour[order(hour)]

#join the two datasets to start comparing 

meeps_haypiles <- inner_join(
  meeps_per_hour,
  haypiles_per_hour_all_cams,
  by = "hour"
)

head(meeps_haypiles)

#plot it
library(ggplot2)

ggplot(meeps_haypiles, aes(x = meep_count, y = haypile_visits)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Meep detections per hour",
    y = "Haypile visits per hour (all cameras)",
    title = "Meep detections vs. haypile visits"
  ) +
  theme_minimal()




