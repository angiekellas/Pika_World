# Combine ARU and climate data ####

Kusawa_Climate_hour <- fread(
  file.path("data", "processed_temperature", "Kusawa_Climate_hour.csv")
)

Pika_climate <- ARU_hour %>%
  left_join(
    Kusawa_Climate_hour,
    by = c("site", "year", "month", "day", "doy", "hour")
  ) %>%
  mutate(
    DATE = as_date(date_simple.x),
    TIME = hour(date_simple.x)
  ) %>%
  select(
    SITE = site,
    ARU_ID,
    DATE,
    TIME,
    MEEPS = meep_hour,
    CLIMATE = temperature,
    RH,
    DEW_POINT = dew_point
  )

#combine camera data with aru and climate


kusawa_camera_hour <- fread(
  file.path("data", "processed_camera", "kusawa_camera_hour.csv")
) %>%
  mutate(DATE = as.Date(DATE))

Pika_climate_camera <- Pika_climate %>%
  left_join(
    kusawa_camera_hour,
    by = c("SITE", "DATE", "TIME")
  )

#save

write.csv(
  Pika_climate_camera,
  file.path(
    "data",
    "combined_temp_meeps",
    "Pika_climate_camera.csv"
  ),
  row.names = FALSE
)

meeps_temp_camera_data <- read.csv("data/combined_temp_meeps/Pika_climate_camera.csv")
hea