#process camera data

#read csv file in and check column names

kusawa_detections <- fread("data/kusawa_detections.csv")

#filter camera detections as "animal" is a pika but classificaiton label has to be blank
#and convert date time

kusawa_pikas <- kusawa_detections %>%
  filter(
    detection_category == "animal",
    is.na(classification_label) |
      trimws(classification_label) == ""
  ) %>%
  mutate(
    event_start = ymd_hms(event_start, tz = "UTC") %>%
      with_tz("Etc/GMT+7"),
    event_end = ymd_hms(event_end, tz = "UTC") %>%
      with_tz("Etc/GMT+7")
  ) %>%
  distinct(deployment_id, event_id, .keep_all = TRUE) #since multiple photos from the same pika visit share an event_id, we want to count each event only once.

#create ARU recording windows to match detections to when the ARU was recording

ARU_windows <- ARU_summary %>%
  mutate(
    recording_start = force_tz(date, "Etc/GMT+7"),
    recording_end = recording_start + minutes(10)
  ) %>%
  distinct(site, recording_start, recording_end)


#match pika events to ARU recording windows #

kusawa_matched <- kusawa_pikas %>%
  inner_join(
    ARU_windows,
    by = join_by(
      event_start < recording_end,
      event_end >= recording_start
    )
  ) %>%
  distinct(deployment_id, event_id, .keep_all = TRUE)

#count pika visits per hour 

kusawa_camera_hour <- kusawa_matched %>%
  mutate(
    SITE = site,
    DATE = as_date(recording_start),
    TIME = hour(recording_start)
  ) %>%
  group_by(SITE, DATE, TIME) %>%
  summarise(
    PIKA_VISITS = n(),
    .groups = "drop"
  )

#identify hours with ARU recordings

ARU_recording_hours <- ARU_windows %>%
  mutate(
    SITE = site,
    DATE = as_date(recording_start),
    TIME = hour(recording_start)
  ) %>%
  distinct(SITE, DATE, TIME)

#add zeros for hours without pika visits 

kusawa_camera_hour <- ARU_recording_hours %>%
  left_join(
    kusawa_camera_hour,
    by = c("SITE", "DATE", "TIME")
  ) %>%
  mutate(
    PIKA_VISITS = replace_na(PIKA_VISITS, 0)
  )

#save
write.csv(
  kusawa_camera_hour,
  file.path("data", "processed_camera", "kusawa_camera_hour.csv"),
  row.names = FALSE
)


