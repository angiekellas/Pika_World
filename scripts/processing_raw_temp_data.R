

#process climate data #

CLIMATE_CSV <- list.files(
  file.path("data", "raw_temperture"),
  pattern = "\\.csv$",
  full.names = TRUE
)

Climate_summary <- foreach(file = CLIMATE_CSV, .combine = rbind) %do% {
  
  path <- file
  one_site_climate <- fread(path)
  
  ### Assign site name
  site_name <- "Kusawa"
  
  # convert date time 
  one_site_climate[, date := mdy_hms(
    `Date-Time (MST)`
  )]
  
  # extract date and time variables
  one_site_climate[, year := year(date)]
  one_site_climate[, month := month(date)]
  one_site_climate[, day := day(date)]
  one_site_climate[, doy := yday(date)]
  one_site_climate[, hour := hour(date)]
  
  # keep summer months and years consistent to way ARU data is processed 
  one_site_climate <- one_site_climate[!is.na(date), ]
  one_site_climate <- one_site_climate[month %in% 6:8]
  one_site_climate <- one_site_climate[year >= 2025]
  
  # add site name column
  one_site_climate[, site := site_name]
  
  #summarize hourly climate data
  one_site_climate[, date_simple := floor_date(date, "hour")]
  
  climate_hourly <- one_site_climate[, .(
    temperature = mean(`Temperature , °C`, na.rm = TRUE),
    RH = mean(`RH , %`, na.rm = TRUE),
    dew_point = mean(`Dew Point , °C`, na.rm = TRUE),
    n_climate = .N
  ), by = .(site, year, month, day, doy, hour, date_simple)]
  
  climate_hourly
}

#save

write.table(
  Climate_summary,
  file.path("data", "processed_temperature", "Kusawa_Climate_hour.csv"),
  row.names = FALSE,
  sep = ","
)
