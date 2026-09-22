library(readr)
MtAdney_Temp_Above_17Sept2026 <- read_csv("data/MtAdney_Temp_Above_17Sept2026.csv")
View(MtAdney_Temp_Above_17Sept2026)


#make camera data haypile visits per hour and filter out only "Animal" cause those are pikas

library(dplyr)
library(lubridate)

hourly_haypiles <- MtAdney_Camera_17Sept2026 %>%
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


#convert temp data time to match haypile visits

MtAdney_Temperature <- MtAdney_Temp_Above_17Sept2026 %>%
  mutate(
    datetime = mdy_hms(`Date-Time (MST)`),
    hour = floor_date(datetime, unit = "hour")
  )

#calculate average hourly temp
hourly_temperature <- MtAdney_Temperature %>%
  group_by(hour) %>%
  summarise(
    temperature = mean(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

#combine hourly temp and haypile visits
hourly_data <- hourly_temperature %>%
  left_join(hourly_haypiles, by = "hour")

#cut out data on July 27 and make NA's 0 for when there was no haypile visits but the cameras were recording
library(tidyr)

hourly_data <- hourly_data %>%
  filter(as.Date(hour) != as.Date("2026-07-27")) %>%
  mutate(
    haypile_visits = replace_na(haypile_visits, 0)
  )

table(as.Date(hourly_data$hour))
sum(is.na(hourly_data$haypile_visits))

#cutout after Aug 18th cause amera was no longer deployed

hourly_data <- hourly_data %>%
  filter(hour <= ymd_hms("2026-08-18 18:00:00"))

#Plot it

library(ggplot2)

ggplot(hourly_data, aes(x = temperature, y = haypile_visits)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Hourly temperature (°C)",
    y = "Haypile visits per hour",
    title = "Relationship between temperature and haypile visits"
  ) +
  theme_classic()

#beysian stuff
install.packages("brms")
library(brms)

# Number of hourly observations
nrow(hourly_data)

# Number of hours with zero haypile visits
sum(hourly_data$haypile_visits == 0)

# Proportion of hours with zero haypile visits
mean(hourly_data$haypile_visits == 0)

#Check the mean and variance of haypile visits
# ============================================================

mean(hourly_data$haypile_visits)

var(hourly_data$haypile_visits)

#they are close so try the poission 
pika_poisson <- brm(
  haypile_visits ~ temperature + I(temperature^2),
  data = hourly_data,
  family = poisson(),
  chains = 4,
  cores = 4,
  iter = 4000
)

summary(pika_poisson)

#trouble shotting
R.version.string
packageVersion("brms")
packageVersion("StanHeaders")
pkgbuild::has_build_tools(debug = TRUE)
library(cmdstanr)
install.packages(
  "cmdstanr",
  repos = c("https://stan-dev.r-universe.dev", "https://cloud.r-project.org")
)
library(cmdstanr)
cmdstanr::cmdstan_version()
cmdstanr::install_cmdstan()
cmdstanr::cmdstan_path()
cmdstanr::cmdstan_version()

#run model again
pika_poisson <- brm(
  haypile_visits ~ temperature + I(temperature^2),
  data = hourly_data,
  family = poisson(),
  chains = 4,
  cores = 4,
  iter = 4000,
  backend = "cmdstanr"
)
summary(pika_poisson)
#plot
conditional_effects(
  pika_poisson,
  effects = "temperature",
  re_formula = NA
)
