library(data.table)
library(brms)
library(ggplot2)

Pika_climate_camera

# remove one climate NA ?
Pika_climate_camera <- Pika_climate_camera[!is.na(Pika_climate_camera$CLIMATE),]


ggplot(Pika_climate_camera,aes(x = CLIMATE, y= MEEPS, color = ARU_ID))+
  geom_point()+
  geom_smooth()+
  theme_classic()

ggplot(Pika_climate_camera,aes(x = CLIMATE, y= MEEPS, color = ARU_ID))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  facet_wrap(~ ARU_ID)

ggplot(Pika_climate_camera,aes(x = TIME, y= CLIMATE, color = ARU_ID))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  facet_wrap(~ ARU_ID)

ggplot(Pika_climate_camera,aes(x = CLIMATE, y= MEEPS, color = ARU_ID))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  facet_wrap(~ DATE)

ggplot(Pika_climate_camera,aes(x = CLIMATE, y= MEEPS, color = ARU_ID))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  facet_wrap(~ TIME)


# 1 is the intercept aka Beta 0
formula_climate_one_site <- bf(MEEPS ~ 1 + CLIMATE + I(CLIMATE^2) +(1| ARU_ID)+(1| TIME)+(1|DATE),
                               shape ~ 1 +CLIMATE + I(CLIMATE^2)+(1| ARU_ID)+(1| TIME)) # + (1| SITE)

my_prior <-  set_prior("normal(3.5,0.75)",coef = "Intercept", class = "b")



model_temp_one_site <- brm(formula_climate_one_site,
                           data =Pika_climate_camera ,
                           family = negbinomial(),
                           iter = 700,
                           warmup = 250,
                           cores = 3,
                           chains = 3 ,
                           prior = my_prior ,
                           backend= "cmdstanr")

plot(model_temp_one_site)
pp_check(model_temp_one_site)
conditional_effects(model_temp_one_site,"CLIMATE")

