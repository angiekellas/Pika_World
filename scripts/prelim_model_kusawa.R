library(data.table)
library(brms)
library(ggplot2)


Pika_climate_camera <- fread("data/combined_temp_meeps.csv")

Pika_climate_camera

# remove one climate NA ?
Pika_climate_camera <- Pika_climate_camera[!is.na(Pika_climate_camera$CLIMATE),]


ggplot(Pika_climate_camera,aes(x = CLIMATE, y= MEEPS, color = ARU_ID))+
  geom_point()+
  geom_smooth()+
  theme_classic()

ggplot(Pika_climate_camera,aes(x = CLIMATE, y= MEEPS))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  facet_wrap(~ DATE)

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
formula_climate_one_site <- bf(MEEPS ~ 1 + poly(CLIMATE,2)+(1 + poly(CLIMATE,2)|ARU_ID)+(1|TIME)+(1 + poly(CLIMATE,2)|DATE),
                               shape ~ 1 + poly(CLIMATE,2)+(1|ARU_ID)+(1|TIME)) # + (1| SITE)

poly(Pika_climate_camera$CLIMATE,2)


plot(Pika_climate_camera$CLIMATE^1,poly(Pika_climate_camera$CLIMATE,2)[,1])
my_prior <-  set_prior("normal(3.5,0.75)",coef = "Intercept", class = "b")



model_temp_one_site <- brm(formula_climate_one_site,
                           data =Pika_climate_camera ,
                           family = negbinomial(),
                           iter = 700,
                           warmup = 250,
                           cores = 3,
                           chains = 3 ,
                           threads = threading(4),
                           file = "model/kusawa_temp_pika", ##delete the modle if you want to REFIT it
                          # prior = my_prior ,
                           backend= "cmdstanr")

plot(model_temp_one_site)
pp_check(model_temp_one_site)
conditional_effects(model_temp_one_site,"CLIMATE")

## marginal effect
marge_new_data <-  Pika_climate_camera[,.( CLIMATE = seq(from = min(CLIMATE) , to =max(CLIMATE) , length.out = 40)),by = SITE]

marginal_pred <- cbind(marge_new_data,fitted(model_temp_one_site,newdata = marge_new_data, re_formula = NA))


ggplot(Pika_climate_camera,aes(x = CLIMATE, y= MEEPS))+
  geom_point()+
  theme_classic()+
  geom_line(data = marginal_pred, aes (y = Estimate , ymin = Q2.5, ymax = Q97.5 ),color = "red")+
  geom_ribbon(data = marginal_pred, aes (y = Estimate , ymin = Q2.5, ymax = Q97.5 ),fill ="red",alpha = 0.25)


## conditional effect
cond_new_data <-  Pika_climate_camera[,.( CLIMATE = seq(from = min(CLIMATE) , to =max(CLIMATE) , length.out = 20)),by = .(SITE,DATE)]
cond_pred <- cbind(cond_new_data,fitted(model_temp_one_site,newdata = cond_new_data, re_formula = ~(1 + poly(CLIMATE,2)|DATE)))



ggplot(Pika_climate_camera,aes(x = CLIMATE, y= MEEPS))+
  geom_point()+
  theme_classic()+
  facet_wrap(~DATE)+
  geom_line(data = cond_pred, aes (y = Estimate , ymin = Q2.5, ymax = Q97.5 ),color = "red")+
  geom_ribbon(data = cond_pred, aes (y = Estimate , ymin = Q2.5, ymax = Q97.5 ),fill ="red",alpha = 0.25)

