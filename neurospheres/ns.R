library(tidyverse)
library(janitor)
library(readxl)
library(ggplot2)



readRDS("2023-03-31-Metformin Neuro.rds")->met


met%>%

  group_by(concentration)%>%
  summarise(mean = mean(luminescence),
            sd = sd(luminescence))%>%
  
  left_join(met)%>%
  mutate(conc_log = log(concentration+.01))%>%
  rename(conc = concentration)%>%
  rename(lum = luminescence)%>%
  mutate(conc_lum = log(lum+.01))%>%
  filter(!is.na(conc))->met2



#plotting
met2%>%
  ggplot(aes(conc_log, lum))+
  geom_point(shape = 21, size = .5, alpha = .3)+
  geom_point(aes(conc_log, mean), size = 1.5, color = "red")+
  theme_classic()
  
met2%>%
  ggplot(aes(conc, lum))+
  geom_point(shape = 21, size = 2, stroke = 1.5, alpha = .3)+
  geom_point(aes(y = mean), color = "red")+
  theme_classic()
