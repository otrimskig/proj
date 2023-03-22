library(tidyverse)
library(janitor)
library(readxl)
library(ggplot2)



#read_excel("20230317-met.xlsx")->met

readRDS("2023-03-22_12-59-drive_import.rds")->met




met%>%
  select(2:last_col())%>%
  select(-tail(names(.), 4))%>%
  rename(xrow = 1)%>%
  clean_names()->met2


met2%>%
  slice(1:8)%>%
  pivot_longer(2:last_col(), "xcol", values_to = "conc")->met_conc

met2%>%
  slice(-c(1:8))%>%
  slice(-c(1:2))%>%
  pivot_longer(2:last_col(), "xcol", values_to = "abs")%>%
  left_join(met_conc)->met_vals
 
met_vals%>%
  group_by(conc)%>%
  summarise(mean = mean(abs),
            sd = sd(abs))%>%
  left_join(met_vals)%>%
  mutate(conc_log = log(conc))->met_vals2




met_vals2%>%
  
  ggplot(aes(conc_log, abs))+
  geom_point(shape = 21, size = 2, stroke = 1.5, alpha = .3)+
  geom_point(aes(conc_log, mean), color = "red")+
  theme_classic()
  
