library(tidyverse)
library(janitor)
library(readxl)
library(ggplot2)


readRDS("2023-03-31-IACS plate 1.rds")->iacs1
readRDS("2023-03-31-iacs plate 2.rds")->iacs2

readRDS("2023-03-31-Ven plate 1.rds")->ven1
readRDS("2023-03-31-ven plate 2.rds")->ven2






iacs1%>%
  mutate(plate = 1)%>%
  full_join(iacs2%>%mutate(plate=2))%>%
  rename(iacs_conc = concentration)->iacs3




ven1%>%
  mutate(plate = 1)%>%
 full_join(ven2%>%mutate(plate=2))%>%
  rename(ven_conc = concentration)->ven3


full_join(iacs3, ven3, by = c("plate", "xrow", "xcol", "luminescence"))%>%
  relocate(plate, xrow, xcol, iacs_conc, ven_conc)%>%
  filter(!is.na(luminescence))->iacs_ven


# ggplot(iacs_ven%>%filter(plate==1), aes(x = iacs_conc, y = luminescence))+
#   geom_point()+
#   facet_wrap(~ ven_conc)
# 
# 
# ggplot(iacs_ven, aes(x = ven_conc, y = luminescence))+
#   geom_point()+
#   facet_wrap(~ iacs_conc)


iacs_ven%>%
  arrange(iacs_conc, ven_conc)%>%
  view()

iacs_ven%>%
  arrange(ven_conc, iacs_conc)%>%
  view()


iacs_ven%>%
  arrange(ven_conc, iacs_conc)%>%
  write_csv("iacs_ven.csv")

