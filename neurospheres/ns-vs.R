library(tidyverse)
library(janitor)
library(readxl)
library(ggplot2)


readRDS("2023-04-11-vs-4718.rds")->vs4718


readRDS("2023-04-11-vs-6766.rds")->vs6766




  view()


vs6766%>%
  rename(vs6766_conc = concentration)%>%
  left_join(vs4718%>%
              rename(vs4718_conc = concentration))%>%
  relocate(luminescence, .after = last_col())%>%
  filter(!is.na(vs6766_conc))%>%
  arrange(vs6766_conc, vs4718_conc)->vs_df



vs_df%>%
  write_csv("vs_df.csv")

