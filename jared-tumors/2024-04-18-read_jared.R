library(tidyverse)
library(janitor)
library(readxl)

df_4718<-read_xlsx("ds/VS RCAS All Allele Data.xlsx", sheet = "VS-4718")%>%
  clean_names()

df_6766<-read_xlsx("ds/VS RCAS All Allele Data.xlsx", sheet = "VS-6766")%>%
  clean_names()

df_combo<-read_xlsx("ds/VS RCAS All Allele Data.xlsx", sheet = "Combination")%>%
  clean_names()



df2<-df_4718%>%
  filter_all(any_vars(!is.na(.)))%>%
  select(-cage)%>%
  select(c(mouse:date_of_tumor_measurement), tumor_number, volume_mm3)%>%
  
  
  group_by(mouse, date_of_tumor_measurement)%>%
  summarise(treatment,
            treatment_start_date, 
            total_tumor_vol = sum(volume_mm3))%>%
  
  
  filter(!is.na(total_tumor_vol))


saveRDS(df2, "clds/4718.rds")

