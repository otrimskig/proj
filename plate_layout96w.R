library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)

#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
w96_df<-read_sheet("12NUnpmY9GzYT31EfhmvQFXkvBaSehO7420PTCk_eyTs", 
                      sheet = "input")


#name of plate
plate_name<-(w96_df%>%pull(2))[1]

#name of layout descriptor
labels<-(w96_df%>%pull(2))[3]

#name of measured value. 
measured<-(w96_df%>%pull(2))[15]



#plate layout
w96_df%>%
  select(2:last_col())%>%
  slice(-c(1:3))%>%
  row_to_names(1)%>%
  clean_names()->w96
 
 
#get w96 label layout. 
w96%>%
  slice(1:8)%>%
  pivot_longer(2:last_col(), "xcol", 
               values_to = labels)->w96.layout


#get w96 measured values. 
w96%>%
  slice(13:20)%>%
  pivot_longer(2:last_col(), "xcol", 
               values_to = measured)->w96.measured


#join by plate location. 
left_join(w96.layout, w96.measured)->w96_all



#save to RDS file for later use,
w96_all%>%
  saveRDS(file = paste0(as.character(today()),"-", plate_name, ".", "rds"))



#moves to google sheet for convenience. 
w96_all%>%
  
  #arrange by column 3 and 4. 
  arrange(.[3], .[4])%>%
  range_write("12NUnpmY9GzYT31EfhmvQFXkvBaSehO7420PTCk_eyTs",
                          sheet = "output",
                          .,
                          range = "A1")



#creating summary stats df. 
w96_all%>%
  group_by_at(3)%>%
  summarise_at(3, .funs = funs(mean, sd))%>%
  rename_with(~paste0(measured, "_", "mean"), .cols = "mean")%>%
  rename_with(~paste0(measured, "_", "sd"), .cols = "sd")->w96_summary




#writing summary stats to sheets. 
w96_summary%>%
  range_write("12NUnpmY9GzYT31EfhmvQFXkvBaSehO7420PTCk_eyTs",
              sheet = "summary_stats",
              .,
              range = "A1")




