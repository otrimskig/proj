#R input using Data to R sheet
library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)

#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
sheets_df<-read_sheet("1DYwkZEVK39qMZl1vYrg3R7hi41nye1yikUNYN4tzWbs", 
                    sheet = "input")

#save to RDS file for later use,
#if need to recreate at later time. 

sheets_df%>%
  saveRDS(file = paste0(as.character(now())%>%
                          gsub(":", "-", .)%>%
                          gsub(" ", "_", .)%>%
                          gsub('.{3}$', '', .),
                        
                        "-", "drive_import.rds"))
#############################################################

#sheets_df%>%
#
#
# view()






############################################################
#output code for export of any datasets. 


# library(googlesheets4)
# gs4_auth(email = "gotrimski@gmail.com")
# 
# df%>%
#   range_write("1DYwkZEVK39qMZl1vYrg3R7hi41nye1yikUNYN4tzWbs",
#               sheet = "output",
#               .,
#               range = "A1")