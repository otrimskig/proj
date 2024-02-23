#R input using Data to R sheet
#necessary libraries
library(tidyverse)
library(googlesheets4)


##################################
#sheet id and sheet name to read in from sheets. 

sheet_id<-"1bdya6WH1KA-w3ziKprTpbxd-S1zJh1zPef754b8eDKY"
name_of_sheet<-"lab_samples"



#################################



############
#authorize user.
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
sheets_df<-read_sheet(sheet_id, 
                    sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()




#save to RDS file for later use,
#if need to recreate at later time. 
#######title of .rds file output (if using)

title<-"nf1 cataloged samples"

#name of subdirectory of current project, for location of rds file (if desired). 
#include trailing /. 
file_loc<-"ds/"




sheets_df%>%
  saveRDS(file = paste0(file_loc, as.character(round(Sys.time(), "min")), "-", title, ".rds")%>%
            gsub(":","-",.)%>%
            sub("-00-", " ", .))





############################################################
#output code for export of any datasets. 


# library(googlesheets4)
# gs4_auth(email = "gotrimski@gmail.com")
# 
# df%>%
#   range_write("1DYwkZEVK39qMZl1vYrg3R7hi41nye1yikUNYN4tzWbs",
#               sheet = "output",
#               .,
#                reformat=FALSE,
#               range = "A1")