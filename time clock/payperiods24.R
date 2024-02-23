library(googlesheets4)
library(tidyverse)
library(lubridate)
library(hms)


pp.1<-read_sheet("https://docs.google.com/spreadsheets/d/1EMKP1T11kPVPc2-wmztRzEPNngxkw0rcAkpz09I3rFA/edit#gid=0", 
                   
####!!! #change sheet for year.       
                   
                   sheet = "pp24")

pp.1%>%
  select(1:2)%>%
  pivot_longer(c(1:2), names_to = "type", values_to = "date")%>%
  filter(type == "pp_start")%>%
  mutate(pp_num = row_number())->pps


#####!!! 
#change dates. 

seq(as.Date("2024/01/01"), as.Date("2024/12/31"), "days")%>%
  as_tibble()%>%
  rename(date = value)%>%
  left_join(pps)%>%
  mutate(index = row_number())%>%
  fill(pp_num)->pp_year



pp.1%>%
  select(3,4)->hols

pp_year
hols
pp_year%>%
  left_join(hols)%>%
  mutate(wkday = wday(date, label = TRUE))%>%
  mutate(workday = ifelse(is.na(holiday)&wkday!="Sun"&wkday!="Sat", "1", "0"))->pp_year2



pp_year2%>%
  group_by(pp_num)%>%
  filter(workday == "1")%>%
  count(workday)%>%
  
  
  select(-2)%>%
  rename(workdays_in_pp = n)%>%
  mutate(exp_hours = workdays_in_pp*8)->pp_workdays


pp_year%>%
  filter(type == "pp_start")%>%
  select(-index)%>%
  left_join(pp_workdays)%>%
  select(-type)%>%
  rename(pp_start = date)%>%
  select(1:3)->pp_workdays2




###!!!
###change year



write_rds(pp_year2, "pp_year24.rds")
