library(googlesheets4)
library(tidyverse)
library(lubridate)
library(hms)

otc23.1<-read_sheet("https://docs.google.com/spreadsheets/d/1EMKP1T11kPVPc2-wmztRzEPNngxkw0rcAkpz09I3rFA/edit#gid=1911378700", sheet = "2023")


otc23.1%>%
  rename(col1 = 1)%>%
  
  
  mutate(date = str_split(.$col1, "\\s", simplify=TRUE)[,1])%>%
  
  
  mutate(in_time = paste(str_split(.$col1, "\\s", simplify=TRUE)[,3], 
                      str_split(.$col1, "\\s", simplify=TRUE)[,4]))%>%
  
  mutate(out_time = trimws(paste(str_split(.$col1, "\\s", simplify=TRUE)[,5], 
                      str_split(.$col1, "\\s", simplify=TRUE)[,6])))%>%
  
  mutate(duration = paste0(str_split(.$col1, "\\s", simplify=TRUE)[,7],
                       str_split(.$col1, "\\s", simplify=TRUE)[,8]))%>%
  
 
  
  mutate(index = row_number())%>%
  select(-col1)%>%
  filter(out_time != "missing")%>%
  
  
  
  mutate(date = gsub(",", "/23", date))%>%
  mutate(date = mdy(date))%>%
  mutate(duration = gsub("m", "M", duration))%>%
  mutate(duration = as.period(duration))%>%
  
  mutate(day = wday(date, label = TRUE))%>%
  
  
  mutate(duration = as.numeric(make_difftime(as.duration(duration))))%>%
  

  mutate(in_time2 = strptime(in_time, "%I:%M %p"))%>%
  mutate(in_full = make_datetime(year = as.numeric(year(date)), 
                                 month = as.numeric(month(date)),
                                 day = as.numeric(day(date)),
                                 hour = as.numeric(hour(in_time2)),
                                 min = as.numeric(minute(in_time2))))%>%
  
  
  
  mutate(out_time2 = strptime(out_time, "%I:%M %p"))%>%
  
  mutate(out_full = make_datetime(year = as.numeric(year(date)), 
                                 month = as.numeric(month(date)),
                                 day = as.numeric(day(date)),
                                 hour = as.numeric(hour(out_time2)),
                                 min = as.numeric(minute(out_time2))))%>%
  
  
  select(-c(1:7))%>%
  select(-2)%>%
  rename(in_time = in_full, out_time = out_full)%>%
  mutate(duration_mins = as.numeric(out_time - in_time))->otc23.2
  
 

otc23.2%>%
  select(-duration_mins)->otc23.3



###########################################################

pp23.1<-read_sheet("https://docs.google.com/spreadsheets/d/1EMKP1T11kPVPc2-wmztRzEPNngxkw0rcAkpz09I3rFA/edit#gid=0", sheet = "pp23")

pp23.1%>%
  select(1:2)%>%
  pivot_longer(c(1:2), names_to = "type", values_to = "date")%>%
  filter(type == "pp_start")%>%
  mutate(pp_num = row_number())->pps
  



seq(as.Date("2023/01/01"), as.Date("2023/12/31"), "days")%>%
  as_tibble()%>%
  rename(date = value)%>%
  left_join(pps)%>%
  mutate(index = row_number())%>%
  fill(pp_num)->pp_year
  
  

pp23.1%>%
  select(3,4)->hols
  


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



# range_write(pp_workdays2, 
#            ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423",
#            sheet = "pp23",
#            range = "A3",
#            col_names = FALSE,
#            reformat = FALSE)


# sheet_write(pp_workdays2,
#             ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423",
#             sheet = "pp23")



# range_write(pp_workdays2,
#             ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423",
#             sheet = "pp23",
#             range = "A7",
#             col_names = FALSE,
#             reformat = FALSE)







##################################################  

# sheet_write(otc23.3, 
#             ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423", 
#             sheet = "2023")




otc23.3%>%
  mutate(date = date(in_time))%>%
  left_join(pp_year)%>%
  
  relocate(index, pp_num, date, type)%>%
  select(-index)%>%
  arrange(in_time)->otc23.4


range_write(otc23.4, 
            ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423", 
            sheet = "2023",
            range = "A3",
            col_names = FALSE,
            reformat = FALSE)





#########################

otc23.4%>%
  mutate(hrs_worked = out_time - in_time)%>%
  group_by(date)%>%
  
  summarise(hrs_worked = as.numeric(sum(hrs_worked)))%>%


  
  full_join(pp_year2)%>%

  
  arrange(date)%>%
  
  
  
  
  mutate(hrs_worked = ifelse(is.na(hrs_worked), 0, hrs_worked))%>%
  mutate(expected = ifelse(workday == 1, 8*60/1440, 0))%>%
  
 
  
  
  
  filter(date <= as.Date(today()-1))%>%
  
  mutate(hrs_worked = hrs_worked/1440)->otc23.5

  
  
range_write(otc23.5, 
            ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423", 
            sheet = "day23",
            range = "A3",
            col_names = FALSE,
            reformat = FALSE)
