library(googlesheets4)
library(tidyverse)
library(lubridate)
library(hms)

gs4_auth(email = "gotrimski@gmail.com")

otc.new<-read_sheet("https://docs.google.com/spreadsheets/d/1EMKP1T11kPVPc2-wmztRzEPNngxkw0rcAkpz09I3rFA/edit#gid=1911378700", 
                    sheet = "current month")%>%
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
  mutate(duration_mins = as.numeric(out_time - in_time))%>%
  select(-duration_mins)->otc_new1





#######################
#year before this month

otc_year<-read_sheet("https://docs.google.com/spreadsheets/d/1EMKP1T11kPVPc2-wmztRzEPNngxkw0rcAkpz09I3rFA/edit#gid=1911378700", 
                    sheet = "2023")


otc_year%>%
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
  mutate(duration_mins = as.numeric(out_time - in_time))%>%
  select(-duration_mins)->otc_year2




#original, unaltered pay periods: 
read_rds("pp_year23.rds")->pp_year





otc_year2%>%
  full_join(otc_new1)%>%
  distinct(in_time, out_time)->otc_year3








#######################
#actual updates

#all time punches
otc_year3%>%
  mutate(date = date(in_time))%>%
  left_join(pp_year)%>%
  relocate(pp_num, date, type)%>%
  select(1:5)%>%
  arrange(in_time)->otc_year4




range_write(otc_year4,
            ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423",
            sheet = "2023",
            range = "A3",
            col_names = FALSE,
            reformat = FALSE)






###################
#daily hours vs. expected

otc_year4%>%
  mutate(hrs_worked = out_time - in_time)%>%
  group_by(date)%>%
  
  summarise(hrs_worked = as.numeric(sum(hrs_worked)))%>%
  
  full_join(pp_year)%>%
  arrange(date)%>%
  
  mutate(hrs_worked = ifelse(is.na(hrs_worked), 0, hrs_worked))%>%
  mutate(expected = ifelse(workday == 1, 8*60/1440, 0))%>%
  
  mutate(hrs_worked = hrs_worked/1440)%>%
  relocate(hrs_worked, .after = "workday")%>%
  filter(date <= as.Date(today()-1))->otc_update_day
  
  
  
  




range_write(otc_update_day,
            ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423",
            sheet = "day23",
            range = "A3",
            col_names = FALSE,
            reformat = FALSE)
