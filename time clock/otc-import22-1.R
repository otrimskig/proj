library(googlesheets4)
library(tidyverse)
library(lubridate)
library(hms)

otc22.1<-read_sheet("https://docs.google.com/spreadsheets/d/1EMKP1T11kPVPc2-wmztRzEPNngxkw0rcAkpz09I3rFA/edit#gid=0", sheet = "2022")


otc22.1%>%
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
  
  
  
  mutate(date = gsub(",", "/22", date))%>%
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
  mutate(duration_mins = as.numeric(out_time - in_time))->otc22.2
  
  

otc22.2%>%
  select(-duration_mins)->otc22.3
  



############################################################

pp22.1<-read_sheet("https://docs.google.com/spreadsheets/d/1EMKP1T11kPVPc2-wmztRzEPNngxkw0rcAkpz09I3rFA/edit#gid=0", sheet = "pp22")

pp22.1%>%
  select(1:2)%>%
  pivot_longer(c(1:2), names_to = "type", values_to = "date")%>%
  filter(type == "pp_start")%>%
  mutate(pp_num = row_number())->pps
  



seq(as.Date("2022/01/01"), as.Date("2022/12/31"), "days")%>%
  as_tibble()%>%
  rename(date = value)%>%
  left_join(pps)%>%
  mutate(index = row_number())%>%
  fill(pp_num)->pp_year
  
  

pp22.1%>%
  select(3,4)->hols
  


pp_year%>%
  left_join(hols)%>%
  filter(pp_num>=18)%>%
  mutate(wkday = wday(date, label = TRUE))%>%
  mutate(workday = ifelse(is.na(holiday)&wkday!="Sun"&wkday!="Sat", "1", "0"))%>%
  filter(index >261)%>%
  mutate(type = if_else(date == as.Date("2022-09-19"), "pp_start", type))->pp_year2
  

  
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


pp_workdays2%>%
  view()



# sheet_write(pp_workdays2,
#             ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423",
#             sheet = "pp22")



# range_write(otc22.4, 
#             ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423", 
#             sheet = "2023",
#             range = "A3",
#             col_names = FALSE,
#             reformat = FALSE)  
#  

range_write(pp_workdays2, 
            ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423",
            sheet = "pp22",
            range = "A3",
            col_names = FALSE,
            reformat = FALSE)




##################################################  

# sheet_write(otc22.3, 
#             ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423", 
#             sheet = "2023")




otc22.3%>%
  mutate(date = date(in_time))%>%
  left_join(pp_year)%>%
  
  relocate(index, pp_num, date, type)%>%
  select(-index)->otc22.4

otc22.4%>%
  view()

otc22.4%>%
  view()






range_write(otc22.4,
            ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423",
            sheet = "2022",
            range = "A3",
            col_names = FALSE,
            reformat = FALSE)






pp_year2%>%
  view()


otc22.4%>%
  mutate(hrs_worked = out_time - in_time)%>%
  group_by(date)%>%
  summarise(hrs_worked = as.numeric(sum(hrs_worked)))%>%
  full_join(pp_year2)%>%
  arrange(date)%>%
  mutate(hrs_worked = ifelse(is.na(hrs_worked), 0, hrs_worked))%>%
  mutate(expected = ifelse(workday == 1, 8*60/1440, 0))%>%
  filter(date <= today())%>%
  mutate(hrs_worked = hrs_worked/1440)->otc22.5



otc22.5%>%
  view()

range_write(otc22.5, 
            ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423", 
            sheet = "day22",
            range = "A3",
            col_names = FALSE,
            reformat = FALSE)
