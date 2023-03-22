library(googlesheets4)
library(tidyverse)
library(lubridate)
library(hms)

otc<-read_sheet("https://docs.google.com/spreadsheets/d/1EMKP1T11kPVPc2-wmztRzEPNngxkw0rcAkpz09I3rFA/edit#gid=0", sheet = "2022")


otc%>%
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
  
  
  
  mutate(date = ifelse(index<=82, gsub(",", "/22", date), 
                       ifelse(index>=82, gsub(",", "/23", date), date)))%>%
  mutate(date = mdy(date))%>%
  mutate(duration = gsub("m", "M", duration))%>%
  mutate(duration = as.period(duration))%>%
  
  mutate(day = wday(date, label = TRUE))%>%
  
  
  mutate(duration = as.numeric(make_difftime(as.duration(duration))))%>%
  
  # mutate(in_time = hm(in_time))%>%
  # mutate(out_time = hm(out_time))%>%

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
  mutate(duration_mins = as.numeric(out_time - in_time))->otc2
  
  
  
  
 









view(otc2)

sheet_write(otc2, ss= "https://docs.google.com/spreadsheets/d/1tKUUyoX9Gufif4kHrN0rivxfPp6jSnzN6Qzv7nPiw0A/edit#gid=698193423", sheet = "2022")



