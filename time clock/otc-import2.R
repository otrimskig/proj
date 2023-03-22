library(googlesheets4)
library(tidyverse)

otc<-read_sheet("https://docs.google.com/spreadsheets/d/1DYwkZEVK39qMZl1vYrg3R7hi41nye1yikUNYN4tzWbs/edit#gid=190808741")


otc%>%
  rename(col1 = 1)%>%
  
  
  mutate(date = paste(str_split(.$col1, "\\s", simplify=TRUE)[,1], 
                      str_split(.$col1, "\\s", simplify=TRUE)[,2]))%>%
  
  mutate(in_time = paste(str_split(.$col1, "\\s", simplify=TRUE)[,3], 
                      str_split(.$col1, "\\s", simplify=TRUE)[,4]))%>%
  
  mutate(out_time = trimws(paste(str_split(.$col1, "\\s", simplify=TRUE)[,5], 
                      str_split(.$col1, "\\s", simplify=TRUE)[,6])))%>%
  
  mutate(duration = paste0(str_split(.$col1, "\\s", simplify=TRUE)[,7],
                       str_split(.$col1, "\\s", simplify=TRUE)[,8]))%>%
  
  select(-col1)%>%
  filter(out_time != "missing")%>%
  
  
  
  
  
  
  
  view()




