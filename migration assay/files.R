
library(fs)
library(tidyverse)


list.files("D://", full.names = TRUE)%>%
  as_tibble()%>%
  rename(filename = value)%>%
  filter(grepl(".tif", filename))->imgs


          
file_info(pull(imgs))->files


files%>%
  select(path, modification_time)%>%
  filter(modification_time > "2023-04-18")%>%
  arrange(modification_time)%>%
  mutate(index = row_number())%>%
  mutate(new_name = paste0("img", sprintf("%02d", row_number()), ".tif"))%>%
  mutate(old_name = basename(path))%>%
  select(-c(1:(ncol(.)-2)))->names


dir_path="D:"

file.rename(paste0(dir_path, "//", pull(names[2])), paste0(dir_path, "//", pull(names[1])))



