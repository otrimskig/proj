#combines all migration assay data. 
#saves tidy ds as RDS file for later use. 

library(tidyverse)
library(janitor)
library(lubridate)
library(magrittr)

#read in all migration data, save all to a list of dfs. 
#add mutate cmd for each import so that correct date 
#is associated with each experiment.
linvs<-list(

inv01 = readRDS("2023-07-10-invasion assay 20230710 plate rd 590 nm.rds")%>%
  mutate(date = 20230705)

)



for (i in 1:length(linvs)){
  
  linvs[[i]]%<>%
    as_tibble()%>%
    clean_names()%>%
    mutate(sample_name = as.character(sample_name))
  
}

#join all dfs. can use reduce here instead of bind, since 
#flattening small dfs is not computationally demanding. 

reduce(linvs, full_join)->all_inv1




read_csv("2023-06-28 incucyte_line_names.csv")%>%
  mutate(line_num = as.character(line_num))->line_names



all_inv2<-all_inv1%>%
  filter(sample_name != "EX")%>%
 
  rename(abs_590_raw = abs_590)%>%
  
  #subtract blank value from abs. 
  mutate(abs_590 = abs_590_raw - .038)%>%
  
  #remove blank sample from ds. 
  filter(sample_name!="blank")%>%
  
  rename(line_num = sample_name)%>%
  
  
  left_join(line_names)
  

saveRDS(all_inv2, "all_invasion.rds")
