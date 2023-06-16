#combines all migration assay data. 
#saves tidy ds as RDS file for later use. 

library(tidyverse)
library(janitor)
library(lubridate)
library(magrittr)

#read in all migration data, save all to a list of dfs. 
#add mutate cmd for each import so that correct date 
#is associated with each experiment. 


lmigs<-list(

mig01 = readRDS("2023-04-04-migration.rds")%>%
  mutate(date = 20230404),

mig02 = readRDS("2023-04-12-migration.rds")%>%
  mutate(date = 20230412),

mig03 = readRDS("2023-04-24-migration.rds")%>%
  mutate(date = 20230424)

)


#############################
#clean names, format all correctly. 
#repeat transformations for each df in list. 


for (i in 1:length(lmigs)){
  
  lmigs[[i]]%<>%
    as_tibble()%>%
    clean_names()%>%
    mutate(sample_name = as.character(sample_name))
  
  }


#join all dfs. can use reduce here instead of bind, since 
#flattening small dfs is not computationally demanding. 

reduce(lmigs, full_join)->all_mig1


##############################################

#get names for each line corresponding to line num. 
readRDS("mig-linenames.rds")%>%
  add_row(line_num = 6, line_name = "YUMM 3.2 pten -/- ; Parental")%>%
  
  #ensure it is in character format for join. 
  mutate(line_num = as.character(line_num))->line_names



#transforming full dataset now. 


all_mig1%>%
  
  rename(abs_590_raw = abs_590)%>%
  
  #subtract blank value from abs. 
  mutate(abs_590 = abs_590_raw - .044)%>%
  
  #remove blank sample from ds. 
  filter(sample_name!="blank")%>%
  
  #reformatting
  #get line num from sample name. 
  mutate(line_num = substr(sample_name, 1,1))%>%
  
  #add line names based on line num. 
  left_join(line_names)%>%
  
  
  #add type label to distinguish wiped vs. non-wiped 
  #wells (total vs. experimental)
  mutate(type = ifelse(grepl(".2", sample_name), "total", "exp"))%>%
  
  #remove now-unecessary column. 
  select(-sample_name)->all_mig2




#calculate "total" avg absorbance for wells not wiped, per condition per date. 
all_mig2%>%
  
  filter(type == "total")%>%
  group_by(date, line_num)%>%
  
  #get mean for each group. 
  summarise(tot_mean = mean(abs_590))->totals



#add calculated controls for each experiment condition + date,
#and calculate migration percentage for each. 
 
all_mig2%>%
  
  #keep only experimental. 
  filter(type == "exp")%>%
  #drop now-unecessary column
  select(-type)%>%
  
  #add in totals calculated above, for each sample
  left_join(totals, by = c("date", "line_num"))%>%
  
  
  #calculate migration percentage
  mutate(mig_perc = (abs_590/tot_mean))%>%
  
  
  #final tidying
  relocate(date, line_num, line_name,
           xcol, xrow, abs_590, 
           tot_mean, mig_perc)%>%
  arrange(date, line_num, xrow)->all_exp





saveRDS(all_exp, "all_mig_exp.rds")



