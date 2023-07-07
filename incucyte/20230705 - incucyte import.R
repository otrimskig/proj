#2023-07-05
#importing incucyte confluency data, and cleaning. 

library(tidyverse)
library(readxl)
library(janitor)
#note: saveRDS is commented out, so if changes need to be made,
#make sure to check end of .R doc, prior to sourcing. 

#get line_names
read_csv("2023-06-28 incucyte_line_names.csv")%>%
  mutate_all(as.character)->line_names



#read in incucyte confluence data
read_excel("20230705 garrett wound assay.xlsx")->df1

df2<-df1%>%
  #remove extraneous info
  slice(-c(1:6))%>%
  #clean up
  row_to_names(1)%>%
  select(-1)%>%
  clean_names()%>%
  
  #change hours elapsed with long decimals to approx hrs elapsed for each scan. 
  mutate(elapsed = as.numeric(substr(elapsed, 1,3)))%>%
  rename(h_elapsed = elapsed)%>%
  
  #rearrange data to meet tidy expectations. 
  pivot_longer(cols = 2:last_col(), names_to ="well", values_to = "conf")%>%
  
  #create row and column variables to enable easier identification
  #of lines and conditions. 
  mutate(row = substr(well, 1,1))%>%
  mutate(col = substr(well, 2,2))%>%
  
  #input experiment-specific conditions. 
  mutate(cells_plated = if_else(row=="a"|row=="b"|row=="c"|row=="d", "30k",
                                "15k"))%>%
  mutate(exp_start_date = "2023-06-28")%>%
  mutate(conf_mask = "yum 5.1")%>%
  
  
  #remove column 8 - there were no cells. 
  filter(col != 8)%>%
  
  #attach line names. 
  mutate(line_num = col)%>%
  left_join(line_names)
  

#dataset now has everything needed to be analyzed. 

saveRDS(df2, "20230628_scratch.rds")
  
  
  
  
