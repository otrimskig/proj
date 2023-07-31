

library(tidyverse)
library(janitor)
library(lubridate)
library(magrittr)
library(stats)


#read in main data
readRDS("all_invasion.rds")->inv_df


#df of all lines in experiment plus corresponding line number. 
lines<-inv_df%>%
  select(line_num, line_name)%>%
  group_by(line_num)%>%
  slice(1)%>%
  ungroup()



#finding p-value.Run stats analysis
attach(inv_df)
pairwise_results<-pairwise.t.test(#testing parameter (observation) 
  abs_590, 
  #grouping
  line_name, 
  #adjustment method
  p.adjust.method = "none")
detach()



pairwise_results[["p.value"]]%>%
  as_tibble()%>%
  mutate(group_2 = attributes(pairwise_results[["p.value"]])[["dimnames"]][[1]])%>%
  relocate(group_2)%>%
  pivot_longer(cols=2:last_col(), names_to = "group_1", values_to = "p_value")%>%
  relocate(group_1)%>%
  filter(!is.na(p_value))%>%
  arrange(group_1, group_2)->all_results



all_results%>%
  arrange(p_value)->p_values


all_results_1<-all_results
all_results_2<-all_results%>%
  relocate(group_2)%>%
  rename(group_1 = group_2, group_2 = group_1)


full_join(all_results_1, all_results_2)%>%
  
  
  pivot_wider(names_from = "group_2", values_from = "p_value")%>%
  relocate(group_1, last_col())%>%
  rename(group = group_1)%>%
  column_to_rownames(var = "group")->results_table




library(openxlsx)

wb<-createWorkbook()

addWorksheet(wb, sheetName = "results_table")
writeData(wb, sheet = "results_table", results_table, rowNames = TRUE)


addWorksheet(wb, sheetName = "p_values")
writeDataTable(wb, sheet = "p_values", p_values)

saveWorkbook(wb, 
             paste0(Sys.Date(), " ", "invasion_stats", ".xlsx"),
             overwrite = TRUE)



