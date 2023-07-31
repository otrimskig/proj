
library(tidyverse)
library(janitor)
library(lubridate)
library(magrittr)
library(stats)


readRDS("20230628_scratch.rds")%>%
  mutate(conf = as.numeric(conf))%>%
  filter(conf>60)%>%
  filter(cells_plated=="30k")->scratch_df


scratch_df%>%
  filter(h_elapsed==0 |
           h_elapsed==24 |
           h_elapsed == 48 |
           h_elapsed == 72)->scratch_days


attach(scratch_days)
pairwise_results<-pairwise.t.test(#testing parameter (observation) 
  conf, 
  #grouping
  line_name, 
  #adjustment method
  p.adjust.method = "none")
detach()




library(pairwiseComparisons)
library(statsExpressions) 

p_values_00h<-scratch_days%>%
  filter(h_elapsed == 00)%>%
  mutate(line_name = as.factor(line_name))%>%


  pairwise_comparisons(data = ., 
                       x = line_name, 
                       y= conf,
                       type = "parametric",
                       var.equal = TRUE,
                       paired = FALSE,
                       p.adjust.method = "none")


 pairwise_comparisons(data = test1, 
                       x = line_num, 
                       y= conf)


 
p_values_days<-p_values_00h%>%
                rename(p_value_00h = p.value)%>%
                select(1:3)%>%
  left_join(p_values_24h%>%
              rename(p_value_24h = p.value)%>%
              select(1:3))%>%
  left_join(p_values_48h%>%
              rename(p_value_48h = p.value)%>%
            select(1:3))%>%
  left_join(p_values_72h%>%
              rename(p_value_72h = p.value)%>%
              select(1:3))




#saving 

library(openxlsx)

wb<-createWorkbook()

addWorksheet(wb, sheetName = "p_values_days")
writeDataTable(wb, sheet = "p_values_days", p_values_days, rowNames = FALSE)

saveWorkbook(wb, 
             paste0(Sys.Date(), " ", "incucyte_stats", ".xlsx"),
             overwrite = TRUE)
