library(pzfx)
library(tidyverse)
library(janitor)


#get list of all tables in file. 
pzfx_tables(path = "ds/myrAKT1 - Drug study.pzfx")


df<-read_pzfx(path="ds/myrAKT1 - Drug study.pzfx", table = "Raw Tumor Size (Time Delay)", strike_action="star")

df2<-df%>%
  rename(x24117=Control_1,
         x24072=Control_2,
         x24114=Control_3,
         x24112=Control_4,
         x24111=Control_5,
         x24073=Control_6,
         x24076=Control_7,
         x24077=Control_8,
         x24078=Control_9,
         x24283=Control_10,
         x24170=Control_11,
         x24281=Control_12,
         x24280=Control_13,
         x24168=Control_14,
         x24272=Control_15,
         x24278=Control_16)%>%
  select(-c(Control_17:last_col()))%>%
  rename(time=Var.1)


df3<-df2%>%
  pivot_longer(cols=2:last_col(), names_to = "mouse_num", values_to = "tumor_vol")%>%
  mutate(mouse_num=str_replace(mouse_num, "^x", ""))%>%
  mutate(time=as.numeric(time))%>%
  mutate(cohort="vehicle")
saveRDS(df3, "clds/control.rds")
