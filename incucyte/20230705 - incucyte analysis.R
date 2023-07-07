#2023-07-05
#analyzing and graphing incucyte data. 


library(tidyverse)
library(readxl)
library(janitor)
library(GGally)

readRDS("20230628_scratch.rds")%>%
  mutate(conf = as.numeric(conf))->scratch_df


df_wide<-scratch_df%>%
  pivot_wider(names_from = h_elapsed, values_from = conf)



scratch_df%>%
  filter(conf>60)%>%
 
  
  filter(cells_plated=="30k")%>%

  
  ggplot(., aes(x = h_elapsed, y = conf, color = line_name))+
  #geom_point(size=2)+
  geom_line(aes(group = well, color = line_name),alpha=1)+
  theme_classic()+
  scale_x_continuous(limits=c(0,72), breaks=c(0,24,48,72))+
  scale_color_paletteer_d("awtools::ppalette")


