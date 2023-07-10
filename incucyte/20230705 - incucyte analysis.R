#2023-07-05
#analyzing and graphing incucyte data. 


library(tidyverse)
library(readxl)
library(janitor)
library(GGally)
library(paletteer)

readRDS("20230628_scratch.rds")%>%
  mutate(conf = as.numeric(conf))->scratch_df


df_wide<-scratch_df%>%
  pivot_wider(names_from = h_elapsed, values_from = conf)



scratch_df%>%
  filter(conf>60)%>%
 
  
  filter(cells_plated=="30k")%>%

  
  ggplot(., aes(x = h_elapsed, y = conf, color = line_name))+
  #geom_point(size=2)+
  geom_line(aes(group = well, color = line_name),alpha=.8, size = 1.2)+
  theme_classic()+
  scale_x_continuous(limits=c(0,72), breaks=c(0,24,48,72))+
  scale_color_paletteer_d("awtools::ppalette")->all_samples_plot1





scratch_df%>%
  filter(conf>60)%>%
  filter(cells_plated=="30k")%>%
  
  group_by(line_name, h_elapsed)%>%
  summarise(mean_conf = mean(conf), 
            n = n(),
            sd = sd(conf),
            se = sd/sqrt(n))->line_h_stats


line_h_stats%>%
  ggplot(., aes(x = h_elapsed, y = mean_conf, color = line_name))+
  geom_point(size=2)+
  geom_line(aes(color = line_name),alpha=.8, size = 1.2)+
  theme_classic()+
  scale_x_continuous(limits=c(0,72), breaks=c(0,24,48,72))+
  scale_y_continuous(limits=c(50,100))+
  scale_color_paletteer_d("awtools::ppalette")->means_all_plot





