library(tidyverse)
library(pracma)

combo<-readRDS("clds/combo.rds")

vs4718<-readRDS("clds/v4718.rds")

vs6766<-readRDS("clds/v6766.rds")

vehicle<-readRDS("clds/control.rds")



veh2<-vehicle%>%
  rename(day = time, 
         treatment=cohort)%>%
  filter(!is.na(tumor_vol))%>%
  
  mutate(tumor_vol = as.numeric(tumor_vol))%>%
  
  mutate(treatment = "Vehicle")%>%
  
  mutate(day = day-21)%>%
  
  filter(day>0)




jdf<-vs4718%>%
  full_join(vs6766)%>%
  full_join(combo)%>%
  rename(mouse_num=mouse)%>%
  
  mutate(mouse_num = substr(mouse_num, 1,5))%>%
  mutate(day = as.numeric(date_of_tumor_measurement-treatment_start_date))%>%
  
  
  rename(tumor_vol =total_tumor_vol)%>%
  
  ungroup()%>%
  select(c(mouse_num, treatment, tumor_vol, day))%>%
  
  
  full_join(veh2)



min_vols<-jdf%>%
  group_by(mouse_num) %>%
  filter(tumor_vol != 0) %>%
  slice(which.min(day)) %>%
  summarise(first_non_zero_tumor_vol = first(tumor_vol))


jdf2<-jdf%>%
  left_join(min_vols, by="mouse_num")%>%
  mutate(normalized_tumor_vol = tumor_vol/first_non_zero_tumor_vol)




saveRDS(jdf2, "clds/jared_df.rds")

write_csv(jdf2, "clds/jared-cohorts.csv", na = "")

library(ggplot2)


ggplot(jdf2)+
  
  geom_line(aes(x=day, y=normalized_tumor_vol, color=treatment, group=mouse_num), size=1, alpha=.3)+
  geom_point(aes(x=day, y=normalized_tumor_vol, color=treatment, group=mouse_num), size=2, alpha=.5)+
  
 
  theme_classic()





ggplot(jdf2)+
  geom_point(aes(x=day, y=tumor_vol, color=treatment, group=mouse_num), size=2, alpha=.5)+
  geom_line(aes(x=day, y=tumor_vol, color=treatment, group=mouse_num), size=1, alpha=.3)+
  

  
  theme_classic()




jdf2%>%
  ungroup()%>%
  group_by(treatment, day)%>%
  summarise(mean_tv_n = mean(normalized_tumor_vol),
            n=n())%>%
  filter(!is.na(mean_tv_n))%>%
  
  arrange(desc(n))
  
  
  ggplot()+
  
  geom_line(aes(x=day, y=mean_tv_n, color=treatment), size=1)+
  geom_point(aes(x=day, y=mean_tv_n, color=treatment), size=4)+
  
  theme_classic()

  
  
  
  
  
  

  
onsets<-jdf2%>%
  group_by(mouse_num) %>%
  filter(tumor_vol> 0) %>%
  
  slice(which.min(day)) %>%
  summarise(day_of_onset = first(day),
            first_vol = first(tumor_vol))


onsets2<-jdf2%>%
  left_join(onsets)%>%
  mutate(onset_day = day - day_of_onset + 1)%>%
  
  mutate(tumor_vol_change = tumor_vol - first_vol)%>%
  
  mutate(tumor_vol3= nthroot(tumor_vol,3))%>%
  
  mutate(tumor_vol_change3= nthroot(tumor_vol_change,3))%>%
  
  #if is in treatment window
  mutate(tw = if_else(day<28, "tw", "ut"))






ggplot(onsets2%>%
         filter(onset_day>0))+
  
  geom_line(aes(x=onset_day, y=tumor_vol3, color=treatment, grp=mouse_num), size=1, alpha=.7)+
  geom_point(aes(x=onset_day, y=tumor_vol3, color=treatment, grp=mouse_num), size=4)+
  
  theme_classic()



ggplot(onsets2%>%
         filter(onset_day>0))+
  
  geom_line(aes(x=onset_day, y=tumor_vol_change3, color=treatment, grp=mouse_num), size=1, alpha=.7)+
  geom_point(aes(x=onset_day, y=tumor_vol_change3, color=treatment, grp=mouse_num), size=4)+
  
  theme_classic()






ggplot(onsets2)+
  
  
  geom_rect(
    xmin = 0, xmax = 28, ymin = -Inf, ymax = Inf,
    fill = "#c8ebfa", alpha=.7, size = 1
  )+
  
  geom_line(aes(x=day, y=tumor_vol3, color=treatment, grp=mouse_num), size=1, alpha=.7)+
  geom_point(aes(x=day, y=tumor_vol3, color=treatment, grp=mouse_num), size=4)+
  
  theme_classic()


ggplot(onsets2%>%
         filter(tumor_vol_change>=0))+
  geom_rect(
    xmin = 0, xmax = 28, ymin = -Inf, ymax = Inf,
    fill = "#c8ebfa", alpha=.7, size = 1
  )+
  geom_line(aes(x=day, y=tumor_vol_change3, color=treatment, grp=mouse_num), size=1, alpha=.7)+
  geom_point(aes(x=day, y=tumor_vol_change3, color=treatment, grp=mouse_num), size=4)+
  
  theme_classic()







