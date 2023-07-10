

library(tidyverse)
library(janitor)
library(lubridate)
library(magrittr)


readRDS("all_invasion.rds")->inv_df


ggplot(inv_df, aes(x = line_name, y = abs_590))+
  geom_point(aes(color = line_name))+
  theme_classic()+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 3), "lines"),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  xlab("Cell Line")+
  ylab("Percent Migrated Cells")












total_means<-all_inv2%>%
  select(date, line_name, abs_590)%>%
  group_by(line_name)%>%
  summarise(
    abs_mean = mean(abs_590),
    sd = sd(abs_590)
    
  )














line_counts<-all_inv2%>%
  select(date, line_name, abs_590)%>%
  count(line_name)


all_dates_sum<-left_join(total_means, line_counts)%>%
  mutate(se = sd/sqrt(n))


all_dates_sum$line_name <- reorder(all_dates_sum$line_name, 
                                   all_dates_sum$abs_mean)

ggplot(all_dates_sum, aes(x = line_name, y = abs_mean, fill = line_name))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 3), "lines"),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none")+
  xlab("Cell Line")+
  ylab("Abs at 590 nm")+
  geom_errorbar(aes(x = line_name, 
                    ymin = abs_mean-se,
                    ymax = abs_mean+se),
                width =.3,
                size = .1)
