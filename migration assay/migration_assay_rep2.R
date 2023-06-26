library(tidyverse)
library(janitor)
library(lubridate)


readRDS("migration assay/2023-04-19_13-03-migration0412.rds")->mig



mig%>%
  view()

readRDS("2023-05-05-migration assay 20230426 plate read abs 590 nm.rds")%>%
  mutate(date = "20230505")%>%
  left_join(mig)->mig





mig%>%
  clean_names()%>%
  mutate(abs_fixed = abs_590 - .044)%>%
  filter(sample_name!="blank")%>%
  mutate(sample_num = substr(sample_name, 1,1))%>%
  mutate(type = ifelse(grepl(".2", sample_name), "total", "mig"))->mig1
  



mig1%>%
  filter(type == "total")%>%
  group_by(date, sample_name)%>%
  summarise(tot_mean = mean(abs_fixed))%>%
  mutate(sample_num = substr(sample_name, 1,1))%>%
  select(-sample_name)->totals


mig1
totals
mig1%>%
  filter(type == "mig")%>%
  left_join(totals, by = c("date", "sample_num"))%>%
  mutate(mig_perc = (abs_fixed/tot_mean))%>%
  rename(line_num = sample_num)->mig_all


readRDS("migration assay/2023-04-19_13-40-linenames.rds")->lines



mig_all%>%
  left_join(lines%>%mutate(line_num = as.character(line_num)))->mig_all2


mig_all2$line_name <- reorder(mig_all2$line_name, mig_all2$mig_perc, mean)


library(ggplot2)


ggplot(mig_all2, aes(x = line_name, y = mig_perc, color = as.factor(date)))+
  geom_point(size = 3, alpha = .7, shape = 16)+
  theme_classic()+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 3), "lines"),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  xlab("Cell Line")+
  ylab("Percent Migrated Cells")+
  labs(color = "date of exp")
