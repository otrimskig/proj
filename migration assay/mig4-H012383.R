library(tidyverse)
library(janitor)
library(lubridate)

#use nums, not chars for dates. 


readRDS("2023-04-04-migration.rds")%>%
  mutate(date = 20230404)->mig01

readRDS("2023-04-12-migration.rds")%>%
  mutate(date = 20230412)->mig02

readRDS("2023-04-24-migration.rds")%>%
  mutate(date = 20230424)->mig03



mig03%>%
  view()



str(mig01)

str(mig02)

str(mig03)


mig01%>%
  clean_names()%>%
  mutate(sample_name = as.character(sample_name))




mget(ls(pattern = "mig"))->migs123


migs123
#############################


for (p in migs123)function(df1){df1%>%clean_names()%>%
    mutate(sample_name = as.character(sample_name))
  }




lmap(migs123, tease)->migsnew



migs123[1]
migsnew[1]





reduce(migsnew, full_join)->migs_joined


migs_joined









migs_joined%>%
  clean_names()%>%
  #subtract blank value from abs. 
  mutate(abs_fixed = abs_590 - .044)%>%
  #remove blank sample from ds. 
  filter(sample_name!="blank")%>%
  
  #reformatting
  #get sample num from sample name. 
  mutate(sample_num = substr(sample_name, 1,1))%>%
  #type subset for wells that were not wiped apically. 
  mutate(type = ifelse(grepl(".2", sample_name), "total", "mig"))->mig1


#get "total" avg absorbance for wells not wiped, per condition per date. 
mig1%>%
  filter(type == "total")%>%
  group_by(date, sample_name)%>%
  #get mean for each group. 
  summarise(tot_mean = mean(abs_fixed))%>%
  
  
  mutate(sample_num = substr(sample_name, 1,1))%>%
  select(-sample_name)->totals




mig1%>%
  filter(type == "mig")%>%
  left_join(totals, by = c("date", "sample_num"))%>%
  mutate(mig_perc = (abs_fixed/tot_mean))%>%
  rename(line_num = sample_num)->mig_all


readRDS("mig-linenames.rds")%>%
  add_row(line_num = 6, line_name = "YUMM 3.2 pten -/- ; Parental")->lines



mig_all%>%
  left_join(lines%>%mutate(line_num = as.character(line_num)))->mig_all2








library(ggplot2)
library(jcolors)
library(viridis)

####"normalized to mean of non-migrated cells, per line per day"


mig_all2%>%
  filter(mig_perc<1)->mig_all3


mig_all3$line_name <- reorder(mig_all3$line_name, mig_all3$mig_perc, mean)


ggplot(mig_all3, aes(x = line_name, y = mig_perc, color = as.factor(date)))+
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






#########non-normalized. 

mig_all2->mig_all4


mig_all4$line_name <- reorder(mig_all4$line_name, mig_all4$abs_fixed, mean)

ggplot(mig_all2, aes(x = line_name, y = abs_fixed, color = as.factor(date)))+
  geom_point(size = 3, alpha = .7, shape = 16)+
  
  theme_classic()+
  
  #scale_y_continuous(limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 3), "lines"),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  xlab("Cell Line")+
  ylab("Abs at 590 nm")+
  labs(color = "date of exp")





