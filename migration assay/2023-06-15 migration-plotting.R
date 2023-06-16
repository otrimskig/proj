
library(tidyverse)
library(ggplot2)
library(jcolors)
library(viridis)


readRDS("all_mig_exp.rds")->all_exp2


####"normalized to mean of non-migrated cells, per line per day"


all_exp2%>%
  filter(mig_perc<1)->all_exp3


all_exp3$line_name <- reorder(all_exp3$line_name, all_exp3$mig_perc, mean)


ggplot(all_exp3, aes(x = line_name, y = mig_perc, color = as.factor(date)))+
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

all_exp2->all_exp4


all_exp4$line_name <- reorder(all_exp4$line_name, all_exp4$abs_590, mean)

ggplot(all_exp2, aes(x = line_name, y = abs_590, color = as.factor(date)))+
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




#########non-normalized exclude last experiment

all_exp4%>%
  filter(date!=20230412)->all_exp4



all_exp4$line_name <- reorder(all_exp4$line_name, all_exp4$abs_590, mean)

ggplot(all_exp4, aes(x = line_name, y = abs_590, color = as.factor(date)))+
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





