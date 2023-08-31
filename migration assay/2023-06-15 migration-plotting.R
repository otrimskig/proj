
library(tidyverse)
library(ggplot2)
library(jcolors)
library(viridis)

library(rstatix)
library(ggpubr)





readRDS("all_mig_exp.rds")->all_exp2

# #export for non-R users
# all_exp2%>%
#   select(date,
#          line_num, 
#          line_name, 
#          abs_590,
#          abs_590_raw)%>%
#   write_csv("GO_all_migrations.csv")




library(RColorBrewer)
palette_colors<-brewer.pal(n=6,"Accent")

group.colors.name <- c("YUMM 3.2 pten -/- ; HA-AKT1-E17K" = "#7FC97F", 
                  "YUMM 3.2 pten -/- ; HA-FAK (wt)" = "#BEAED4", 
                 "YUMM 3.2 pten -/- ; HA-FAK-K454R" ="#FDC086", 
                  "YUMM 3.2 pten -/- ; HA-FAK-Y394E" = "#5bf4fc", 
                 "YUMM 3.2 pten -/- ; HA-ndel-FAK"= "#386CB0",
                  "YUMM 3.2 pten -/- ; Parental" = "#F0027F")



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



###############################################





total_means<-all_exp2%>%
  select(date, line_name, abs_590)%>%
  group_by(line_name)%>%
  summarise(
    abs_mean = mean(abs_590),
    sd = sd(abs_590)
    
  )


line_counts<-all_exp2%>%
  select(date, line_name, abs_590)%>%
  count(line_name)
  

all_dates_sum<-left_join(total_means, line_counts)%>%
  mutate(se = sd/sqrt(n))

#export for non-R users
all_dates_sum%>%
  write_csv("GO_all_migrations_summary_data.csv")




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
                size = .1)#+
  #scale_fill_manual(values=group.colors.name)




attach(all_exp2)
t_tests1<-pairwise.t.test(abs_590,
                g = line_name,
                p.adjust.method = "none")
detach()

t_tests1[["p.value"]]%>%
  as_tibble(row_names = TRUE)

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





