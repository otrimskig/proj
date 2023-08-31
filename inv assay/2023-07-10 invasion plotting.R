

library(tidyverse)
library(janitor)
library(lubridate)
library(magrittr)
library(stats)

#read in data
readRDS("all_invasion.rds")->inv_df





ggplot(inv_df, aes(x = line_name, y = abs_590))+
  geom_point(aes(color = line_name), size = 3, alpha = .7)+
  theme_classic()+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 3), "lines"),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  xlab("Cell Line")+
  ylab("Percent Migrated Cells")+
  theme(legend.position = "none")


#getting means for all lines. 
total_means<-inv_df%>%
  select(date, line_name, abs_590)%>%
  group_by(line_name)%>%
  summarise(
    abs_mean = mean(abs_590),
    sd = sd(abs_590)
    )


#getting n's for all lines. 
line_counts<-inv_df%>%
  select(date, line_name, abs_590)%>%
  count(line_name)


all_dates_sum<-left_join(total_means, line_counts)%>%
  mutate(se = sd/sqrt(n))


all_dates_sum$line_name <- reorder(all_dates_sum$line_name, 
                                   all_dates_sum$abs_mean)


#bar plot
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






#finding p-value.
attach(inv_df)
pairwise_results<-pairwise.t.test(abs_590, line_name, p.adjust.method = "none")
detach()



attributes(pairwise_results[["p.value"]])[["dimnames"]][[1]]->group_a




pairwise_results[["p.value"]]%>%
  as_tibble()%>%
  
  mutate(group_a = group_a)%>%
  relocate(group_a)%>%
  arrange(group_a)%>%
  

  pivot_longer(cols=2:7, names_to = "group_b", values_to = "p_value")%>%
  

  mutate(group_c = group_a)->table


table%>%
  select(group_a, group_b, p_value)->tableab

table%>%
  select(group_b, group_c, p_value)%>%
  rename(group_a=group_b, group_b=group_c)->tablebc

full_join(tableab, tablebc)%>%
  filter(!is.na(p_value))%>%
  pivot_wider(names_from = "group_b", values_from = "p_value")->all_ps
  
  
all_ps%>%
  view()


str(all_ps)


arrange(all_ps, group_a)
  
  # rename(group_name = group_a)%>%
  # mutate(num = c(1:6))%>%
  # 
  # relocate(group_name, num)%>%
  # 
  # 
  # rename_at(vars(3:last_col()), ~as.character(c(1:6)))

all_ps%>%
  pull(group_a)->v

sort(v)


all_ps%>%
  as_tibble()%>%
  arrange(group_a)%>%
  view()
