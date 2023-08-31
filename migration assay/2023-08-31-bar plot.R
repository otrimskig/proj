
library(tidyverse)
library(ggplot2)
library(jcolors)


#library(rstatix)
library(ggpubr)

#read in line_info
read_csv("lines.csv")->line_info


#read in data previously cleaned. 
readRDS("all_mig_exp.rds")->mig_df

line_ns<-mig_df%>%
  select(date, line_name, abs_590)%>%
  count(line_name)


#get basic stats for experiments by line. 
sds_df<-mig_df%>%
  left_join(line_ns)%>%
  select(date, line_name, abs_590, n)%>%
  group_by(line_name)%>%
  summarise(
    abs_mean = mean(abs_590),
    sd = sd(abs_590)
  )


#creates summary dataframe for plotting. 
summ_df<-mig_df%>%
  select(line_name, abs_590)%>%
  group_by(line_name)%>%
  
  #creates summary stats for each line. 
  summarise(
    abs_mean = mean(abs_590),
    sd = sd(abs_590)
    )%>%
  
  #join that to the number of obs, per line. 
  left_join(line_ns)%>%
  
  #finally, calc se for each line. 
  mutate(se = sd/sqrt(n))%>%
  left_join(line_info)%>%
  filter(exclude==0)




#order lines by correct order. Set in csv file. 
summ_df$line_name <- reorder(summ_df$line_name, summ_df$order)



 
#now have everything we need for plotting. 

ggplot(summ_df, aes(x = line_name, y = abs_mean, fill = line_name))+
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



####################################################
#####################################################
######################################################
#invasion plotting. 


readRDS("../inv assay/all_invasion.rds")->inv_df
read_csv("lines_inv.csv")->line_info

line_ns<-inv_df%>%
  select(date, line_name, abs_590)%>%
  count(line_name)


#get basic stats for experiments by line. 
sds_df<-inv_df%>%
  left_join(line_ns)%>%
  select(date, line_name, abs_590, n)%>%
  group_by(line_name)%>%
  summarise(
    abs_mean = mean(abs_590),
    sd = sd(abs_590)
  )



#creates summary dataframe for plotting. 
summ_df2<-inv_df%>%
  select(line_name, abs_590)%>%
  group_by(line_name)%>%
  
  #creates summary stats for each line. 
  summarise(
    abs_mean = mean(abs_590),
    sd = sd(abs_590)
  )%>%
  
  #join that to the number of obs, per line. 
  left_join(line_ns)%>%
  
  #finally, calc se for each line. 
  mutate(se = sd/sqrt(n))%>%
  left_join(line_info)%>%
  filter(exclude==0)


summ_df2$line_name <- reorder(summ_df2$line_name, summ_df2$order)



#bar plot
ggplot(summ_df2, aes(x = line_name, y = abs_mean, fill = line_name))+
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




