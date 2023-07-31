

library(tidyverse)
library(janitor)
library(lubridate)
library(magrittr)
library(stats)
library(ggsignif)

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
                size = .1)+
  # geom_signif(
  #   comparisons = list(c("YUMM 3.2 Pten -/- ; HA-AKT1-E17K ; Luc/GFP", 
  #                        "YUMM 3.2 Pten -/- ; HA-ndel-FAK ; Luc/GFP")),
  #   map_signif_level = FALSE,
  #   annotations = "poo"
  # )
  
  # geom_signif(
  #   comparisons = list(c(ttests$groups[[1]])),
  #   annotations = ttests$expression[[1]],
  #   map_signif_level = FALSE
  # )

geom_signif(
  comparisons = list(c("YUMM 3.2 Pten -/- ; HA-AKT1-E17K ; Luc/GFP", 
                       "YUMM 3.2 Pten -/- ; HA-ndel-FAK ; Luc/GFP")),
  annotations = list(~italic(p)[uncorrected]==0.561),
  map_signif_level = FALSE
)


library(pairwiseComparisons)
library(statsExpressions) 


ttests<-pairwise_comparisons(data = inv_df, 
                     x = line_name, 
                     y= abs_590,
                     type = "parametric",
                     var.equal = TRUE,
                     paired = FALSE,
                     p.adjust.method = "none")%>%
  
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1)


ttests%>%
  view()


ttests$expression[[1]]
list(ttests$groups[[1]])

ttests$expression[[20]]%>%

