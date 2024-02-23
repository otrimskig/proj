library(tidyverse)
library(janitor)
library(readxl)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(gridExtra)
library(rstatix)


#main dataset which has results from flow data. 
df<-read_csv("ds/SubQ FAK Data.csv")

df2<-df%>%
  clean_names()%>%
  group_by(line_name)%>%
  left_join(read_csv("ds/line_names_KS.csv"))


# 
# stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif")

# attach(df2)
# pairwise.t.test(tcell_percent_live,
#                 line_name, p.adjust.method = "none")
# detach()









####set factor levels, which will change plot x-axis order.
df2$line_name <- factor(df2$line_name, 
                        levels = c("pten_null", "akt1", "ndel", "y397e", "k454r"))



#matches line names to cleaned names to be used in figure labels. 
line_labs<-df2%>%select(line_name, line_label)%>%
  group_by(line_name)%>%
  slice(1)



#variables to be plotted on y-axis, 
#plus corresponding clean label for y-axis, plus axis titles. 
plot_specs<-read_csv("ds/plot_titles_KS.csv")%>%
  clean_names()%>%
  rename(y_var=variable)



#for loop creates list of plots, which can then be faceted in next operation. 
#create empty list. 
plot_list <- list() 

#set comparisons for p-values to be displayed on plots. 
my_comparisons <- list( c("ndel", "y397e"), 
                        c("k454r", "y397e"), 
                        c("pten_null", "y397e"))


data.frame(df2)%>%
pairwise_t_test(tcell_percent_live ~ line_name, p.adjust.method = "none")%>%
  add_xy_position(x = "line_name")





data.frame(df2)%>%
  pairwise_t_test(tcell_percent_live ~ line_name, p.adjust.method = "none")%>%
  add_xy_position(x = "line_name")


data.frame(df2) %>%
  pairwise_t_test(reformulate(response = plot_specs$y_var[7], 
                              termlabel = "line_name"), 
                  p.adjust.method = "fdr") %>%
  
  add_xy_position(x = "line_name")






#####################################
###################################
#loop through all y-axis variables designated. 
for(i in 1:length(plot_specs$y_var)){
  
  pwc<-data.frame(df2) %>%
    pairwise_t_test(reformulate(response = plot_specs$y_var[i], 
                                termlabel = "line_name"), 
                    p.adjust.method = "fdr") %>%
    
    add_xy_position(x = "line_name")
  
  
    plot_list[[i]] <- ggplot(df2, aes(x = as.factor(line_name), 
                                    #.data[[]] wrap allows return of value to be used in 
                                    #the function. Otherwise would have "" in it. 
                                    
                                    y = .data[[plot_specs$y_var[i]]], 
                                    color=line_name)) +
    
    #main plot
    geom_beeswarm(
      cex=3,
      priority = "density")+
    
    #add mean line
    stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = .75, color="black")+
    
    
      
      
    #p-value comparisons  
    stat_pvalue_manual(pwc, 
                       hide.ns = TRUE, 
                       label = "p.adj.signif",
                       tip.length = 0.01, 
                       step.increase = .01) +
      
    #   #add p-value comparisons. (manual comparisons)
    # stat_compare_means(comparisons = my_comparisons)+
    
    #expands y axis to ensure p-value labels aren't cut off. 
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
    
    #set titles and labels  
    ggtitle(plot_specs$plot_title[i])+
    labs(y = plot_specs$y_axis_lab[i])+
    scale_x_discrete(labels = line_labs$line_label)+
    
    #theme elements
    theme_classic()+
    theme(axis.title.x = element_blank(),#remove x-axis title
          legend.position = "none",#remove legend
          axis.text.x = element_text(angle = 45, hjust = 1), #set angle and justification of group names. 
          
          #title size and spacing. center with hjust.
          plot.title = element_text(size=11,
                                    face="bold",
                                    hjust = 0.5, vjust=0),
          
          #add some margin so plots are properly-spaced when faceted. 
          plot.margin = margin(t=3,b=25,l=10))
  
}



grid.arrange(grobs=plot_list,
             ncol=6)





# names1<-df2%>%
# colnames()
# voi<-c("tcell_","cd4_","cd4_pd1_","cd8_","cd8_pd1_")
# 
# 
# matches <- grep(paste(paste0("^",voi), collapse="|"), names1, value = TRUE) 
#     
# matches%>%
#   as_tibble()%>%
#   write_csv("ds/plot_titles.csv")



# df%>%
#   clean_names()%>%
#   select(line_name)%>%
#   group_by(line_name)%>%
#   slice(1)%>%
#   write_csv("ds/line_names.csv")

