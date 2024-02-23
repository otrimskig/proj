library(flowWorkspace)
library(CytoML)
library(tidyverse)
library(gridExtra)

#import data xml
ws <- open_diva_xml('exp/10-4-23 Garrett single cell sort GFP and RFP.xml')

#make empty plot list
plot_list<-list()

#count samples and create vector of 1:n
sample_count<-c(1:length(diva_get_sample_groups(ws)$tube))

#we only need the 1st 6 samples. 
for(i in 1:6){

#pull gating set from xml file. 
#name refers to the sample
gs <- diva_to_gatingset(ws, name=1, subset = c(i))

#paths for gatings. 
nodelist <- gs_get_pop_paths(gs, path = "auto")

#make list of plots. 
#need to convert them into ggplots. 
#flowWorkspace uses autoplots, but can convert into ggplot object. 
plot_list[[i]]<-as.ggplot(autoplot(gs, nodelist[5], bins=64)+
  theme_bw()+
    theme(legend.position = "none")
  )

}


#arranging all plots. 
grid.arrange(grobs=plot_list,
             ncol=3)