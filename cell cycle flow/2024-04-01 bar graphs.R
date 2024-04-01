#R input using Data to R sheet
#necessary libraries
library(tidyverse)
library(googlesheets4)
library(gridExtra)

##################################
#sheet id and sheet name to read in from sheets. 

sheet_id<-"https://docs.google.com/spreadsheets/d/1CtLYkniUb8s1KLOD2Tmcu7hkx48bo8x2EoamsJ9Yyso/edit#gid=0"
name_of_sheet<-"Sheet1"


############
#authorize user.
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
sheets_df<-read_sheet(sheet_id, 
                      sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()



library(ggplot2)


df<-sheets_df%>%
  filter(stain!="Unstained")%>%
  
 
  mutate(across(contains("percent"), ~ as.numeric(.)))%>%
  
  mutate(treatment=factor(treatment, levels=c("VS-4718","VS-6766", "Combo")))%>%
  mutate(percent_s=if_else(percent_s<0, 0, percent_s))
  
  


df1<-df%>%
  filter(grepl("^b", tube_number))%>%
  
  pivot_longer(cols = contains("percent"), 
               names_to = "phase", 
               names_pattern = "percent_(.*)", 
               values_to = "percent")%>%
  
  mutate(phase=toupper(phase))%>%
  mutate(phase=factor(phase, levels=c("G1", "S", "G2")))%>%
  
  mutate(concentration=factor(concentration, levels=c("DMSO", "150 nM", "300 nM", "600 nM")))


p1<-df1%>%
  ggplot(.)+
  geom_bar(aes(x=phase , y=percent, fill=factor(concentration)), colour="black",position="dodge", stat="identity")+
  
  
  facet_wrap(~treatment)+

  
  scale_fill_manual(values=c("white", brewer.pal(name = "Blues", n=4)[2:4]),
                    name = "Concentration")+
  
  labs(x="Phase",
       y="Cell Cycle Distribution (%)")+
  theme_classic()+
  labs(x="Phase",
       y="Cell Cycle Distribution (%)",
       title="YUMM 3.2 Pten -/- ; HA-AKT1-E17K ; L/G",
       subtitle="24h incubation")+
  
  theme(plot.title=element_text(size=16,face="bold"))








df2<-df%>%
  filter(grepl("^[0-9]", tube_number))%>%
  
  pivot_longer(cols = contains("percent"), 
               names_to = "phase", 
               names_pattern = "percent_(.*)", 
               values_to = "percent")%>%
  
  mutate(phase=toupper(phase))%>%
  mutate(phase=factor(phase, levels=c("G1", "S", "G2")))%>%
  
  mutate(concentration=factor(concentration, levels=c("DMSO", "150 nM", "300 nM", "600 nM")))


p2<-df2%>%
  ggplot(.)+
  geom_bar(aes(x=phase , y=percent, fill=factor(concentration)), colour="black",position="dodge", stat="identity")+
  
  
  facet_wrap(~treatment)+
  
  
  scale_fill_manual(values=c("white", brewer.pal(name = "Greens", n=4)[2:4]),
                    name = "Concentration")+
  theme_classic()+
  labs(x="Phase",
       y="Cell Cycle Distribution (%)",
       title="YUMM 3.2 Pten -/- ; L/G",
       subtitle="24h incubation")+

  theme(plot.title=element_text(size=16,face="bold"))

p2





 
grid.arrange(p2, p1)
