library(googlesheets4)
library(tidyverse)

docs_address<-"https://docs.google.com/spreadsheets/d/1YbjziK9ZKUtjQivyTnq_oUx6OhqRcaSF2DVAakyilAc/edit#gid=1853303322"







#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#get experiment set-up data. 
genes_key<-read_sheet(docs_address, 
           sheet = "exp_setup", range = "A8:C")%>%
  rename_with(~ paste0("gene_", .))

sample_key<-read_sheet(docs_address, 
           sheet = "exp_setup", range = "E8:G")%>%
  rename_with(~ paste0("sample_", .))



#read plate map
plate_map<-read_sheet(docs_address, 
                      sheet = "plate_map", range = "B2:Z18")%>%
  janitor::clean_names()%>%
  mutate(across(1:last_col(), as.character))%>%
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  rename(row=x1)%>%
  
  
  pivot_longer(2:last_col(),names_to = "xcol",values_to = "well_combo")%>%

  
  
  mutate(well = paste0(row, sub("x","", xcol)))%>%
  relocate(well)%>%
  select(-row, -xcol)%>%
  filter(!is.na(well_combo))


#get measurements
qpcr_reads<-read_sheet(docs_address,
                      sheet = "qpcr_reads")%>%
  mutate(across(1:last_col(), as.character))%>%
  rename(cq=Cq, well=Well)%>%
  mutate(cq=na_if(cq, "N/A"))%>%
  mutate(cq=as.numeric(cq))








mapped_data<-left_join(plate_map, qpcr_reads)%>%
  filter(!is.na(well_combo))%>%
  
  mutate(sample_shorthand=substr(well_combo, 1,2))%>%
  
  mutate(gene_shorthand=substr(well_combo, 4,5))%>%
  left_join(genes_key)%>%
  left_join(sample_key)%>%
  relocate(cq, .after=last_col())%>%
  arrange(sample_shorthand, gene_shorthand, well)


range_write(docs_address, sheet= "mapped_data", matched_data)




control_genes<-genes_key%>%
  filter(gene_type=="control")%>%
  pull(gene_name)

experimental_genes<-genes_key%>%
  filter(gene_type=="experimental")%>%
  pull(gene_name)





for(experimental_gene in experimental_genes){
  
  
subset_data<-matched_data%>%
  filter(gene_name==experimental_gene|gene_type=="control")

for(control_gene in control_genes){
subset_data2<-subset_data%>%
  filter(gene_type=="experimental"|gene_name==control_gene)

 



control<-subset_data2%>%
  filter(gene_type=="control")%>%
  group_by(sample_shorthand)%>%
  summarise(mean_control_cq=mean(cq))
  
  


exp<-subset_data2%>%
  arrange(sample_shorthand)%>%
  filter(gene_type=="experimental")%>%
  left_join(control)%>%
  mutate(ct=2**(mean_control_cq-cq))%>%
  group_by(sample_shorthand)%>%
  mutate(mean_ct=mean(ct))


#get mean_ct value from control sample.
normalize_to<-exp%>%filter(sample_type=="control")%>%
  slice(1)%>%pull(mean_ct)


exp2<-exp%>%
  mutate(norm_ct=ct/normalize_to)%>%
  group_by(sample_shorthand)%>%
  mutate(mean_norm_ct=mean(norm_ct))%>%
  mutate(se_norm_ct=plotrix::std.error(norm_ct))




t_test_norm<-exp2%>%filter(sample_type=="control")%>%
  pull(norm_ct)


exp3<-exp2%>%
  mutate(t_test=t.test(norm_ct, t_test_norm, paired = TRUE, alternative = "two.sided")$p.value)%>%
  
  mutate_all(~ ifelse(is.nan(.), NA, .))

sheet_name<-paste0(experimental_gene," v. ",control_gene)



if (sheet_name %in% sheet_names(docs_address)) {
  
  range_write(docs_address, sheet= sheet_name, exp3)
  
  
} else {
  
  sheet_add(docs_address, sheet = sheet_name, .after = "mapped_data")
  
  range_write(docs_address, sheet= sheet_name, exp3)
  
}


}
}

