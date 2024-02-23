library(tidyverse)

library(readxl)
df1 <- read_excel("FAK In Vivo Brain Mets.xlsx", 
                                     sheet = "Sheet2")

df2<-df1%>%
  janitor::clean_names()%>%
  rename(mouse_num=x1)%>%
  pivot_longer(2:last_col(), values_to = "tumor", names_to = "group")%>%
  drop_na()


sumt<-df2%>%
  group_by(group)%>%
  summarise(tumor_prop = mean(tumor), n=n())%>%
  mutate(counts=tumor_prop*n)


combo<-sumt%>%
  mutate(group_a=group)%>%
  mutate(group_b=group)%>%
  expand(group_a, group_b)%>%
  
  left_join(sumt, by=c("group_a"="group"))%>%
  rename(tumor_a=tumor_prop, n_a=n)%>%
  relocate(group_b, .after = last_col())%>%
  
  left_join(sumt, by=c("group_b"="group"))%>%
  rename(tumor_b=tumor_prop, n_b=n)



test<-prop.test(x=sumt$counts[2:3], n=sumt$n[2:3])

test$statistic          
test$p.value

unname(test$statistic)


row_nums<-tibble(a=1:5, b=1:5)%>%
  expand(a,b)%>%
  filter(a<=b)%>%
  filter(a!=b)







results<-matrix(nrow=0, ncol = 8)



for (i in 1:nrow(row_nums)){
  
  #function to set subset of data. 
  d<-row_nums[i,1:2]
  
  #actually subset data. 
  e<-sumt%>%slice(d$a, d$b)
  
  
  #perform stats test
  z<-prop.test(x=e$counts,
               n=e$n,
               alternative = "greater")
  
  
  chi_squared_val<-unname(z$statistic)
  
  p_value<-z$p.value
  
  
 results<-rbind(results, 
        c(group_a=e[1,1:3], 
          group_b=e[2,1:3],
          x_sq=chi_squared_val,
          p_val=p_value))
  
}

output<-results%>%
  as_tibble()%>%
  unnest()%>%
  mutate_all(~ifelse(is.nan(.), NA, .))



str(output)








#fisher's exact
freq_table<-table(group=df2$group, df2$tumor)%>%
 as.data.frame()%>%
  as_tibble()%>%
  rename(has_tumor = Var2)%>%
  pivot_wider(names_from = "has_tumor", values_from = "Freq")%>%
  
  rename(no_tumor = 2, tumor=3)





row_nums<-tibble(a=1:5, b=1:5)%>%
  expand(a,b)%>%
  filter(a<=b)%>%
  filter(a!=b)



results2<-tibble(group_a=NULL, group_b=NULL, p_val=NULL)


for (i in 1:nrow(row_nums)){
#function to set subset of data. 
d<-row_nums[i,1:2]

#actually subset data. 

#get subsetted tibble.
t<-freq_table%>%slice(d$a, d$b)


#convert to matrix.
e<-data.matrix(t)

#add rownames to matrix, using tibble. 
rownames(e)<-t$group

#remove extraneous "group" column, as rownames now contain. 
f<-e[,-1]



#perform stats test
z<-fisher.test(f)


p_value<-z$p.value


each_result<-tibble(group_a = as.character(unlist(t[1,1])),
       group_b = as.character(unlist(t[2,1])),
       p_val=p_value)




results2<-bind_rows(results2, each_result)

}

results2



output2<-results2%>%
  rename(fisher_exact=p_val)

output2




final<-output%>%
  rename(x_sq_p_val=p_val)%>%
  left_join(output2, by=c("group_a.group"="group_a", "group_b.group"="group_b"))
  
write_csv(final, "final.csv")
