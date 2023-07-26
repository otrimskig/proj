
attributes(pairwise_results[["p.value"]])[["dimnames"]][[1]]->group_a



pairwise_results[["p.value"]]%>%
  as_tibble()%>%
  
  mutate(group_a = group_a)%>%
  relocate(group_a)%>%
  
  
  
  pivot_longer(cols=2:7, names_to = "group_b", values_to = "p_value")%>%
  filter(!is.na(p_value))%>%
  
  
  mutate(group_c = group_a)->table


table%>%
  select(group_a, group_b, p_value)->tableab

table%>%
  select(group_b, group_c, p_value)%>%
  rename(group_a=group_b, group_b=group_c)->tablebc

full_join(tableab, tablebc)%>%
  
  
  
  pivot_wider(names_from = "group_b", values_from = "p_value")->all_ps





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
