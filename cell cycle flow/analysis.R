library(flowCore)
library(ggcyto)
library(tidyverse)

x<-read.FCS("fcs/Garreth_Group_bo1.fcs", transformation=FALSE, alter.names = TRUE,
            emptyValue = FALSE)




autoplot(x, "YL2.W")

autoplot(x, "YL2.H")

autoplot(x, "YL2.A")


tb<-exprs(x)%>%
  as_tibble()%>%

  #filter(SSC.W>1000)%>%
  filter(YL2.H>=10000)%>%
  mutate(YL2.Adj = YL2.H/SSC.W)


ggplot(tb, aes(YL2.W))+
  geom_density(adjust=1/10000)


ggplot(tb, aes(YL2.Adj))+
  geom_density(adjust=1/10000)


