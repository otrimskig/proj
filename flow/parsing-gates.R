library(flowWorkspace)
library(CytoML)

fcs_files<-list.files("fcs/", full.names = TRUE)

fcs_files[1]


cf <- load_cytoframe_from_fcs(fcs_files[1], num_threads = 4)


cf


ws <- open_diva_xml('exp/10-4-23 Garrett single cell sort GFP and RFP.xml')


diva_get_sample_groups(ws)

gs <- diva_to_gatingset(ws, subset = c(1))


sampleNames(gs)
gs_get_pop_paths(gs)

plot(gs, bool = TRUE)

gs_get_pop_paths(gs, path = 2)
gs_get_pop_paths(gs, path = "full")
nodelist <- gs_get_pop_paths(gs, path = "auto")
nodelist
node <- nodelist[3]
g <- gs_pop_get_gate(gs, node)
g
gs_pop_get_stats(gs)[1:10,]


library(ggcyto)
autoplot(gs[[1]], bins=300)



autoplot(gs, nodelist[5], bins=64, fill="black")+
  theme_bw()






