library(jpeg)
library(tiff)
library(fs)
library(tidyverse)

#used to get filename extension. 
library(tools)



#input (and output) directory
setwd("C://Users//u1413890//OneDrive - University of Utah//garrett hl-onedrive//experiment files//")






#get df of all files in directory with corresponding file info.
#extract all useful information needed for subsequent ops. 
tif_df<-dir_info()%>%
  as_tibble()%>%
  
  #remove extraneous info
  select(path, type, modification_time)%>%
  
  #remove non-files. 
  filter(type=="file")%>%
  select(-type)%>%
  
  
  #change modification time to date. Filter for TODAY. 
  
  
  mutate(modification_time = as.Date(modification_time))%>%
  
  
  
  filter(modification_time > today()-7)%>%
  
  filter(modification_time == today())%>%
  
  
  #find file extension for each file. remove if not a tiff/tif. 
  mutate(ext = file_ext(path))%>%
  
  
 
  
  filter(ext == "tif" | ext == "tiff")%>%
  
  #create variable of fs::path for origin tif files, as character string.
  mutate(tif_path = as.character(path))%>%
  
  #take the tif input files and change the filename extensions to jpeg. 
  mutate(jpeg_path = sub(".tiff", ".jpeg", tif_path))%>%
  mutate(jpeg_path = sub(".tif", ".jpeg", tif_path))%>%
  mutate(jpeg_path = paste0("conv/", jpeg_path))
  


###############

#from created df, pull tif file names as vec. 
tif_vec<-tif_df%>%
  pull(tif_path)

#from created df, pull jped file names as vec. 
jpeg_vec<-tif_df%>%
  pull(jpeg_path)




#for loop to convert all tifs to jpegs. 
for (i in 1:length(tif_vec)){
writeJPEG(readTIFF(tif_vec[i], native=TRUE), target = jpeg_vec[i], quality = 3)
}
