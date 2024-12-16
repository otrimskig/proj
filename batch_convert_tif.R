library(jpeg)
library(tiff)
library(fs)
library(tidyverse)
#used to get filename extension. 
library(tools)

gsheet<-"https://docs.google.com/spreadsheets/d/1ZFF_V6eAI6gDJsrHpkpmPIZuc2BLushdqBbf0SqbpQw/edit#gid=0"

#input (and output) directory
setwd("C:/Users/u1413890/OneDrive - University of Utah/garrett hl-onedrive/experiment files/input tifs")

library(googlesheets4)

gs4_auth(email = "gotrimski@gmail.com")






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
  
  #filter(modification_time == today())%>%
  
  
  #find file extension for each file. remove if not a tiff/tif. 
  mutate(ext = file_ext(path))%>%
  
  
 
  
  filter(ext == "tif" | ext == "tiff")%>%
  
  #create variable of fs::path for origin tif files, as character string.
  mutate(tif_path = as.character(path))%>%
  
  
  ##
  ##
  #cleaning up file names to rename them in consistent manner. 
  #edit this if file naming scheme changes. 
  ##
  ##
  mutate(new_path= sub("^23[0-9]{4}","", tif_path))%>%
  mutate(new_path= sub("^2023[0-9]{4}","", new_path))%>%
  mutate(new_path= paste0(as.character(modification_time), new_path))



tif_df%>%
  rename(path1=path)%>%
  select(path1)%>%
  range_write(gsheet,
              sheet = "Sheet1",
              .,
              reformat=FALSE,
              range = "A1")


stop("edit names in sheet https://docs.google.com/spreadsheets/d/1ZFF_V6eAI6gDJsrHpkpmPIZuc2BLushdqBbf0SqbpQw/edit#gid=0")




sheets_df<-read_sheet(gsheet, 
                      sheet = "Sheet1")%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()





file_move(sheets_df$path1, sheets_df$path2)


df2<-sheets_df%>%
  
  #take the tif input files and change the filename extensions to jpeg. 
  mutate(jpeg_path = sub(".tiff", ".jpeg", path2))%>%
  mutate(jpeg_path = sub(".tif", ".jpeg", path2))
  


###############

#from created df, pull tif file names as vec. 
tif_vec<-df2%>%
  pull(path2)

#from created df, pull jped file names as vec. 
jpeg_vec<-df2%>%
  pull(jpeg_path)




#for loop to convert all tifs to jpegs. 
for (i in 1:length(tif_vec)){
writeJPEG(readTIFF(tif_vec[i], native=TRUE), target = jpeg_vec[i], quality = 10)
}



move2<-tibble(p1=list.files())%>%
  mutate(p2=paste0("../", p1))

file_move(move2$p1, move2$p2)

