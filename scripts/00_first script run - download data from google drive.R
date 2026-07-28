#This is the start of my project code


library(tidyverse)
source("scripts/install_packages_function.R")


lp("googledrive")
lp("tidyverse")
#BB1 Fall24----
fid<-"https://drive.google.com/drive/folders/17jMX-KMDLhsykFaiaY5jqNiq9mAxknDI"
folder_id = drive_get(as_id(fid))
files =drive_ls(folder_id)

dep<-"BB1_Fall2024"
dir.create(paste0( "odata/",dep))
for(i in 1:nrow(files)){
  drive_download(file = files$id[i],
               path = paste0("odata/",dep,"/",files$name[i]),
               overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on
}
#chnage FID and DEP to match what data set Im downloading
#BB1 WInter 25----
fid<-"https://drive.google.com/drive/folders/1ITxFkvxSvwwCID_oBsAuwf2no6EDJMdL"
folder_id = drive_get(as_id(fid))
files =drive_ls(folder_id)

dep<-"BB1_Winter25"
dir.create(paste0( "odata/",dep))
for(i in 1:nrow(files)){
  drive_download(file = files$id[i],
                 path = paste0("odata/",dep,"/",files$name[i]),
                 overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on
}
#BB1 Summer 25
fid<-"https://drive.google.com/drive/u/0/folders/1zivBEXsQIVpmXplkb1nRVQ6vRkss7YjV"
folder_id = drive_get(as_id(fid))
files =drive_ls(folder_id)

dep<-"BB1_Summer25"
dir.create(paste0( "odata/",dep))
for(i in 1:nrow(files)){
  drive_download(file = files$id[i],
                 path = paste0("odata/",dep,"/",files$name[i]),
                 overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on
}
#BB1 Spring 25
fid<-"https://drive.google.com/drive/folders/1p_UJiXMv5u_cSL9V-tcqvwIKI-jAgAHw"
folder_id = drive_get(as_id(fid))
files =drive_ls(folder_id)

dep<-"BB1_Spring25"
dir.create(paste0( "odata/",dep))
for(i in 1:nrow(files)){
  drive_download(file = files$id[i],
                 path = paste0("odata/",dep,"/",files$name[i]),
                 overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on
}
#BB2 Fall 24
  fid<-"https://drive.google.com/drive/folders/1NKVinEbOo6T5Absrd6tnTWNv3KyZfbKL"
  folder_id = drive_get(as_id(fid))
  files =drive_ls(folder_id)
  
  dep<-"BB2_Fall24"
  dir.create(paste0( "odata/",dep))
  for(i in 1:nrow(files)){
    drive_download(file = files$id[i],
                   path = paste0("odata/",dep,"/",files$name[i]),
                   overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on 
}

#BB2 Winter 25
  fid<-"https://drive.google.com/drive/folders/1-JpPi8v4F4R8kL6Of8LvpoVqaVplEf82"
  folder_id = drive_get(as_id(fid))
  files =drive_ls(folder_id)
  
  dep<-"BB2_Winter25"
  dir.create(paste0( "odata/",dep))
  for(i in 1:nrow(files)){
    drive_download(file = files$id[i],
                   path = paste0("odata/",dep,"/",files$name[i]),
                   overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on 
  } 
#BB2 Spring 25
  fid<-"https://drive.google.com/drive/folders/1JoyAu7c9sID2ORYUAmdYOlta33y7Ev7J"
  folder_id = drive_get(as_id(fid))
  files =drive_ls(folder_id)
  
  dep<-"BB2_Spring25"
  dir.create(paste0( "odata/",dep))
  for(i in 1:nrow(files)){
    drive_download(file = files$id[i],
                   path = paste0("odata/",dep,"/",files$name[i]),
                   overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on 
  }   
#BB2 Summer25
  fid<-"https://drive.google.com/drive/folders/1oM4yf2khWQ3_y_RhAvLK0eE5hpXVdNXu"
  folder_id = drive_get(as_id(fid))
  files =drive_ls(folder_id)
  
  dep<-"BB2_Summer25"
  dir.create(paste0( "odata/",dep))
  for(i in 1:nrow(files)){
    drive_download(file = files$id[i],
                   path = paste0("odata/",dep,"/",files$name[i]),
                   overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on 
  }    
  #LUMO6 Summer 25
  fid<-"https://drive.google.com/drive/folders/1RQiMRwepwxjn1jDPQsr00pwUqlDadjS1"
  folder_id = drive_get(as_id(fid))
  files =drive_ls(folder_id)
  
  dep<-"LUMO6_Summer25"
  dir.create(paste0( "odata/",dep))
  for(i in 1:nrow(files)){
    drive_download(file = files$id[i],
                   path = paste0("odata/",dep,"/",files$name[i]),
                   overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on 
  } 
  #LUMO6 Winter 25
  fid<-"https://drive.google.com/drive/folders/1F-JK2uTXP5MNOSoePECfdivBKZUlkVS1"
  folder_id = drive_get(as_id(fid))
  files =drive_ls(folder_id)
  
  dep<-"LUMO6_Winter25"
  dir.create(paste0( "odata/",dep))
  for(i in 1:nrow(files)){
    drive_download(file = files$id[i],
                   path = paste0("odata/",dep,"/",files$name[i]),
                   overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on 
  } 
  