# Code to add in marine center env data 

source("scripts/install_packages_function.R")
lp("tidyverse")
library("readxl")
lp("googledrive")

fid<-"https://drive.google.com/drive/u/0/folders/1A32PGi8P4YOSe1Vz8WFNU-Xai27kOvKU"

folder_id = drive_get(as_id(fid))
files =drive_ls(folder_id)
files<-files[grep(files$name,pattern="Mary"),]

for(i in 1:nrow(files)){
  drive_download(file = files$id[i],
                 path = paste0("odata/",files$name[i]),
                 overwrite = TRUE) #drive download, file downloaded is whatever number of the loop we are on
}

# download the data

# create a dataset that is the start and end times of the deployments at LUMO6

# bring in deployment times
deps<-read_xlsx(path="odata/Mary deployment times.xlsx")%>%
  mutate(start=ymd_hms(paste(date.start.record,"18:00:00")),
         end=ymd_hms(paste(date.retrieved,"08:00:00")))

# code to find intervals


#Read in Marine Sal and Temp data
mcsalandtemp<-read_xlsx(path="odata/Mary_MC_SAL_WT.xlsx",skip=1)
summary(mcsalandtemp)
head(mcsalandtemp)

#changing column names 

mcsat<-mcsalandtemp%>%
  mutate(yr=year(UTC),
         mnth=month(UTC),
         dy=day(UTC),
         hr=hour(UTC),
         min=minute(UTC))%>%
  filter(min==0)%>%
  select(-min)%>%
  rename(
    temp.c = C,
    sal.ppt = PSU)

# add in intervals
mcsat$deploy<-NA
mcsat$deploy[mcsat$UTC %within% interval(deps$start[1],deps$end[1])]<-deps$Folder[1]
mcsat$deploy[mcsat$UTC %within% interval(deps$start[2],deps$end[2])]<-deps$Folder[2]
mcsat$deploy[mcsat$UTC %within% interval(deps$start[3],deps$end[3])]<-deps$Folder[3]
mcsat$deploy[mcsat$UTC %within% interval(deps$start[4],deps$end[4])]<-deps$Folder[4]

# check that this worked
table(mcsat$deploy)

mcsat2<-filter(mcsat,!is.na(deploy))


#DO MC Data - dont run any of this 
#Read in Marine Sal and Temp data
mcdo<-read_xlsx(path="odata/Mary_MC_DO.xlsx",skip=1)
summary(mcdo)
head(mcdo)

mcdo2<-mcdo%>%
  mutate(mnth=month(TS),
         dy=day(TS),
         hr=hour(TS),
         min=minute(TS))%>%
  filter(min==0)%>%
  select(-min,-...3,-...4,-...5,-...6)%>%
  rename(
    do.mg.l = "mg/L")%>%
  distinct()


mcdo2$deploy<-NA
mcdo2$deploy[mcdo2$TS %within% interval(deps$start[1],deps$end[1])]<-deps$Folder[1]
mcdo2$deploy[mcdo2$TS %within% interval(deps$start[2],deps$end[2])]<-deps$Folder[2]
mcdo2$deploy[mcdo2$TS %within% interval(deps$start[3],deps$end[3])]<-deps$Folder[3]
mcdo2$deploy[mcdo2$TS %within% interval(deps$start[4],deps$end[4])]<-deps$Folder[4]

table(mcdo2$deploy)

mcdo3<-filter(mcdo2,!is.na(deploy))

#join DO with SALTEMp


lumo6_envi<-left_join(mcdo3%>%select(-TS),mcsat2%>%select(-UTC))

write.csv(lumo6_envi,"wdata/lumo6_env.csv",row.names = F)


