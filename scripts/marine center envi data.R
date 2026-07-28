# Code to add in marine center env data 

source("scripts/install_packages_function.R")
lp("tidyverse")
library("readxl")
lp("googledrive")

fid<-"https://drive.google.com/drive/u/0/folders/1A32PGi8P4YOSe1Vz8WFNU-Xai27kOvKU"

folder_id = drive_get(as_id(fid))
2
files =drive_ls(folder_id)
files2<-bind_rows(files[grep(files$name,pattern="Mary"),],
                  files[grep(files$name,pattern="MC"),])

for(i in 1:nrow(files2)){
  drive_download(file = files2$id[i],
                 path = paste0("odata/",files2$name[i]),
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

# OLD DATA----
# Marine Center Station----
# read in data files
# each variable is its own file
do<-read_xlsx(path="odata/MC_DO_2008-2024.xlsx", sheet=1, skip=1)
sal<-read_xlsx(path="odata/MC-Salinity 2000-2024.xlsx", sheet=1, skip=1)
tempC<-read_xlsx(path="odata/MC-WaterTemp_2000-2024.xlsx", sheet=1)

# DO
# look at structure of DO data
head(do)
summary(do)
# Okay, now take out the problem columns
# rename columns so they align with USGS data
do2<-do|>
  #mutate(
  #...1 = NULL,
  #...2 = NULL,
  #'%' = NULL)|>
  select(UTC, 'mg L')|>
  rename(
    time = UTC,
    do.mg.l = 'mg L')
#check structure
head(do2)
summary(do2)

# try salinity
head(sal)
summary(sal)

# rename ppt to sal.ppt
sal2=sal |>
  rename(
    time = UTC,
    sal.ppt = ppt)
head(sal2)

# now temp
head(tempC)
summary(tempC)

# need to clean columns
temp2<-tempC |>
  #mutate(
  #...1 = NULL,
  #...2 = NULL,
  #'Water Temp °F' = NULL)|>
  select('Date/Time', Temp)|>
  rename(
    time = 'Date/Time',
    temp.c = Temp)
# check
head(temp2)
summary(temp2)

# can join all three together
# will only join 2008 on, since that's when DO data starts...
# temp and salinity start in 2000
# (do a left join)

# something is off with the times...?! how to figure that out?
# are there duplicate times?
test_do2 <- distinct(do2, time, .keep_all=TRUE) # cuts 4500 rows
# try join again?
LUMO_dosal <- left_join(test_do2, sal2)
# no errors, but somehow there are more rows in the output (496806)
# than in do2 (496798) or test_do2 (492256) ...???
test_sal2 <- distinct(sal2, time, .keep_all=TRUE) # cuts 6000+ rows
# sal2 has 747110, test_sal2 has 741867
LUMO_dosal <- left_join(test_do2, test_sal2)
# NOW its got 492256 rows, same as test_do2!
# ...why are there so many duplicate rows? does it matter which dupe you keep?
# join temp
test_temp2 <- distinct(temp2, time, .keep_all=TRUE)
# drops from 747855 rows to 743123 rows (cuts ~4500 rows)
LUMO_envdata <- left_join(LUMO_dosal, test_temp2)%>%
  mutate(date=ymd(paste(year(time),month(time),day(time))),
         hr=hour(time))%>%
  group_by(date,hr)%>%
  summarize(do.mg.l=median(do.mg.l,na.rm=T),
            sal.ppt=median(sal.ppt,na.rm=T),
            temp.c=median(temp.c,na.rm=T))%>%
  mutate(TS=ymd_h(paste(date,hr)))

deps2<-deps
year(deps2$start)<-c(2017,rep(2018,3))
year(deps2$end)<-c(2017,rep(2018,3))

LUMO_envdata$deploy<-NA
LUMO_envdata$deploy[LUMO_envdata$TS %within% interval(deps2$start[1],deps2$end[1])]<-deps2$Folder[1]
LUMO_envdata$deploy[LUMO_envdata$TS %within% interval(deps2$start[2],deps2$end[2])]<-deps2$Folder[2]
LUMO_envdata$deploy[LUMO_envdata$TS %within% interval(deps2$start[3],deps2$end[3])]<-deps2$Folder[3]
LUMO_envdata$deploy[LUMO_envdata$TS %within% interval(deps2$start[4],deps2$end[4])]<-deps2$Folder[4]


LUMO_envdata<-LUMO_envdata|>
  filter(!is.na(deploy))
#492256 rows, same as test_do2
head(LUMO_envdata) # all 3 variables are there!

# write the data
write.csv(LUMO_envdata,"wdata/lumo6_old_env.csv",row.names = F)

# Terrebonne Bay Station----
# just need DO
# DO is one sheet in the workbook; each variable has its own sheet in a single workbook
tbdo<-read_xlsx(path="odata/Terrebonne Bay_ 2008-2020.xlsx", sheet=2)
head(tbdo)

# clean up! pull out relevant columns; rename time and DO variables
tbdo2<-tbdo |>
  select(UTC, 'DO mg/L')|>
  rename(
    time = UTC,
    do.mg.l = 'DO mg/L')
# check
head(tbdo2)
summary(tbdo2)

# join DO with existing env data from other script..


# write the data
write.csv(lumo6_envi,"wdata/lumo6_env.csv",row.names = F)

