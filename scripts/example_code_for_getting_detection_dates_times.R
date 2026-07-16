# code to get dates and times out of selection files

# Stephanie K. Archer 6/30/2026

#load packages----
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("Rraven")

#load data----

# this gets the start time for each selection table
fls<-data.frame(fls=list.files("odata/BB1_Fall2024"))%>%#this gives me a list of files as a data frame
  separate(fls,into=c("hydro","dt","tab","ex1","ex2","ex3"),remove = F)%>% #separates a variable into multiple
  mutate(fls.starttime=force_tz(ymd_hms(dt),tz="America/Chicago"))%>%# where I can create new variables
  select(selec.file=fls,fls.starttime)



# to get the start and end time of each selection
sel.tables<-imp_raven(path="odata/BB1_Fall2024",
                      all.data=T)%>%#this brings in all selection tables within a folder
  left_join(fls)%>%#this joins the fls dataset onto the selection tables by the selection table name
  mutate(start.time=ifelse(is.na(`Begin Time (s)`),`Begin Time (s)1`,`Begin Time (s)`),
         start.time = fls.starttime + start.time,
         end.time=start.time+`Delta Time (s)`)%>%
  select(deployment,start.time,end.time)%>%
  mutate(start.mnth=month(start.time),
         start.dy=day(start.time),
         start.hr=hour(start.time),
         end.mnth=month(end.time),
         end.dy=day(end.time),
         end.hr=hour(end.time))%>%# have to decide what to do if start and end times cross the hour or day line
  group_by(start.mnth,start.dy,start.hr)%>%
  summarize(n.dolphins=n())


# code to create a dataset of all days and hours in a deployment
strt.dt<-ymd_hms("2024-09-25 21:29:58",tz="America/Chicago")
end.dt<-ymd_hms("2024-10-09 08:00:00",tz="America/Chicago")

dep.dt<-data.frame(date.time=seq(strt.dt,end.dt,by="hour"))%>%
  mutate(start.mnth=month(date.time),
         start.dy=day(date.time),
         start.hr=hour(date.time))%>%
  select(-date.time)%>%
  left_join(sel.tables)%>%
  mutate(n.dolphins=ifelse(is.na(n.dolphins),0,n.dolphins),
         binary.dolphins=ifelse(n.dolphins==0,0,1))


         