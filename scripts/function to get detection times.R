# code to get dates and times out of selection files

# Stephanie K. Archer 6/30/2026

#load packages----

#create the funtion----

getseltimes<-function(deploy=NA,strt.dt=NA,end.dt=NA){
  source("scripts/install_packages_function.R")
  lp("tidyverse")
  lp("Rraven")
  
  #load data----
  
  
  
  # this gets the start time for each selection table
  fls<-data.frame(fls=list.files(paste0("odata/",deploy)))%>%#this gives me a list of files as a data frame
    separate(fls,into=c("hydro","dt","tab","ex1","ex2","ex3"),remove = F)%>% #separates a variable into multiple
    mutate(fls.starttime=force_tz(ymd_hms(dt),tz="America/Chicago"))%>%# where I can create new variables
    select(selec.file=fls,fls.starttime)
  
  
  
  # to get the start and end time of each selection
  sel.tables<-imp_raven(path=paste0("odata/",deploy),
                        all.data=T)%>%#this brings in all selection tables within a folder
    left_join(fls)#this joins the fls dataset onto the selection tables by the selection table name
  
  if(!"Begin Time (s)" %in% colnames(sel.tables))sel.tables$`Begin Time (s)`<-NA
  
  seltables<-sel.tables%>%
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
  dep.dt<-data.frame(date.time=seq(strt.dt,end.dt,by="hour"))%>%
    mutate(start.mnth=month(date.time),
           start.dy=day(date.time),
           start.hr=hour(date.time))%>%
    select(-date.time)%>%
    left_join(seltables)%>%
    mutate(n.dolphins=ifelse(is.na(n.dolphins),0,n.dolphins),
           binary.dolphins=ifelse(n.dolphins==0,0,1))%>%
    rename(mnth=start.mnth,dy=start.dy,hr=start.hr)
  return(dep.dt)
}


#run the funtion----

#BB1 Fall 24
fall24.seltables<-getseltimes(deploy="BB1_Fall2024",
                              strt.dt<-ymd_hms("2024-09-25 21:29:58",tz="America/Chicago"),
                              end.dt<-ymd_hms("2024-10-09 08:00:00",tz="America/Chicago"))

write.csv(fall24.seltables,"wdata/fall24_dolphins.csv",row.names = F)

#BB1 Summer 25
summer25.seltables<-getseltimes(deploy="BB1_Summer25",
                              strt.dt<-ymd_hms("2025-07-21 18:00:00",tz="America/Chicago"),
                              end.dt<-ymd_hms("2025-08-04 08:00:00",tz="America/Chicago"))
write.csv(summer25.seltables,"wdata/summer25_dolphins.csv",row.names = F)
#BB1 Winter 25
winter25.seltables<-getseltimes(deploy="BB1_Winter25",
                                strt.dt<-ymd_hms("2025-01-29 18:00:00",tz="America/Chicago"),
                                end.dt<-ymd_hms("2025-02-17 08:00:00",tz="America/Chicago"))
write.csv(winter25.seltables,"wdata/winter25_dolphins.csv",row.names = F)

#BB1 Spring 25
spring25.seltables<-getseltimes(deploy="BB1_Spring25",
                                strt.dt<-ymd_hms("2025-04-14 18:00:00",tz="America/Chicago"),
                                end.dt<-ymd_hms("2025-04-28 08:00:00",tz="America/Chicago"))
write.csv(spring25.seltables,"wdata/spring25_dolphins.csv",row.names = F)
