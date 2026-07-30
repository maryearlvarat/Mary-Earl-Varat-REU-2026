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
                        # files = fls$selec.file[13],
                        all.data=T)%>%#this brings in all selection tables within a folder
    left_join(fls)#this joins the fls dataset onto the selection tables by the selection table name
  
  if(!"Begin Time (s)" %in% colnames(sel.tables))sel.tables$`Begin Time (s)`<-NA
  
  seltables<-sel.tables%>%
    mutate(start.time=ifelse(is.na(`Begin Time (s)`),`Begin Time (s)1`,`Begin Time (s)`),
           start.time = fls.starttime + start.time,
           end.time=start.time+`Delta Time (s)`)%>%
    select(deployment,start.time,end.time,delt.time=`Delta Time (s)`)%>%
    mutate(start.mnth=month(start.time),
           start.dy=day(start.time),
           start.hr=hour(start.time),
           end.mnth=month(end.time),
           end.dy=day(end.time),
           end.hr=hour(end.time))%>%# have to decide what to do if start and end times cross the hour or day line
    group_by(start.mnth,start.dy,start.hr)%>%
    summarize(n.dolphins=n(),
              detection.minutes=sum(delt.time)/60)
  # code to create a dataset of all days and hours in a deployment
  dep.dt<-data.frame(date.time=seq(strt.dt,end.dt,by="hour"))%>%
    mutate(start.mnth=month(date.time),
           start.dy=day(date.time),
           start.hr=hour(date.time))%>%
    select(-date.time)%>%
    left_join(seltables)%>%
    mutate(n.dolphins=ifelse(is.na(n.dolphins),0,n.dolphins),
           binary.dolphins=ifelse(n.dolphins==0,0,1),
           detection.minutes=ifelse(is.na(detection.minutes),0,detection.minutes))%>%
    rename(mnth=start.mnth,dy=start.dy,hr=start.hr)
  return(dep.dt)
}


#run the funtion----

#BB1 Fall 24
BB1fall24.seltables<-getseltimes(deploy="BB1_Fall2024",
                              strt.dt<-ymd_hms("2024-09-25 21:29:58",tz="America/Chicago"),
                              end.dt<-ymd_hms("2024-10-09 08:00:00",tz="America/Chicago"))

write.csv(BB1fall24.seltables,"wdata/BB1fall24_dolphins.csv",row.names = F)

# Calculate the grand total
total_minutes <- sum(BB1fall24_dolphins$detection.minutes, na.rm = TRUE)

# Print the result
print(total_minutes)

#BB1 Summer 25
BB1summer25.seltables<-getseltimes(deploy="BB1_Summer25",
                              strt.dt<-ymd_hms("2025-07-21 18:00:00",tz="America/Chicago"),
                              end.dt<-ymd_hms("2025-08-04 08:00:00",tz="America/Chicago"))
write.csv(BB1summer25.seltables,"wdata/BB1summer25_dolphins.csv",row.names = F)


# Calculate the grand total
total_minutes <- sum(BB1summer25_dolphins$detection.minutes, na.rm = TRUE)

# Print the result
print(total_minutes)

#BB1 Winter 25
BB1winter25.seltables<-getseltimes(deploy="BB1_Winter25",
                                strt.dt<-ymd_hms("2025-01-29 18:00:00",tz="America/Chicago"),                               end.dt<-ymd_hms("2025-02-17 08:00:00",tz="America/Chicago"))
write.csv(BB1winter25.seltables,"wdata/BB1winter25_dolphins.csv",row.names = F)

# Calculate the grand total
total_minutes <- sum(BB1winter25_dolphins$detection.minutes, na.rm = TRUE)

# Print the result
print(total_minutes)

#BB1 Spring 25
BB1spring25.seltables<-getseltimes(deploy="BB1_Spring25",
                                strt.dt<-ymd_hms("2025-04-14 18:00:00",tz="America/Chicago"),
                                end.dt<-ymd_hms("2025-04-28 08:00:00",tz="America/Chicago"))
write.csv(BB1spring25.seltables,"wdata/BB1spring25_dolphins.csv",row.names = F)

# Calculate the grand total
total_minutes <- sum(BB1spring25_dolphins$detection.minutes, na.rm = TRUE)

# Print the result
print(total_minutes)

#BB2 
#BB2 Fall 24
BB2fall24.seltables<-getseltimes(deploy="BB2_Fall24",
                                 strt.dt<-ymd_hms("2024-09-25 21:29:58",tz="America/Chicago"),
                                 end.dt<-ymd_hms("2024-10-09 08:00:00",tz="America/Chicago"))

write.csv(BB2fall24.seltables,"wdata/BB2fall24_dolphins.csv",row.names = F)

# Calculate the grand total
total_minutes <- sum(BB2fall24_dolphins$detection.minutes, na.rm = TRUE)

# Print the result
print(total_minutes)

#BB2 Summer 25
BB2summer25.seltables<-getseltimes(deploy="BB2_Summer25",
                                   strt.dt<-ymd_hms("2025-07-22 18:00:00",tz="America/Chicago"),
                                   end.dt<-ymd_hms("2025-08-04 08:00:00",tz="America/Chicago"))
write.csv(BB2summer25.seltables,"wdata/BB2summer25_dolphins.csv",row.names = F)
# Calculate the grand total
total_minutes <- sum(BB2summer25_dolphins$detection.minutes, na.rm = TRUE)

# Print the result
print(total_minutes)

#BB2 Winter 25
BB2winter25.seltables<-getseltimes(deploy="BB2_Winter25",
                                   strt.dt<-ymd_hms("2025-01-29 18:00:00",tz="America/Chicago"),
                                   end.dt<-ymd_hms("2025-02-17 08:00:00",tz="America/Chicago"))
write.csv(BB2winter25.seltables,"wdata/BB2winter25_dolphins.csv",row.names = F)
# Calculate the grand total
total_minutes <- sum(BB2winter25_dolphins$detection.minutes, na.rm = TRUE)
# Print the result
print(total_minutes)

#BB2 Spring 25
BB2spring25.seltables<-getseltimes(deploy="BB2_Spring25",
                                   strt.dt<-ymd_hms("2025-04-14 18:00:00",tz="America/Chicago"),
                                   end.dt<-ymd_hms("2025-04-28 08:00:00",tz="America/Chicago"))
write.csv(BB2spring25.seltables,"wdata/BB2spring25_dolphins.csv",row.names = F)

# Calculate the grand total
total_minutes <- sum(BB2spring25_dolphins$detection.minutes, na.rm = TRUE)
# Print the result
print(total_minutes)


#LUMO6 
#LUMO6 summer 25
LUMO6summer25.seltables<-getseltimes(deploy="LUMO6_summer25",
                                   strt.dt<-ymd_hms("2025-07-28 18:00:00",tz="America/Chicago"),
                                   end.dt<-ymd_hms("2025-08-11 08:00:00",tz="America/Chicago"))
write.csv(LUMO6summer25.seltables,"wdata/LUMO6summer25_dolphins.csv",row.names = F)
# Calculate the grand total
total_minutes <- sum(LUMO6summer25_dolphins$detection.minutes, na.rm = TRUE)
# Print the result
print(total_minutes)


#LUMO6 winter 25 
LUMO6winter25.seltables<-getseltimes(deploy="LUMO6_winter25",
                                     strt.dt<-ymd_hms("2025-01-27 18:00:00",tz="America/Chicago"),
                                     end.dt<-ymd_hms("2025-02-10 08:00:00",tz="America/Chicago"))
write.csv(LUMO6winter25.seltables,"wdata/LUMO6winter25_dolphins.csv",row.names = F)
# Calculate the grand total
total_minutes <- sum(LUMO6winter25_dolphins$detection.minutes, na.rm = TRUE)
# Print the result
print(total_minutes)

#LUMO6 fall 24 
LUMO6fall24.seltables<-getseltimes(deploy="LUMO6_fall24",
                                     strt.dt<-ymd_hms("2024-09-23 18:00:00",tz="America/Chicago"),
                                     end.dt<-ymd_hms("2024-10-07 08:00:00",tz="America/Chicago"))
write.csv(LUMO6fall24.seltables,"wdata/LUMO6fall24_dolphins.csv",row.names = F)
# Calculate the grand total
total_minutes <- sum(LUMO6fall24_dolphins$detection.minutes, na.rm = TRUE)
# Print the result
print(total_minutes)


#LUMO6 Spring 25
LUMO6spring25.seltables<-getseltimes(deploy="LUMO6_spring25",
                                   strt.dt<-ymd_hms("2024-04-01 18:00:00",tz="America/Chicago"),
                                   end.dt<-ymd_hms("2024-04-16 08:00:00",tz="America/Chicago"))
write.csv(LUMO6spring25.seltables,"wdata/LUMO6spring25_dolphins.csv",row.names = F)
# Calculate the grand total
total_minutes <- sum(LUMO6spring25_dolphins$detection.minutes, na.rm = TRUE)
# Print the result
print(total_minutes)

#TB1
#TB1 Summer 25
TB1summer25.seltables<-getseltimes(deploy="TB1_summer25",
                                     strt.dt<-ymd_hms("2024-07-28 18:00:00",tz="America/Chicago"),
                                     end.dt<-ymd_hms("2024-08-11 08:00:00",tz="America/Chicago"))
write.csv(TB1summer25.seltables,"wdata/TB1summer25_dolphins.csv",row.names = F)
# Calculate the grand total
total_minutes <- sum(TB1summer25_dolphins$detection.minutes, na.rm = TRUE)
# Print the result
print(total_minutes)

#TB1 Winter 25
TB1winter25.seltables<-getseltimes(deploy="TB1_winter25",
                                   strt.dt<-ymd_hms("2024-01-27 18:00:00",tz="America/Chicago"),
                                   end.dt<-ymd_hms("2024-02-10 08:00:00",tz="America/Chicago"))
write.csv(TB1winter25.seltables,"wdata/TB1winter25_dolphins.csv",row.names = F)
# Calculate the grand total
total_minutes <- sum(TB1winter25_dolphins$detection.minutes, na.rm = TRUE)
# Print the result
print(total_minutes)
