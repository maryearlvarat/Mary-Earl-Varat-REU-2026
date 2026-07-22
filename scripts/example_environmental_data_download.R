# example script for pulling environmental data

# run this the first time you install the package - after that you can skip lines 4-7
library(remotes)

install_github("DOI-USGS/dataRetrieval",
               ref = "develop") # this is the development version, which may be buggy (could use "main" instead)

# load packages----
library("dataRetrieval")
library("tidyverse")

# set parameters for downloads
bb2site<-"USGS-073802512"
bb1site<-"USGS-07380249"
tb1site<-"USGS-073813498"
params=c("00480","00010","00300")# this is salinity (PPT), temperature (degrees C), and dissolved oxygen (mg/L)

# download data
download.envdata<-function(start.date=NA,end.date=NA,site=NA){
  env.data<-read_waterdata_continuous(monitoring_location_id = site,#change to match site
                                      parameter_code = params,
                                      time=c(start.date,
                                             end.date))
  # if you get a curl error -- update the curl and httr packages!
  # if you get a cli error -- update the cli package
  # can do updates when installing dataRetrieval package
  
  # organize data - note you should QAQC data before proceeding with this summarization step
  env.data2<-env.data%>%
    mutate(param=case_when(
      parameter_code=="00480"~"sal.ppt",
      parameter_code=="00010"~"temp.c",
      parameter_code=="00300"~"do.mg.l"),#creates a variable that tells you what parameter is recorded
      yr=year(time),
      mnth=month(time),
      dy=day(time),
      hr=hour(time))%>%
    group_by(yr,mnth,dy,hr,param)%>%
    summarize(value=median(value,na.rm=T))%>% # taking the median measurement over each hour
    pivot_wider(names_from = param,values_from = value,values_fill = NA)
  
  
  return(env.data2)
}

# make sure all start times are 00:00:00 and all end times are 23:59:59 for environmental data

# trying function with bb1 and fall 24 dates
bb1fall24.env<-download.envdata(start.date=as.POSIXct("2024-09-25 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                             end.date=as.POSIXct("2024-10-09 23:59:59",tz="America/Chicago"),
                             site=bb1site)#change to match the end date/time of the deployment
# save the dataset
write.csv(bb1fall24.env,"wdata/bb1fall24_env.csv",row.names=F)                            

# Winter 25
bb1winter25.env<-download.envdata(start.date=as.POSIXct("2025-01-29 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                             end.date=as.POSIXct("2025-02-17 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                             site=bb1site)

# save the dataset
write.csv(bb1winter25.env,"wdata/bb1winter25_env.csv",row.names=F)

# BB1 Spring 25
bb1spring25.env<-download.envdata(start.date=as.POSIXct("2025-04-14 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                               end.date=as.POSIXct("2025-04-28 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                               site=bb1site)
# save the dataset
write.csv(bb1spring25.env,"wdata/bb1spring25_env.csv",row.names=F)

# BB1 Summer 25
bb1summer25.env<-download.envdata(start.date=as.POSIXct("2025-07-21 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                               end.date=as.POSIXct("2025-08-04 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                               site=bb1site)
# save the dataset
write.csv(bb1summer25.env,"wdata/bb1summer25_env.csv",row.names=F)

#BB2 Fall 24
bb2fall24.env<-download.envdata(start.date=as.POSIXct("2024-09-25 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                               end.date=as.POSIXct("2024-10-09 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                               site=bb2site)
# save the dataset
write.csv(bb2fall24.env,"wdata/bb2fall24_env.csv",row.names=F)

#BB2 Winter 25
bb2winter25.env<-download.envdata(start.date=as.POSIXct("2025-01-29 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2025-02-17 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=bb2site)
# save the dataset
write.csv(bb2winter25.env,"wdata/bb2winter25_env.csv",row.names=F)

#BB2 Spring 25
bb2spring25.env<-download.envdata(start.date=as.POSIXct("2025-04-14 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2025-04-28 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=bb2site)
# save the dataset
write.csv(bb2spring25.env,"wdata/bb2spring25_env.csv",row.names=F)

#BB2 Summer 25
bb2summer25.env<-download.envdata(start.date=as.POSIXct("2025-07-22 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2025-08-04 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=bb2site)
# save the dataset
write.csv(bb2summer25.env,"wdata/bb2summer25_env.csv",row.names=F)

#TB1 Fall 24
tb1fall24.env<-download.envdata(start.date=as.POSIXct("2024-09-23 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2024-10-11 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=tb1site)
# save the dataset
write.csv(tb1fall24.env,"wdata/tb1fall24_env.csv",row.names=F)

#TB1 Winter 25
tb1winter25.env<-download.envdata(start.date=as.POSIXct("2025-01-27 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2025-02-10 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=tb1site)
# save the dataset
write.csv(tb1winter25.env,"wdata/tb1winter25_env.csv",row.names=F)

#TB1 Spring 25
tb1spring25.env<-download.envdata(start.date=as.POSIXct("2025-04-09 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2025-04-23 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=tb1site)
# save the dataset
write.csv(tb1spring25.env,"wdata/tb1spring25_env.csv",row.names=F)

#TB1 Summer 25
tb1summer25.env<-download.envdata(start.date=as.POSIXct("2025-07-28 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2025-08-11 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=tb1site)
# save the dataset
write.csv(tb1summer25.env,"wdata/tb1summer25_env.csv",row.names=F)
