# example script for pulling environmental data

# run this the first time you install the package - after that you can skip lines 4-7
library(remotes)

# install_github("DOI-USGS/dataRetrieval",
#                ref = "develop") # this is the development version, which may be buggy (could use "main" instead)

# load packages----
library("dataRetrieval")
library("tidyverse")

# set parameters for downloads
BB2site<-"USGS-073802512"
BB1site<-"USGS-07380249"
TB1site<-"USGS-073813498"
BB1site2<-"USGS-073802516"
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

# trying function with BB1 and fall 24 dates
BB1fall24.env<-download.envdata(start.date=as.POSIXct("2024-09-25 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                             end.date=as.POSIXct("2024-10-09 23:59:59",tz="America/Chicago"),
                             site=BB1site)#change to match the end date/time of the deployment
# save the dataset
write.csv(BB1fall24.env,"wdata/BB1fall24_env.csv",row.names=F)                            

# Winter 25
BB1winter25.env<-download.envdata(start.date=as.POSIXct("2025-01-29 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                             end.date=as.POSIXct("2025-02-17 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                             site=BB1site)

# save the dataset
write.csv(BB1winter25.env,"wdata/BB1winter25_env.csv",row.names=F)

# BB1 Spring 25
BB1spring25.env<-download.envdata(start.date=as.POSIXct("2025-04-14 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                               end.date=as.POSIXct("2025-04-28 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                               site=BB1site)
# save the dataset
write.csv(BB1spring25.env,"wdata/BB1spring25_env.csv",row.names=F)

# BB1 Summer 25
BB1summer25.env<-download.envdata(start.date=as.POSIXct("2025-07-21 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                               end.date=as.POSIXct("2025-08-04 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                               site=BB1site)
# save the dataset
write.csv(BB1summer25.env,"wdata/BB1summer25_env.csv",row.names=F)

#BB2 Fall 24
BB2fall24.env<-download.envdata(start.date=as.POSIXct("2024-09-25 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                               end.date=as.POSIXct("2024-10-09 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                               site=BB2site)
# save the dataset
write.csv(BB2fall24.env,"wdata/BB2fall24_env.csv",row.names=F)

#BB2 Winter 25
BB2winter25.env<-download.envdata(start.date=as.POSIXct("2025-01-29 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2025-02-17 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=BB2site)
# save the dataset
write.csv(BB2winter25.env,"wdata/BB2winter25_env.csv",row.names=F)

#BB2 Spring 25
BB2spring25.env<-download.envdata(start.date=as.POSIXct("2025-04-14 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2025-04-28 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB2site)
# save the dataset
write.csv(BB2spring25.env,"wdata/BB2spring25_env.csv",row.names=F)

#BB2 Summer 25
BB2summer25.env<-download.envdata(start.date=as.POSIXct("2025-07-22 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2025-08-04 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB2site)
# save the dataset
write.csv(BB2summer25.env,"wdata/BB2summer25_env.csv",row.names=F)

#TB1 Fall 24
TB1fall24.env<-download.envdata(start.date=as.POSIXct("2024-09-23 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2024-10-11 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=TB1site)
# save the dataset
write.csv(TB1fall24.env,"wdata/TB1fall24_env.csv",row.names=F)

#TB1 Winter 25
TB1winter25.env<-download.envdata(start.date=as.POSIXct("2025-01-27 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2025-02-10 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=TB1site)
# save the dataset
write.csv(TB1winter25.env,"wdata/TB1winter25_env.csv",row.names=F)

#TB1 Spring 25
TB1spring25.env<-download.envdata(start.date=as.POSIXct("2025-04-09 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2025-04-23 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=TB1site)
# save the dataset
write.csv(TB1spring25.env,"wdata/TB1spring25_env.csv",row.names=F)

#TB1 Summer 25
TB1summer25.env<-download.envdata(start.date=as.POSIXct("2025-07-28 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2025-08-11 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=TB1site)
# save the dataset
write.csv(TB1summer25.env,"wdata/TB1summer25_env.csv",row.names=F)



#QUESTION 2 2017 envi data 
#BB1 fall 24 isnt working
BB1fall17.env<-download.envdata(start.date=as.POSIXct("2017-09-25 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2017-10-09 23:59:59",tz="America/Chicago"),
                                site=BB1site2)#change to match the end date/time of the deployment
# save the dataset
write.csv(BB1fall17.env,"wdata/BB1fall17_env.csv",row.names=F) 


# BB1 Spring 18
BB1spring18.env<-download.envdata(start.date=as.POSIXct("2018-04-14 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-04-28 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB1site)
# save the dataset
write.csv(BB1spring18.env,"wdata/BB1spring18_env.csv",row.names=F)

# Winter 25
BB1winter18.env<-download.envdata(start.date=as.POSIXct("2018-01-29 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-02-17 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB1site)

# save the dataset
write.csv(BB1winter18.env,"wdata/BB1winter18_env.csv",row.names=F)

# BB1 Spring 18
BB1spring18.env<-download.envdata(start.date=as.POSIXct("2018-04-14 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-04-28 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB1site)
# save the dataset
write.csv(BB1spring18.env,"wdata/BB1spring18_env.csv",row.names=F)

# BB1 Summer 18
BB1summer18.env<-download.envdata(start.date=as.POSIXct("2018-07-21 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-08-04 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB1site)
# save the dataset
write.csv(BB1summer18.env,"wdata/BB1summer18_env.csv",row.names=F)

#BB2 Fall 17
BB2fall17.env<-download.envdata(start.date=as.POSIXct("2017-09-25 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2017-10-09 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=BB2site)
# save the dataset
write.csv(BB2fall17.env,"wdata/BB2fall17_env.csv",row.names=F)

#BB2 Winter 18
BB2winter18.env<-download.envdata(start.date=as.POSIXct("2018-01-29 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-02-17 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB2site)
# save the dataset
write.csv(BB2winter18.env,"wdata/BB2winter18_env.csv",row.names=F)

#BB2 Spring 18
BB2spring18.env<-download.envdata(start.date=as.POSIXct("2018-04-14 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-04-28 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB2site)
# save the dataset
write.csv(BB2spring18.env,"wdata/BB2spring18_env.csv",row.names=F)

#BB2 Summer 18
BB2summer18.env<-download.envdata(start.date=as.POSIXct("2018-07-22 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-08-04 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=BB2site)
# save the dataset
write.csv(BB2summer18.env,"wdata/BB2summer18_env.csv",row.names=F)

#TB1 Fall 17
TB1fall17.env<-download.envdata(start.date=as.POSIXct("2017-09-23 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                end.date=as.POSIXct("2017-10-11 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                site=TB1site)
# save the dataset
write.csv(TB1fall17.env,"wdata/TB1fall17_env.csv",row.names=F)

#TB1 Winter 18
TB1winter18.env<-download.envdata(start.date=as.POSIXct("2018-01-27 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-02-10 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=TB1site)
# save the dataset
write.csv(TB1winter18.env,"wdata/TB1winter18_env.csv",row.names=F)

#TB1 Spring 18
TB1spring18.env<-download.envdata(start.date=as.POSIXct("2018-04-09 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-04-23 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=TB1site)
# save the dataset
write.csv(TB1spring18.env,"wdata/TB1spring18_env.csv",row.names=F)

#TB1 Summer 18
TB1summer18.env<-download.envdata(start.date=as.POSIXct("2018-07-28 00:00:00",tz="America/Chicago"),# change to match the start date/time of the deployment
                                  end.date=as.POSIXct("2018-08-11 23:59:59",tz="America/Chicago"),#change to match the end date/time of the deployment
                                  site=TB1site)
# save the dataset
write.csv(TB1summer18.env,"wdata/TB1summer18_env.csv",row.names=F)


