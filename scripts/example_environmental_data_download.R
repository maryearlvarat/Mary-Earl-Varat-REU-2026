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
start.date<-as.POSIXct("2024-09-25 00:00:00",tz="America/Chicago")# change to match the start date/time of the deployment
end.date<-as.POSIXct("2024-10-09 23:59:59",tz="America/Chicago")#change to match the end date/time of the deployment

# download data
env.data<-read_waterdata_continuous(monitoring_location_id = bb1site,#change to match site
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



# save the dataset
write.csv(env.data2,"wdata/PICKADESCRIPTIVEFILENAME.csv",row.names=F)

