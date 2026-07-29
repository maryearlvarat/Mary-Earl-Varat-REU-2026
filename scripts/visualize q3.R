#q3 visualization
#q3 is: Do patterns of occupancy correlate with environmental variables

#load packages----
library(tidyverse)
library(vegan)

# load data----
fls<-list.files("wdata",pattern="combined")
fls<-paste0("wdata/",fls)

dts<-read.csv(fls[1])
for(i in 2:length(fls))dts<-bind_rows(dts,read.csv(fls[i]))

table(dts$site,dts$deployment)
dts<-dts%>%
  mutate(season=case_when(
    mnth %in% c(12,1,2)~"Winter",
    mnth %in% c(3,4,5)~"Spring",
    mnth %in% c(6,7,8)~"Summer",
    mnth %in% c(9,10,11)~"Fall"),
    location=ifelse(site %in% c("BB1","TB1"),"outer","inner"),
    bay=ifelse(site %in% c("BB1","BB2"),"Barataria","Terrebonne"))

dts$season<-factor(dts$season,levels=c("Fall","Winter","Spring","Summer"))

ggplot(data=dts)+
  geom_point(aes(x=sal.ppt,y=detection.minutes,color=location))+
  facet_grid(season~bay)+
  ggtitle("Salinity")


ggplot(data=dts)+
  geom_point(aes(x=temp.c,y=detection.minutes,color=location))+
  facet_grid(season~bay)+
  ggtitle("Temperature")

ggplot(data=dts)+
  geom_point(aes(x=do.mg.l,y=detection.minutes,color=location))+
  facet_grid(season~bay)+
  ggtitle("do.mg.l")

dts$dt<-ymd_h(paste(dts$yr,dts$mnth,dts$dy,dts$hr))

ggplot(dts%>%filter(location=="inner"))+
  geom_line(aes(x=dt,y=detection.minutes,group=location))+
  geom_point(aes(x=dt,y=detection.minutes,color=sal.ppt))+
  facet_grid(bay~season,scales="free_x")+
  scale_color_viridis_c()+
  ggtitle("Salinity inner bay")


ggplot(dts%>%filter(location!="inner"))+
  geom_line(aes(x=dt,y=detection.minutes,group=location))+
  geom_point(aes(x=dt,y=detection.minutes,color=sal.ppt))+
  facet_grid(bay~season,scales="free_x")+
  scale_color_viridis_c()+
  ggtitle("Salinity outer bay")

ggplot(dts%>%filter(location=="inner"))+
  geom_line(aes(x=dt,y=detection.minutes,group=location))+
  geom_point(aes(x=dt,y=detection.minutes,color=temp.c))+
  facet_grid(bay~season,scales="free_x")+
  scale_color_viridis_c()+
  ggtitle("Temperature inner bay")


ggplot(dts%>%filter(location!="inner"))+
  geom_line(aes(x=dt,y=detection.minutes,group=location))+
  geom_point(aes(x=dt,y=detection.minutes,color=temp.c))+
  facet_grid(bay~season,scales="free_x")+
  scale_color_viridis_c()+
  ggtitle("Temperature outer bay")


ggplot(dts%>%filter(location=="inner"))+
  geom_line(aes(x=dt,y=detection.minutes,group=location))+
  geom_point(aes(x=dt,y=detection.minutes,color=do.mg.l))+
  facet_grid(bay~season,scales="free_x")+
  scale_color_viridis_c()+
  ggtitle("DO inner bay")


ggplot(dts%>%filter(location!="inner"))+
  geom_line(aes(x=dt,y=detection.minutes,group=location))+
  geom_point(aes(x=dt,y=detection.minutes,color=do.mg.l))+
  facet_grid(bay~season,scales="free_x")+
  scale_color_viridis_c()+
  ggtitle("DO outer bay")
