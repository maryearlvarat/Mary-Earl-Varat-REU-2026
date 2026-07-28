
# explore missing data
library(tidyverse)

#load data
fls<-list.files("wdata",pattern="combined")
fls<-paste0("wdata/",fls)

dts<-read_csv(fls)%>%
  mutate(dt=ymd_h(paste(yr,mnth,dy,hr)),
         is.missing.sal=ifelse(is.na(sal.ppt),1,0),
         is.missing.temp=ifelse(is.na(temp.c),1,0),
         is.missing.do=ifelse(is.na(do.mg.l),1,0))


#plot
ggplot(data=dts)+
  geom_point(aes(x=dt,y=is.missing.sal))+
  facet_wrap(~site,scales="free")

ggplot(data=dts)+
  geom_point(aes(x=dt,y=is.missing.temp))+
  facet_wrap(~site,scales="free_x")

ggplot(data=dts)+
  geom_point(aes(x=dt,y=is.missing.do))+
  facet_wrap(~site,scales="free_x")

pairs(dts[,8:10])

ggplot(dts)+
  # geom_point(aes(x=sal.ppt,y=do.mg.l,color=deployment))+
  # facet_wrap(~site)
  geom_point(aes(x=sal.ppt,y=do.mg.l,color=site,shape=as.factor(binary.dolphins)),size=2,alpha=.6)+
  facet_wrap(~deployment)


ggplot(dts)+
  # geom_point(aes(x=sal.ppt,y=do.mg.l,color=deployment))+
  # facet_wrap(~site)
  geom_point(aes(x=sal.ppt,y=temp.c,color=site,size=do.mg.l),alpha=.6)+
  facet_wrap(~deployment)
