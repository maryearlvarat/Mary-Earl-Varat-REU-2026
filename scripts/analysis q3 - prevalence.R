# analysis for q3

#load packages----
library(tidyverse)
library(car)
library(lmerTest)

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

# model looking at drivers of detection minutes
check.col<-lmer(detection.minutes~sal.ppt+do.mg.l+temp.c+location+bay+
                  (1|season),data=dts)

vif(check.col)
dts$sal.sc<-scale(dts$sal.ppt)
dts$temp.sc<-scale(dts$temp.c)
dts$do.sc<-scale(dts$do.mg.l)

# try out one model with everything
q3.m1<-lmer(detection.minutes~sal.sc*do.sc*temp.sc+location+bay+
         (1|season),data=dts)

plot(q3.m1)
summary(q3.m1)
