# script to visualize q2-

# how does the environment differ between 2017-2018 and 2024-2025?

# load packages
source("scripts/install_packages_function.R")
library(tidyverse)
lp("vegan")

# load data
fls<-list.files(path="wdata",pattern="env")
fls2<-data.frame(fls=fls,fpath=paste0("wdata/",fls))%>%
  separate(fls,into = c("dep2","env"),sep=3)%>%
  select(-env)
fpath<-fls2$fpath

env<-read.csv(fpath[1])
env$site<-fls2$dep2[1]
for(i in 2:nrow(fls2)){
  t2<-read.csv(fls2$fpath[i])
  t2$site<-fls2$dep2[i]
  env<-bind_rows(env,t2)
}

env<-env|>
  mutate(mnth=ifelse(is.na(mnth),month(date),mnth),
         yr=ifelse(is.na(yr),year(date),yr),
    season=case_when(
    mnth %in% c(12,1,2)~"Winter",
    mnth %in% c(3,4,5)~"Spring",
    mnth %in% c(6,7,8)~"Summer",
    mnth %in% c(9,10,11)~"Fall"),
    yr2=ifelse(yr %in% c(2017,2018),2018,2025))

# visualize data at sites across time
ggplot(data=env)+
  geom_boxplot(aes(y=sal.ppt,fill=as.factor(yr2)))+
  facet_grid(site~season)+
  ggtitle("Salinity")

ggplot(data=env)+
  geom_boxplot(aes(y=temp.c,fill=as.factor(yr2)))+
  facet_grid(site~season)+
  ggtitle("Temp")

ggplot(data=env)+
  geom_boxplot(aes(y=do.mg.l,fill=as.factor(yr2)))+
  facet_grid(site~season)+
  ggtitle("do.mg.l")

env2<-env%>%
  filter(!is.na(sal.ppt))|>
  filter(!is.na(temp.c))|>
  filter(!is.na(do.mg.l))

env.a<-env2[,c(5,6,8)]
env.a2<-decostand(env.a,method="standardize")
env.rda<-rda(env.a2)  
plot(env.rda)
env.scores<-data.frame(scores(env.rda,c(1,2),"sites"))
env2<-bind_cols(env2,env.scores)
ggplot(env2)+
  geom_point(aes(x=PC1,y=PC2,color=site))

ggplot(env2)+
  geom_point(aes(x=PC1,y=PC2,color=as.factor(yr2)))+
  facet_grid(site~season)
summary(env.rda)
