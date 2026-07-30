#analysis Q2
# load packages
#load packages----
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("glmmTMB")
lp("DHARMa")
lp("ggeffects")
lp("vegan")
lp("interactions")

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
         yr2=ifelse(yr %in% c(2017,2018),2018,2025),
         location=ifelse(site %in% c("BB1","TB1"),"outer","inner"),
         bay=ifelse(site %in% c("BB1","BB2"),"Barataria","Terrebonne"))

env.new<- env %>%
  filter(yr2!=2018)
table(env.new$location,env.new$bay)  

env.new.nodo<-env.new%>%
  filter(location!="outer")
#make models 

glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  plot(t1)
}
#model with  new sal 
q2.sal.m1<-glmmTMB(sal.ppt~location*bay+(1|season),data=env.new)
glmm.resids(q2.sal.m1)
summary(q2.sal.m1)
sal<-ggpredict(q2.sal.m1)
plot(sal)

cat_plot(q2.sal.m1, pred=bay,modx=location)

#model with new  DO
q2.do.m1<-glmmTMB(do.mg.l~bay+(1|season),data=env.new.nodo)
glmm.resids(q2.do.m1)
summary(q2.do.m1)
do<-ggpredict(q2.do.m1)
plot(do)

cat_plot(q2.do.m1, pred=bay)

#model with new  temp 
q2.temp.m1<-glmmTMB(temp.c~location*bay+(1|season),data=env.new)
glmm.resids(q2.temp.m1)
summary(q2.temp.m1)
temp<-ggpredict(q2.temp.m1)
plot(temp)

cat_plot(q2.temp.m1, pred=bay,modx=location)
