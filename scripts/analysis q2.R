#analysis Q2
# load packages
#load packages----
source("scripts/install_packages_function.R")
lp("jtools")
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
         location=ifelse(site %in% c("BB1","bb1","TB1","tb1"),"Outer","Inner"),
         bay=ifelse(site %in% c("BB1","BB2","bb1","bb2"),"Barataria","Terrebonne"))

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

#cat_plot(q2.sal.m1, pred=bay,modx=location, colors = c("#BEBBFC","#3127F5" ))+
  #ggplot2::labs(title = "Salinity across Locations")
       
q2sal.lb<-ggeffect(q2.sal.m1,terms=c("location","bay"))
plot(q2sal.lb)

ggplot(data=q2sal.lb)+
  geom_errorbar(aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=.1)+
  geom_point(aes(x=x,y=predicted,color=group),size=4)+
  ggtitle("Salinity across bays and locations")+
  ylab("Salinity (ppt)")+
  xlab("")

sal.sum<-env.new|>
  filter(!is.na(sal.ppt))|>
  group_by(bay,location)|>
  summarize(m.sal=mean(sal.ppt),
            se.sal=sd(sal.ppt)/sqrt(n()+1))

ggplot(data=sal.sum)+
  geom_errorbar(aes(x=location,ymin=m.sal-se.sal,ymax=m.sal+se.sal,color=bay),width=.5,position = position_dodge(0.9))+
  geom_bar(aes(x=location,y=m.sal,fill=bay),position=position_dodge(0.9),stat="identity")+
  ylab("Salinity (ppt)")+
  xlab("")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        legend.text = element_text(size=14))+
  scale_color_viridis_d(option="B",begin=.4,end=.6,name="Bay")+
  scale_fill_viridis_d(option="B",begin=.4,end=.6,name="Bay")
  
summary(q2.sal.m1)

#model with new  DO
q2.do.m1<-glmmTMB(do.mg.l~bay+location+(1|season),data=env.new.nodo)
glmm.resids(q2.do.m1)
summary(q2.do.m1)
do<-ggpredict(q2.do.m1)
plot(do)

cat_plot(q2.do.m1, pred=bay,colors = c("#BEBBFC","#3127F5"))

#p <- cat_plot(q2.do.m1, pred = bay)
#p + ggplot2::scale_fill_manual(values = c("#BEBBFC", "#3127F5")) +
  ggplot2::scale_color_manual(values = c("#BEBBFC", "#3127F5" ))

effect_plot(q2.do.m1, pred = bay, colors = c("#BEBBFC", "#3127F5"), interval = TRUE)+
  ggplot2::labs(title = "Dissolved Oxygen across Locations")



#model with new  temp 
q2.temp.m1<-glmmTMB(temp.c~location*bay+(1|season),data=env.new)
glmm.resids(q2.temp.m1)
summary(q2.temp.m1)
temp<-ggpredict(q2.temp.m1)
plot(temp)

cat_plot(q2.temp.m1, pred=bay,modx=location,colors = c("#BEBBFC","#3127F5" ))+
  ggplot2::labs(title = "Temperature across Locations")
summary(q2.temp.m1)


