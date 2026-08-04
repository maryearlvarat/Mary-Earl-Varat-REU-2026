# script to do logistic regression of detection vs environmental variables

#load packages----
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("glmmTMB")
lp("DHARMa")
lp("ggeffects")


# load data
# find files with combined in them
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


#look to see if there is time structure
ggplot(data=dts%>%mutate(dt=ymd_h(paste(yr,mnth,dy,hr))))+
  geom_line(aes(x=dt,y=binary.dolphins))  

ggplot(data=dts)+
  geom_boxplot(aes(y=do.mg.l,fill=as.factor(binary.dolphins)))

ggplot(data=dts)+
  geom_point(aes(x=do.mg.l,y=binary.dolphins))

ggplot(data=dts)+
  geom_boxplot(aes(y=sal.ppt,fill=as.factor(binary.dolphins)))

ggplot(data=dts)+
  geom_point(aes(x=sal.ppt,y=binary.dolphins))

ggplot(data=dts)+
  geom_boxplot(aes(y=temp.c,fill=as.factor(binary.dolphins)))

ggplot(data=dts)+
  geom_point(aes(x=temp.c,y=binary.dolphins))

pairs(dts[,c(7:9)])

glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  plot(t1)
}

# binomial model
log.mod<-glmmTMB(binary.dolphins~do.mg.l*sal.ppt+
                   sal.ppt*temp.c+location+bay,
                 data=dts,
                 family=binomial)
glmm.resids(log.mod)

summary(log.mod)

plot(ggeffect(log.mod,terms=c("sal.ppt","do.mg.l")))+
ggplot2::scale_color_manual(values = c("#BEBBFC", "#3127F5", "#0B0570"))+
ggplot2::scale_fill_manual(values = c("#BEBBFC", "#3127F5", "#0B0570"))
