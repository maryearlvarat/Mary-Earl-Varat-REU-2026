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

dts<-read_csv(fls)

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
log.mod<-glmmTMB(binary.dolphins~do.mg.l*sal.ppt+do.mg.l*temp.c+sal.ppt*temp.c,
                 data=dts,
                 family=binomial)
glmm.resids(log.mod)

summary(log.mod)

plot(ggeffect(log.mod,terms=c("do.mg.l","temp.c")))
