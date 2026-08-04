# analysis for q3

#load packages----
library(tidyverse)
library(car)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
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

glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  plot(t1)
}

# try out one model with everything
q3.m1<-lmer(detection.minutes~sal.sc*do.sc*temp.sc+location+bay+
         (1|season),data=dts)

plot(q3.m1)
summary(q3.m1)

#trying zero inflated model


q3.m2<-glmmTMB(detection.minutes~sal.sc*do.sc+
                 sal.sc*temp.sc+
                 location+bay+
                 (1|season),
               family=tweedie,
               data=dts)
residuals<-simulateResiduals(q3.m2)
testZeroInflation(residuals)
testDispersion(residuals)
glmm.resids(q3.m2)

summary(q3.m2)

dosal<-ggpredict(q3.m2,terms=c("sal.sc","do.sc"))
plot(dosal)
dosal2<-ggpredict(q3.m2,terms=c("sal.sc","do.sc"),
                  condition=c(bay="Terrebonne",
                              temp.sc=-2))
plot(dosal2)

temp<-ggpredict(q3.m2,terms=c("temp.sc"))
plot(temp)

loc<-ggpredict(q3.m2,terms=c("location"))
plot(loc)

bay<-ggpredict(q3.m2,terms=c("bay"))
plot(bay)

# make pretty figures
theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           legend.key.width = unit(2.25,"line")))

tlab<-expression("Temp " ( degree*C))
slab<-"Salinity (ppt)"
dlab<-expression("Dissolved \nOxygen \nmg L"^"-1")
ylab<-expression("Minutes of dolphin detection hr"^"-1")
doscale<-c("#BEBBFC","#3127F5","#0B0570")

(sal.vals.x<-data.frame(sc=c(-3,-2,-1,0,1,2),
                         new=round(predict(lm(sal.ppt~as.numeric(sal.sc),data=dts),
                                           newdata=data.frame(sal.sc=c(-3,-2,-1,0,1,2))),2))%>%
    mutate(new=ifelse(new<0,0,new)))

(do.vals.leg<-data.frame(sc=c(-1.09,-0.17,0.75),
                        new=round(predict(lm(do.mg.l~as.numeric(do.sc),data=dts),
                                          newdata=data.frame(do.sc=c(-1.09,-0.17,0.75))),2))%>%
    mutate(new=ifelse(new<0,0,new)))

ggplot()+
  geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=group),
              data=dosal,
              alpha=.2)+
  geom_line(aes(x=x,y=predicted,color=group),
            data=dosal,
            size=1.5)+
  ylab(ylab)+
  scale_fill_manual(values=doscale,name=dlab,
                    breaks = do.vals.leg$sc,
                    labels=do.vals.leg$new,)+  
  scale_x_continuous(name=slab,
                     breaks=sal.vals.x$sc,
                     labels=sal.vals.x$new)+
  scale_color_manual(name=dlab,
                     breaks = do.vals.leg$sc,
                     labels=do.vals.leg$new,
                     values=doscale)

ggplot()+
  geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),
              data=temp,
              fill="purple",
              alpha=.2)+
  geom_line(aes(x=x,y=predicted),
            data=temp,
            color="purple",
            size=1.5)+
  ylab(ylab)+
  xlab(tlab)

