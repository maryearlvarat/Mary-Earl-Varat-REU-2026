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
saldo<-ggeffect(log.mod,terms=c("sal.ppt","do.mg.l"))
plot(saldo)+
ggplot2::scale_color_manual(name="DO (mg/l)",values = c("#BEBBFC", "#3127F5", "#0B0570"))+
ggplot2::scale_fill_manual(name="DO (mg/l)",values = c("#BEBBFC", "#3127F5", "#0B0570"))+
  ylab("Probability of detecting a dolphin")+
  xlab("Salinity (ppt)")+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))+
  ggtitle("")

ggplot()+
  geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=group),
              data=saldo,
              alpha=.2)+
  geom_line(aes(x=x,y=predicted,color=group),
            data=saldo)+
  ylab(ylab)+
  scale_color_manual(name="DO (mg/l)",values = c("#BEBBFC", "#3127F5", "#0B0570"))+
  scale_fill_manual(name="DO (mg/l)",values = c("#BEBBFC", "#3127F5", "#0B0570"))+
  ylab("Probability of detecting a dolphin")+
  xlab("Salinity (ppt)")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        legend.text=element_text(size=14),
        legend.title = element_text(size=18))+
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4),
                     labels=c("0","10%","20%","30%","40%"))
ggsave("figures/SalandDO PODt.jpeg",width=7, height=6)

#plot for location
eff <- ggeffect(log.mod, terms = "location")
eff_df <- as.data.frame(eff)

ggplot(eff_df, aes(x = x, y = predicted, color = x)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 1) +
  ylab("Probability of detecting a dolphin") +
  xlab("") +
  scale_color_manual(values = c("#BEBBFC","#3127F5","#...")) +
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=16),
        legend.position = "none")
ggsave("figures/Loc POD.jpeg",width=7, height=6)

#plot for bay
eff <- ggeffect(log.mod, terms = "bay")
eff_df <- as.data.frame(eff)

ggplot(eff_df, aes(x = x, y = predicted, color = x)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 1) +
  ylab("Probability of detecting a dolphin") +
  xlab("") +
  scale_color_manual(values = c("#BEBBFC","#3127F5","#...")) +
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=16),
        legend.position = "none")
ggsave("figures/Bay POD.jpeg",width=7, height=6)

#plot for temp.c

eff_df$x <- factor(eff_df$x)

ggplot(eff_df, aes(x = x, y = predicted, color = x)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 1) +
  ylab("Probability of detecting a dolphin") +
  xlab("") +
  scale_color_manual(values = c("#BEBBFC","#948EFA","#6A62F8","#4036F7","#160AF5","#1208C9")) +
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=10),
        legend.position = "none")

ggsave("figures/Temp POD.jpeg", width=7, height=6)


#cant change these colors
plot(ggeffect(log.mod,terms=c("location")))+
  ylab("Probability of detecting a dolphin")+
  ggtitle("")+
  scale_color_manual(values=c("#BEBBFC", "#3127F5"))

plot(ggeffect(log.mod,terms=c("Bay")))+
  ylab("Probability of detecting a dolphin")+
  ggtitle("")

plot(ggeffect(log.mod,terms=c("temp.c")))+
  ylab("Probability of detecting a dolphin")+
  ggtitle("")
scale_color_manual(values=c("#BEBBFC", "#3127F5"))


