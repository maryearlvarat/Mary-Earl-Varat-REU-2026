#visualization for q1
# do qualitative patterns of dolphin detections differ between 2017/2018 and today
# in Barataria 
# and do distribution of detections vary between bays today

source("scripts/install_packages_function.R")
lp("tidyverse")



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


bar.ob1<-dts%>%
  filter(bay=="Barataria")%>%
  filter(!is.na(binary.dolphins))%>%
  filter(!is.na(sal.ppt))%>%
  mutate(sal.group=case_when(
    sal.ppt<=5~1,
    sal.ppt>5 & sal.ppt<=11~2,
    sal.ppt>11~3),
         total.detections=sum(binary.dolphins))%>%
  group_by(sal.group)%>%
  reframe(n.sals=n(),
          n.detections=sum(binary.dolphins),
          per.detections=100*(n.detections/total.detections),
          sal.per.detection=100*(n.detections/n.sals))%>%
  distinct()%>%
  mutate(bay="Barataria")


ter.ob1<-dts%>%
  filter(bay!="Barataria")%>%
  filter(!is.na(binary.dolphins))%>%
  filter(!is.na(sal.ppt))%>%
  mutate(sal.group=case_when(
    sal.ppt<=5~1,
    sal.ppt>5 & sal.ppt<=11~2,
    sal.ppt>11~3),
    total.detections=sum(binary.dolphins))%>%
  group_by(sal.group)%>%
  reframe(n.sals=n(),
          n.detections=sum(binary.dolphins),
          per.detections=100*(n.detections/total.detections),
          sal.per.detection=100*(n.detections/n.sals))%>%
  distinct()%>%
  mutate(bay="Terrebonne")

his.obs<-data.frame(sal.group=c(1,2,3),
                    per.detections=c(1,9,90),
                    sal.per.detection=NA,bay="Barataria - 2017/2018")
obs.sum<-bind_rows(bar.ob1,ter.ob1,his.obs)
obs.sum$bay<-factor(obs.sum$bay,
                    levels=c("Barataria - 2017/2018",
                             "Barataria",
                             "Terrebonne"),
                    labels=c("Barataria - 2017/2018",
                             "Barataria - 2024/2025",
                             "Terrebonne - 2024/2025"))


ggplot(data=obs.sum)+
  geom_bar(aes(x=sal.group,y=per.detections,fill=bay),
           stat="identity",
           position=position_dodge())+
  scale_x_continuous(name="Salinity",
                     breaks=c(1,2,3),labels = c("< 5","5-11",">11"))
ggplot(data=obs.sum)+
  geom_bar(aes(x=sal.group,y=per.detections,fill=bay),
           stat="identity",
           position=position_dodge())+
  scale_x_continuous(name="Salinity (ppt)",
                     breaks=c(1,2,3),
                     labels = c("< 5","5-11",">11"))+
  scale_y_continuous(name="Percent of Detections")+
  scale_fill_manual(name="Bay",
                    values=c("Barataria - 2017/2018"="#BEBBFC",
                             "Barataria - 2024/2025"="#3127F5",
                             "Terrebonne - 2024/2025"="#0B0570"))+
  # labs(title="Dolphin Detections by Salinity Group")+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    panel.grid=element_blank(),
    axis.title=element_text(size=18)
    
  )
obs.sum %>% select(bay, sal.group, per.detections)

