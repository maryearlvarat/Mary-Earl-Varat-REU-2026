
#script to join dolphin and envi data, also to take out envi data that does not match with dolphin data 

#update to ensure all have the same column names
cn<-c("mnth","dy","hr","n.dolphins","binary.dolphins","detection.minutes","yr","do.mg.l","sal.ppt","temp.c","site","deployment")
#load data----

#BB1

BB1fall24_env<-read.csv("wdata/BB1fall24_env.csv")
BB1fall24_dolphins<-read.csv("wdata/BB1fall24_dolphins.csv")

BB1fall24<-left_join(BB1fall24_dolphins,BB1fall24_env)%>%
  mutate(site="BB1",
         deployment="Fall")

# add a check if it has all the same columns
summary(colnames(BB1fall24)%in%cn)

# if all true then move on

write.csv(BB1fall24,"wdata/BB1fall24_combined.csv",row.names = F)




BB1winter25_env<-read.csv("wdata/BB1winter25_env.csv")
BB1winter25_dolphins<-read.csv("wdata/BB1winter25_dolphins.csv")

BB1winter25<-left_join(BB1winter25_dolphins,BB1winter25_env)%>%
  mutate(site="BB1",
         deployment="Winter")

summary(colnames(BB1winter25)%in%cn)

write.csv(BB1winter25,"wdata/BB1winter25_combined.csv",row.names = F)




BB1spring25_env<-read.csv("wdata/BB1spring25_env.csv")
BB1spring25_dolphins<-read.csv("wdata/BB1spring25_dolphins.csv")

BB1spring25<-left_join(BB1spring25_dolphins,BB1spring25_env)%>%
  mutate(site="BB1",
         deployment="Spring")

summary(colnames(BB1spring25)%in%cn)
BB1spring25<-bind_rows(winter25[0,],spring25)
write.csv(BB1spring25,"wdata/BB1spring25_combined.csv",row.names = F)
#BB1 Summer 25

BB1summer25_env<-read.csv("wdata/BB1summer25_env.csv")
BB1summer25_dolphins<-read.csv("wdata/BB1summer25_dolphins.csv")

BB1summer25<-left_join(BB1summer25_dolphins,BB1summer25_env)%>%
  mutate(site="BB1",
         deployment="Summer")

summary(colnames(BB1summer25)%in%cn)
write.csv(BB1summer25,"wdata/BB1summer25_combined.csv",row.names = F)

#BB2 

BB2fall24_env<-read.csv("wdata/BB2fall24_env.csv")
BB2fall24_dolphins<-read.csv("wdata/BB2fall24_dolphins.csv")

BB2fall24<-left_join(BB2fall24_dolphins,BB2fall24_env)%>%
  mutate(site="BB2",
         deployment="Fall")

summary(colnames(BB2fall24)%in%cn)
write.csv(BB2fall24,"wdata/BB2fall24_combined.csv",row.names = F)


#BB2 Winter

BB2winter25_env<-read.csv("wdata/BB2winter25_env.csv")
BB2winter25_dolphins<-read.csv("wdata/BB2winter25_dolphins.csv")

BB2winter25<-left_join(BB2winter25_dolphins,BB2winter25_env)%>%
  mutate(site="BB2",
         deployment="Winter")

summary(colnames(BB2winter25)%in%cn)
write.csv(BB2winter25,"wdata/BB2winter25_combined.csv",row.names = F)


#BB2 Spring 25

BB2spring25_env<-read.csv("wdata/BB2spring25_env.csv")
BB2spring25_dolphins<-read.csv("wdata/BB2spring25_dolphins.csv")

BB2spring25<-left_join(BB2spring25_dolphins,BB2spring25_env)%>%
  mutate(site="BB2",
         deployment="Spring")

summary(colnames(BB2spring25)%in%cn)

write.csv(BB2spring25,"wdata/BB2spring25_combined.csv",row.names = F)

#BB2 Summer 
BB2summer25_env<-read.csv("wdata/BB2summer25_env.csv")
BB2summer25_dolphins<-read.csv("wdata/BB2summer25_dolphins.csv")

BB2summer25<-left_join(BB2summer25_dolphins,BB2summer25_env)%>%
  mutate(site="BB2",
         deployment="Summer",
         do.mg.l=NA)
BB2summer25<-bind_rows(BB2spring25[0,],BB2summer25)

summary(colnames(BB2summer25)%in% cn)

write.csv(BB2summer25,"wdata/BB2summer25_combined.csv",row.names = F)


#LUMO6

LUMO6_env<-read.csv("wdata/LUMO6_env.csv")
LUMO6summer25_dolphins<-read.csv("wdata/LUMO6summer25_dolphins.csv")
LUMO6summer25<-left_join(LUMO6summer25_dolphins,LUMO6_env)%>%
  mutate(site="LUMO6",
         deployment="Summer")%>%
  select(-deploy)
summary(colnames(LUMO6summer25)%in% cn)

LUMO6summer25_combined<-bind_rows(BB2summer25[0,],LUMO6summer25)# lines like this are only to put the columns in the same order
write.csv(LUMO6summer25_combined,"wdata/LUMO6summer25_combined.csv",row.names = F)

#LUMO WINTER
LUMO6_env<-read.csv("wdata/LUMO6_env.csv")
LUMO6winter25_dolphins<-read.csv("wdata/LUMO6winter25_dolphins.csv")
LUMO6winter25<-left_join(LUMO6winter25_dolphins,LUMO6_env)%>%
  mutate(site="LUMO6",
         deployment="Winter")%>%
  select(-deploy)
summary(colnames(LUMO6winter25)%in% cn)
LUMO6winter25_combined<-bind_rows(LUMO6summer25[0,],LUMO6winter25)
write.csv(LUMO6winter25_combined,"wdata/LUMO6winter25_combined.csv",row.names = F)

#LUMOFALL
LUMO6_env<-read.csv("wdata/LUMO6_env.csv")
LUMO6fall24_dolphins<-read.csv("wdata/LUMO6fall24_dolphins.csv")
LUMO6fall24<-left_join(LUMO6fall24_dolphins,LUMO6_env)%>%
  mutate(site="LUMO6",
         deployment="Fall")%>%
  select(-deploy)
summary(colnames(LUMO6fall24)%in% cn)
LUMO6fall24_combined<-bind_rows(LUMO6winter25[0,],LUMO6fall24)
write.csv(LUMO6fall24_combined,"wdata/LUMO6fall24_combined.csv",row.names = F)

#LUMOSpring
LUMO6_env<-read.csv("wdata/LUMO6_env.csv")
LUMO6spring25_dolphins<-read.csv("wdata/LUMO6spring25_dolphins.csv")
LUMO6spring25<-left_join(LUMO6spring25_dolphins,LUMO6_env)%>%
  mutate(site="LUMO6",
         deployment="Spring")%>%
  select(-deploy)
summary(colnames(LUMO6spring25)%in% cn)
LUMO6spring25_combined<-bind_rows(LUMO6winter25[0,],LUMO6spring25)
write.csv(LUMO6spring25_combined,"wdata/LUMO6spring25_combined.csv",row.names = F)

#TB1
#TB1 Winter

## NOT WORKING START HERE
TB1winter_env<-read.csv("wdata/TB1winter25_env.csv")
TB1winter25_dolphins<-read.csv("wdata/TB1winter25_dolphins.csv")
TB1winter25_combined<-left_join(TB1winter25_dolphins,TB1winter_env)%>%
  mutate(site="TB1",
         deployment="Winter",
         do.mg.l=NA)
summary(colnames(TB1winter25_combined)%in% cn)
TB1winter25_combined<-bind_rows(BB1spring25[0,],TB1winter25_combined)
write.csv(TB1winter25_combined,"wdata/TB1winter25_combined.csv",row.names = F)

#TB1 Summer
TB1summer_env<-read.csv("wdata/TB1summer25_env.csv")
TB1summer25_dolphins<-read.csv("wdata/TB1summer25_dolphins.csv")
TB1summer25_combined<-left_join(TB1summer25_dolphins,TB1summer_env)%>%
  mutate(site="TB1",
         deployment="Summer",
         do.mg.l=NA)
summary(colnames(TB1summer25_combined)%in% cn)
TB1summer25_combined<-bind_rows(BB1spring25[0,],TB1summer25_combined)
write.csv(TB1summer25_combined,"wdata/TB1summer25_combined.csv",row.names = F)
