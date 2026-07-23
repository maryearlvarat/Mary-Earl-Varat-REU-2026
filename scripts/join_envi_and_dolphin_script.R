
#script to join dolphin and envi data, also to take out envi data that does not match with dolphin data 

#update to ensure all have the same column names
cn<-c("mnth","dy","hr","n.dolphins","binary.dolphins","yr","do.mg.l","sal.ppt","temp.c","site","deployment")
#load data----
bb1fall24_env<-read.csv("wdata/bb1fall24_env.csv")
bb1fall24_dolphins<-read.csv("wdata/bb1fall24_dolphins.csv")

bb1fall24<-left_join(bb1fall24_dolphins,bb1fall24_env)%>%
  mutate(site="BB1",
         deployment="Fall")

# add a check if it has all the same columns
summary(colnames(bb1fall24)%in%cn)

# if all true then move on

write.csv(bb1fall24,"wdata/bb1fall24_combined.csv",row.names = F)




winter25_env<-read.csv("wdata/bb1winter25_env.csv")
winter25_dolphins<-read.csv("wdata/BB1winter25_dolphins.csv")

winter25<-left_join(winter25_dolphins,winter25_env)%>%
  mutate(site="BB1",
         deployment="Winter")

summary(colnames(winter25)%in%cn)

write.csv(winter25,"wdata/combined_BB1winter25.csv",row.names = F)




spring25_env<-read.csv("wdata/bb1spring25_env.csv")
spring25_dolphins<-read.csv("wdata/BB1spring25_dolphins.csv")

spring25<-left_join(spring25_dolphins,spring25_env)%>%
  mutate(site="BB1",
         deployment="Spring")

summary(colnames(spring25)%in%cn)

write.csv(spring25,"wdata/combined_BB1spring25.csv",row.names = F)


summer25_env<-read.csv("wdata/bb1summer25_env.csv")
summer25_dolphins<-read.csv("wdata/BB1summer25_dolphins.csv")

summer25<-left_join(summer25_dolphins,summer25_env)%>%
  mutate(site="BB1",
         deployment="Summer")

summary(colnames(summer25)%in%cn)
write.csv(summer25,"wdata/combined_BB1summer25.csv",row.names = F)

#BB2 

bb2fall24_env<-read.csv("wdata/bb2fall24_env.csv")
bb2fall24_dolphins<-read.csv("wdata/bb2fall24_dolphins.csv")

bb2fall24<-left_join(bb2fall24_dolphins,bb2fall24_env)%>%
  mutate(site="bb2",
         deployment="Fall")

summary(colnames(bb2fall24)%in%cn)
write.csv(bb2fall24,"wdata/bb2fall24_combined.csv",row.names = F)




winter25_env<-read.csv("wdata/bb2winter25_env.csv")
winter25_dolphins<-read.csv("wdata/bb2winter25_dolphins.csv")

winter25<-left_join(winter25_dolphins,winter25_env)%>%
  mutate(site="bb2",
         deployment="Winter")

summary(colnames(winter25)%in%cn)
write.csv(winter25,"wdata/bb2winter25_combined.csv",row.names = F)




spring25_env<-read.csv("wdata/bb2spring25_env.csv")
spring25_dolphins<-read.csv("wdata/bb2spring25_dolphins.csv")

spring25<-left_join(spring25_dolphins,spring25_env)%>%
  mutate(site="bb2",
         deployment="Spring")

summary(colnames(spring25)%in%cn)

write.csv(spring25,"wdata/bb2spring25_combined.csv",row.names = F)


summer25_env<-read.csv("wdata/bb2summer25_env.csv")
summer25_dolphins<-read.csv("wdata/bb2summer25_dolphins.csv")

summer25<-left_join(summer25_dolphins,summer25_env)%>%
  mutate(site="bb2",
         deployment="Summer")

summary(cn %in% colnames(summer25))

write.csv(summer25,"wdata/bb2summer25_combined.csv",row.names = F)


#LUMO6

lumo6_env<-read.csv("wdata/lumo6_env.csv")
lumo6summer25_dolphins<-read.csv("wdata/lumo6summer25_dolphins.csv")
lumo6summer25_combined<-left_join(lumo6summer25_dolphins,lumo6_env)%>%
  mutate(site="LUMO6",
         deployment="Summer")%>%
  select(-deploy)

write.csv(lumo6summer25_combined,"wdata/lumo6summer25_combined.csv",row.names = F)

lumo6_env<-read.csv("wdata/lumo6_env.csv")
lumo6winter25_dolphins<-read.csv("wdata/lumo6winter25_dolphins.csv")
lumo6winter25_combined<-left_join(lumo6winter25_dolphins,lumo6_env)%>%
  mutate(site="LUMO6",
         deployment="Winter")%>%
  select(-deploy)


write.csv(lumo6winter25_combined,"wdata/lumo6winter25_combined.csv",row.names = F)
