
#script to join dolphin and envi data, also to take out envi data that does not match with dolphin data 

#load data----
fall24_env<-read.csv("wdata/bb1fall24_env.csv")
fall24_dolphins<-read.csv("wdata/BB1fall24_dolphins.csv")

fall24<-left_join(fall24_dolphins,fall24_env)%>%
  mutate(site="BB1",
         deployment="Fall")

write.csv(fall24,"wdata/combined_BB1fall24.csv",row.names = F)




winter25_env<-read.csv("wdata/bb1winter25_env.csv")
winter25_dolphins<-read.csv("wdata/BB1winter25_dolphins.csv")

winter25<-left_join(winter25_dolphins,winter25_env)%>%
  mutate(site="BB1",
         deployment="Winter")

write.csv(winter25,"wdata/combined_BB1winter25.csv",row.names = F)




spring25_env<-read.csv("wdata/bb1spring25_env.csv")
spring25_dolphins<-read.csv("wdata/BB1spring25_dolphins.csv")

spring25<-left_join(spring25_dolphins,spring25_env)%>%
  mutate(site="BB1",
         deployment="Spring")

write.csv(spring25,"wdata/combined_BB1spring25.csv",row.names = F)


summer25_env<-read.csv("wdata/bb1summer25_env.csv")
summer25_dolphins<-read.csv("wdata/BB1summer25_dolphins.csv")

summer25<-left_join(summer25_dolphins,summer25_env)%>%
  mutate(site="BB1",
         deployment="Summer")

write.csv(summer25,"wdata/combined_BB1summer25.csv",row.names = F)

# #delete the env rows that do not match with dolphin data
# env.data3<- env.data2[-c(1:16,340:360),]
# newname<- c(start.mnth = "mnth", start.dy = "dy", start.hr = "hr")
# #slice(17:339)%>%
# envdata.join<-env.data3 %>% 
#   rename(all_of (newname))
# 
# 
# 
# #join envjoin data and dolphin data
# test.join<-left_join(dep.dt,envdata.join)
