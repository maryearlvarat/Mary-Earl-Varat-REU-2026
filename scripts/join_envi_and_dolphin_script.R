
#script to join dolphin and envi data, also to take out envi data that does not match with dolphin data 

#load data----
bb1fall24_env<-read.csv("wdata/bb1fall24_env.csv")
bb1fall24_dolphins<-read.csv("wdata/bb1fall24_dolphins.csv")

bb1fall24<-left_join(bb1fall24_dolphins,bb1fall24_env)%>%
  mutate(site="BB1",
         deployment="Fall")

write.csv(bb1fall24,"wdata/bb1fall24_combined.csv",row.names = F)




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

#BB2 

bb2fall24_env<-read.csv("wdata/bb2fall24_env.csv")
bb2fall24_dolphins<-read.csv("wdata/bb2fall24_dolphins.csv")

bb2fall24<-left_join(bb2fall24_dolphins,bb2fall24_env)%>%
  mutate(site="bb2",
         deployment="Fall")

write.csv(bb2fall24,"wdata/bb2fall24_combined.csv",row.names = F)




winter25_env<-read.csv("wdata/bb2winter25_env.csv")
winter25_dolphins<-read.csv("wdata/bb2winter25_dolphins.csv")

winter25<-left_join(winter25_dolphins,winter25_env)%>%
  mutate(site="bb2",
         deployment="Winter")

write.csv(winter25,"wdata/bb2winter25_combined.csv",row.names = F)




spring25_env<-read.csv("wdata/bb2spring25_env.csv")
spring25_dolphins<-read.csv("wdata/bb2spring25_dolphins.csv")

spring25<-left_join(spring25_dolphins,spring25_env)%>%
  mutate(site="bb2",
         deployment="Spring")

write.csv(spring25,"wdata/bb2spring25_combined.csv",row.names = F)


summer25_env<-read.csv("wdata/bb2summer25_env.csv")
summer25_dolphins<-read.csv("wdata/bb2summer25_dolphins.csv")

summer25<-left_join(summer25_dolphins,summer25_env)%>%
  mutate(site="bb2",
         deployment="Summer")

write.csv(summer25,"wdata/bb2summer25_combined.csv",row.names = F)


