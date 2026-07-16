
#script to join dolphin and envi data, also to take out envi data that does not match with dolphin data 

#load data----
fall24_env<-read.csv("wdata/fall24_env.csv")
fall24_dolphins<-read.csv("wdata/fall24_dolphins.csv")

fall24<-left_join(fall24_dolphins,fall24_env)

write.csv(fall24,"wdata/combind_fall24.csv",row.names = F)

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
