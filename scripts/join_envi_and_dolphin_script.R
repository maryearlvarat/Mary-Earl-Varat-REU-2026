#script to join dolphin and envi data, also to take out envi data that does not match with dolphin data 

#delete the env rows that do not match with dolphin data
env.data3<- env.data2[-c(1:16,340:360),]
newname<- c(start.mnth = "mnth", start.dy = "dy", start.hr = "hr")
#slice(17:339)%>%
envdata.join<-env.data3 %>% 
  rename(all_of (newname))



#join envjoin data and dolphin data
test.join<-left_join(dep.dt,envdata.join)
