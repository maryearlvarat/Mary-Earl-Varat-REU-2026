#READ in BB1 Fall 24 data
library(tidyverse)
bb1fall24<-read.csv(file="wdata/bb1fall24_combined.csv")

ggplot(bb1fall24)+
  geom_point(aes(x=sal.ppt, y=binary.dolphins))
ggplot(bb1fall24)+
  geom_point(aes(x=sal.ppt, y=n.dolphins))
model<-lm(n.dolphins~sal.ppt, data=bb1fall24)
summary(model)


bb1summer25<-read.csv(file="wdata/combined_BB1summer25.csv")

ggplot(bb1summer25)+
  geom_point(aes(x=sal.ppt, y=binary.dolphins))
ggplot(bb1summer25)+
  geom_point(aes(x=sal.ppt, y=n.dolphins))
model<-lm(n.dolphins~sal.ppt, data=bb1summer25)
summary(model)
resids<-residuals(model)
summary(resids)




modelb<-glm(binary.dolphins~sal.ppt, data=bb1summer25, family=binomial)
summary(modelb)
resids<-residuals(modelb)
summary(resids)

#BB2 Fall 24
bb2fall24<-read.csv(file="wdata/bb2fall24_combined.csv")

ggplot(bb2fall24)+
  geom_point(aes(x=sal.ppt, y=binary.dolphins))
ggplot(bb2fall24)+
  geom_point(aes(x=sal.ppt, y=n.dolphins))
model<-lm(n.dolphins~sal.ppt, data=bb2fall24)
summary(model)
resids<-residuals(model)
summary(resids)
#makes bins for bb2
bb2fall24p<-bb2fall24%>%
  filter(binary.dolphins==1)%>%
  mutate(bin=cut(sal.ppt, breaks= c(0,5,8,11,15)))
table(cut(bb2fall24$sal.ppt, breaks= c(0,5,8,11,15)))
#makes bins into bar graph
ggplot(bb2fall24p)+
  geom_bar(aes(bin))
