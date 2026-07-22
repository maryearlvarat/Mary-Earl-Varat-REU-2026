# Code to add in marine center env data 

source("scripts/install_packages_function.R")
lp("tidyverse")
library("readxl")

#Read in Marine Sal and Temp data
mcsalandtemp<-read_xlsx(path="odata/Mary_MC_SAL_WT.xlsx",skip=1)
summary(mcsalandtemp)
head(mcsalandtemp)

#changing column names 

mcsat<-mcsalandtemp%>%
  mutate(yr=year(UTC),
         mnth=month(UTC),
         dy=day(UTC),
         hr=hour(UTC),
         min=minute(UTC))%>%
  filter(min==0)%>%
  select(-min)%>%
  rename(
    temp.c = C,
    sal.ppt = PSU)

write.csv(mcsat,"wdata/lumo6_env.csv",row.names = F)




#DO MC Data - dont run any of this 
#Read in Marine Sal and Temp data
#mcdo<-read_xlsx(path="odata/Mary_MC_DO.xlsx",skip=1)
#summary(mcdo)
#head(mcdo)

#mcdo2<-mcdo%>%
  #mutate(mnth=month(TS),
         #dy=day(TS),
         #hr=hour(TS),
         min=minute(TS))%>%
  filter(min==0)%>%
  select(-min,-...3,-...4,-...5,-...6)%>%
  rename(
    do.mg.l = "mg/L")
mcdo3<-distinct(mcdo2,TS, .keep_all=TRUE)

#join DO with SALTEMp


lumo6_envi<-left_join(mcdo3,mcsat2)

write.csv(bb1fall24,"wdata/bb1fall24_combined.csv",row.names = F)



