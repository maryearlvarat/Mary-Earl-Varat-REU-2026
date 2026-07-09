# thinking about ways to QAQC the environmental data...testing out ideas
# will clean code when workflow is finalized (should this be its own script?)
# or will it get added into existing download --> organization script?

# created by Brianne Du Clos -- 09 July 2026

# code needs tidyverse loaded to run; data comes from dataRetrieval command

# check if provisional or approved
env.data |>
  count(approval_status)
# approval status by variable
table(env.data$approval_status, env.data$unit_of_measure)

# gaps in measurements
# check time interval
# does it start at 00:00ish on the first day?
min(env.data$time)
# does it end at 23:59ish on the last day?
max(env.data$time)
# are there gaps in the measurements?
ggplot(env.data, aes(x=time, y=value))+
  geom_line(aes(linetype=unit_of_measure))

# could check summarized data (though QAQC is supposed to be done before summarizing)
# how many days were samples taken? should be 14
n_distinct(env.data2$dy)
# were samples taken every hour of every day?
# table should be 14 rows long, 24 rows wide; all entries should be 1s
table(env.data2$dy, env.data2$hr)
