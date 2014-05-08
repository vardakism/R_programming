setwd("~/InternetCourses/specialization/2.R_Programming/QA/week1")
hw<-read.csv("hw1_data.csv")
head(hw)
nrow(hw)
tail(hw)
hw$Ozone[47]
sum(is.na(hw$Ozone))
g1<-hw$Ozone[is.na(hw$Ozone)]
g<-hw$Ozone[!is.na(hw$Ozone)]
mean(g)
hws<-subset(hw,hw$Ozone>31 & hw$Temp>90)
mean(hws$Solar.R)
hws2<-subset(hw,hw$Month==6)
mean(hws2$Temp)
hws3<-subset(hw,hw$Month==5)
max(hws3$Ozone,na.rm=T)
