library(readxl)
library(stringr)
library(dplyr)
library(rworldmap)
# df<-read_excel("1.xlsx")
# colnames(df)<-c(1:17)
# df1<-df[-c(1,17)]
# df2<-df1[!(is.na(df1$`3`))&!(is.na(df1$`4`)),]
# 
# df2<-df2 %>%
#   mutate(type = str_replace(df2$`2`, "^\\d{4}[NS]$","1"))
# df3<-df2 %>%
#   filter(type=='1')
# df3<-df3[-c(15,16)]
# colnames(df3)<-c(1:14)
# df4<-df2 %>%
#   filter(type!='1')
# df4<-df4[-c(1,16)]
# colnames(df4)<-c(1:14)
# df5<-rbind(df3,df4)
# df5<-df5 %>%
#   mutate(type = str_replace(df5$`1`, "^\\d{4}[NS]$","1")) %>%
#   filter(type=='1')
# df5<-df5[-c(3,4,5,8,9,13,14,15)]
# colnames(df5)<-c('la','lo','tp_m','tp_d','pr_n','pr_m','pr_d')
# write.csv(df5, "1.csv")
l1<- read.csv('1.csv', stringsAsFactors = FALSE)
l2<- read.csv('2.csv', stringsAsFactors = FALSE)
l3<- read.csv('3.csv', stringsAsFactors = FALSE)
l4<- read.csv('4.csv', stringsAsFactors = FALSE)
l5<- read.csv('5.csv', stringsAsFactors = FALSE)
l6<- read.csv('6.csv', stringsAsFactors = FALSE)
l7<- read.csv('7.csv', stringsAsFactors = FALSE)
l8<- read.csv('8.csv', stringsAsFactors = FALSE)
l9<- read.csv('9.csv', stringsAsFactors = FALSE)
l10<- read.csv('10.csv', stringsAsFactors = FALSE)
l11<- read.csv('11.csv', stringsAsFactors = FALSE)
l12<- read.csv('12.csv', stringsAsFactors = FALSE)
l1<-l1[-1]
l2<-l2[-1]
l3<-l3[-1]
l4<-l4[-1]
l5<-l5[-1]
l6<-l6[-1]
l7<-l7[-1]
l8<-l8[-1]
l9<-l9[-1]
l10<-l10[-1]
l11<-l11[-1]
l12<-l12[-1]

l<-rbind(l1,l2)
l<-rbind(l,l3)
l<-rbind(l,l4)
l<-rbind(l,l5)
l<-rbind(l,l6)
l<-rbind(l,l7)
l<-rbind(l,l8)
l<-rbind(l,l9)
l<-rbind(l,l10)
l<-rbind(l,l11)
l<-rbind(l,l12)
l<-l %>%
  arrange(la, lo) 
l[is.na(l)]<-0
ltemp<-l5
ltemp[6]<-as.numeric(as.character(ltemp[6]))



l_final<-l %>%
  mutate(tp_av=tp_m-tp_d) 
# df<-df %>%
#   mutate(pr_av=pr_m-pr_d)
ll<-l_final[-c(5:7)]  
result<-l_final %>%
  group_by(la,lo) %>%
  summarise(annual_mean_tp=mean(tp_av)) %>%
  filter(annual_mean_tp>18 & annual_mean_tp<30)

final<-result %>%
  mutate(la_1=as.numeric(substr(la,1,2))) %>%
  mutate(la_2=as.numeric(substr(la,3,4))) %>%
  mutate(la_sign=str_replace(la,'\\d+N','1')) %>%
  mutate(la_sign=str_replace(la_sign,'\\d+S','-1')) %>%
  mutate(lo_1=as.numeric(substr(lo,1,3))) %>%
  mutate(lo_2=as.numeric(substr(lo,4,5))) %>%
  mutate(lo_sign=str_replace(lo,'\\d+E','1')) %>%
  mutate(lo_sign=str_replace(lo_sign,'\\d+W','-1')) %>%
  mutate(latitude=(la_1+la_2/60)*as.numeric(la_sign)) %>%
  mutate(longitude=(lo_1+lo_2/60)*as.numeric(lo_sign))

newmap<-getMap(resolution='low')
plot(newmap)
points(final$longitude,final$latitude, col='red',cex=.6)
