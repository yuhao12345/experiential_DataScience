library(tidyverse)
library(ggplot2)
l1<- read.csv('l30.csv', stringsAsFactors = FALSE)
l2<- read.csv('l60.csv', stringsAsFactors = FALSE)
l3<- read.csv('l90.csv', stringsAsFactors = FALSE)
l4<- read.csv('l120.csv', stringsAsFactors = FALSE)
l5<- read.csv('l150.csv', stringsAsFactors = FALSE)
df_l<-rbind(l1,l2,l3,l4,l5)
lt1<- read.csv('lt30.csv', stringsAsFactors = FALSE)
lt2<- read.csv('lt60.csv', stringsAsFactors = FALSE)
lt3<- read.csv('lt90.csv', stringsAsFactors = FALSE)
lt4<- read.csv('lt120.csv', stringsAsFactors = FALSE)
lt5<- read.csv('lt150.csv', stringsAsFactors = FALSE)
df_lt<-rbind(lt1,lt2,lt3,lt4,lt5)

df1<-df_l %>%
  gather(3:11,key = "channel", value = "position")
df2<-df_lt %>%
  gather(3:11,key = "channel", value = "tau")
df_final<-inner_join(df1,df2)
#ggplot(df_final)+
  #geom_point(aes(x=position,y=tau,color=channel))
ggplot(df_final)+
  geom_point(aes(x=position,y=tau,color=channel))+
  facet_wrap(~X0)

df_final %>%
  group_by(X0) %>%
  summarise(mean_pos=mean(position),max_pos=max(position))

df_final2<-df_final %>%
  select(c(2,5)) 
df_final2$X0 <- as.character(df_final2$X0)
ggplot(df_final2, aes(tau, fill = X0)) + 
  geom_density(alpha = 0.2)+
  scale_y_log10()+
  labs(title = "Eigentransmission Distribution", x = "tau", y = "log_distribution")
df_final %>%
  select(c(3,4)) %>%
  ggplot( aes(position, fill = channel)) + 
  geom_density(alpha = 0.2)+
  labs(title = "shape of channel", x = "position", y = "distribution")
df_final %>%
  filter(channel=='X1') %>%
  ggplot()+
    geom_histogram(mapping = aes(x = position), color = 'snow', fill = 'orangered') +
    labs(title = "shape of channel:X1", x = "position", y = "distribution")
