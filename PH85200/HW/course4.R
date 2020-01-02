library(dplyr)
score <- read.csv('scores.csv', stringsAsFactors = FALSE)
score<-score[,-(1:2)] %>%
  arrange(Last.Name,First.Name) %>%
  mutate(grade_type=sub(pattern='(\\..h\\.\\d+|\\.\\d+)',replacement='',x=grade_name))
lab_dropped<-score %>%
  group_by(Last.Name,First.Name) %>%
  filter(grade_type=='Lab') %>%
  mutate(rank=row_number(score)) %>%
  arrange(Last.Name,First.Name,rank) %>%
  filter(rank>2) %>%
  summarise(lab_grade=mean(score)*2)
scores_exam<-score %>%
  group_by(Last.Name,First.Name) %>%
  filter(grade_name=='Midterm'|grade_name=='Final.exam'|grade_name=='Extra.Credit') %>%
  summarise(exam_weighted=sum(score)*0.25)
scores_hw<-score %>%
  group_by(Last.Name,First.Name) %>%
  filter(grade_type=='HW') %>%
  summarise(hw_weighted=sum(score)*0.2)
scores_quiz<-score %>%
  group_by(Last.Name,First.Name) %>%
  filter(grade_type=='Quiz') %>%
  summarise(hw_weighted=sum(score)*0.1)
final_grade<-inner_join(lab_dropped,scores_exam) %>%
  inner_join(scores_hw)

library(forcats)
x1<-c('Dec','Apr','Jan','Mar')
sort(x1)
x2<-c('Dec','Apr','Jam','Mar')
month_levels<-c(
  'Jan','Feb','Mar','Apr','May','Jun',
  'Jul','Aug','Sep','Oct','Nov','Dec'
)
y1<-factor(x1,levels=month_levels)
y1
sort(y1)
factor(x1)
y2<-factor(x2,levels=month_levels)
y2
levels(y2)
f1<-factor(x1,levels=unique(x1))
f1
f2<-x1 %>% fct_inorder()
f2
head(gss_cat)
str(gss_cat)
attr(gss_cat$marital,'levels')
gss_cat %>%
  count(marital)
gss_cat %>%
  count(relig)
gss_cat %>%
  count(partyid)
ggplot(gss_cat,aes(marital))+
  geom_bar(fill='blue')
gss_cat %>%
  mutate(marital=marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital))+
  geom_bar(fill='purple')
relig_sum<-gss_cat %>%
  group_by(relig) %>%
  summarise(
    tvhours=mean(tvhours,na.rm = TRUE),
    n=n()
  )
ggplot(relig_sum,aes(tvhours,relig))+geom_point()
relig_sum %>%
  mutate(relig=fct_reorder(relig,tvhours)) %>%
  ggplot(aes(tvhours,relig)) +
  geom_point()
gss_cat %>%
  count(partyid)
library(ggplot2)
gss_cat %>%
  mutate(rincome=rincome %>% fct_infreq()) %>%
  ggplot(aes(rincome))+
  geom_bar(fill='blue')+
  coord_flip()

df <- read.csv('Advertising.csv', stringsAsFactors = FALSE)
head(df)
ggplot(data=df)+
  geom_point(aes(x=radio,y=sales))
lm.fit<-lm(sales~radio,data=df)
summary(lm.fit)
lm.fit<-lm(sales ~ .,data=df)
summary(lm.fit)

ggplot(data=mpg)+
  geom_smooth(aes(x=displ,y=hwy),se=FALSE)+
  geom_point(aes(x=displ,y=hwy))
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_smooth()+
  geom_point(mapping=aes(color=class))
pl<-ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_smooth(method='lm',formula=y~x)+
  geom_point()
pl
m<-lm(hwy~displ,mpg)
a<-signif(coef(m)[1],digits=2)
b<-signif(coef(m)[2],digits=2)
eqtext<-paste('y=',b,'x+',a,sep='')
eqtext
pl+
  annotate('text',x=6,y=40,label=eqtext,color='blue',size=5)
ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=drv))+
  geom_smooth(method='lm',formula=y~poly(x,2))+
  geom_point()
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_smooth(method='lm',formula=y~poly(x,2))+
  geom_point()
ggplot(data=mpg,mapping=aes(x=displ,y=hwy,group=drv))+
  geom_smooth(method='lm',formula=y~poly(x,2))+
  geom_point()
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_smooth(method='lm',formula=y~poly(x,2))+
  geom_point(aes(color=drv))
ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=drv))+
  geom_smooth(se=FALSE)+
  geom_point()

head(iris)
tail(iris)
?iris
library(ggplot2)
ggplot(data=iris,mapping=aes(x=Sepal.Length,y=Sepal.Width))+
  geom_point()+
  facet_wrap(~Species)
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width))+
  geom_point()+
  facet_wrap(~Species)
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width))+
  geom_point(aes(color=Species))

set.seed(20)
iriscluster<-kmeans(iris[,3:4],3,nstart = 20)
iriscluster
table(iriscluster$cluster,iris$Species)

iriscluster$cluster<-as.factor(iriscluster$cluster)
ggplot(iris,aes(Petal.Length,Petal.Width,color=iriscluster$cluster))+
  geom_point()

set.seed(25)
iriscluster1<-kmeans(iris[,1:2],3,nstart = 20)
ggplot(iris,aes(Sepal.Length,Sepal.Width,color=iriscluster1$cluster))+
  geom_point()
table(iriscluster1$cluster,iris$Species)

wssplot <- function(data, nc=15, seed=123){ 
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){ 
    set.seed(seed) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")}
wssplot(iris[,3:4])
  
library(ISLR)
head(Smarket)
str(Smarket)
cor(Smarket[,-9])

attach(Smarket)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family = binomial,subset=train)
glm.fit
glm.probe<-predict(glm.fit,Smarket.2005,type='response')
head(glm.probe)
contrasts(Direction)
glm.pred<-rep('Down',252)  #replace all label to 'down'
glm.pred[glm.probe>.5]='Up'
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family = binomial,subset=train)
glm.probe<-predict(glm.fit,Smarket.2005,type='response')
head(glm.probe)
contrasts(Direction)
glm.pred<-rep('Down',252)
glm.pred[glm.probe>.5]='Up'
table(glm.pred,Direction.2005)
glm.pred

library(MASS)
lda.fit<-lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)
tail(lda.pred$posterior)
tail(lda.pred$x)
mean(lda.pred$x[lda.pred$class=='up'])

attach(Smarket)
train=(Year>2001)
Smarket.2001=Smarket[!train,]
dim(Smarket.2001)
Direction.2001=Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family = binomial,subset=train)
glm.fit
glm.probe<-predict(glm.fit,Smarket.2001,type='response')
head(glm.probe)
contrasts(Direction)
glm.pred<-rep('Down',242)
glm.pred[glm.probe>.5]='Up'
table(glm.pred,Direction.2001)

lda.fit<-lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit,Smarket.2001)
names(lda.pred)
tail(lda.pred$posterior)
tail(lda.pred$x)
mean(lda.pred$x[lda.pred$class=='up'])


library(tidyverse)
library(maps)
usa<-map_data('usa')
ggplot(data=usa)+
  geom_polygon(mapping=aes(x=long,y=lat,group=group))+
  coord_fixed(1,3)
ggplot(data=usa)+
  geom_polygon(mapping=aes(x=long,y=lat,group=group),fill='blue',color='red')+
  coord_fixed(1,3)
states<-map_data('state')
head(states)
ggplot(data=states)+
  geom_polygon(mapping=aes(x=long,y=lat,fill=region,group=group),color='white')+
  coord_fixed(1,3)+
  guides(fill=FALSE)+
  coord_flip()
ny<-subset(states,region='new york')
ggplot(data=ny)+
  geom_polygon(mapping=aes(x=long,y=lat,fill=region,group=group),color='white')+
  coord_fixed(1,3)+
  guides(fill=FALSE)
counties<-map_data('county')
ny_county<-subset(counties,region=='new york')
cities
nycounties<-ny+
  geom_polygon(data=ny_county,aes(x=long,y=lat,group=group))+
  geom_point(data=cities,aes(x=long,y=lat),color='red')



