library(jsonlite)
margarita<-fromJSON('https://www.thecocktaildb.com/api/json/v1/1/search.php?s=margarita')
str(margarita)
margarita$drinks
gin<-fromJSON('https://www.thecocktaildb.com/api/json/v1/1/filter.php?i=Gin')
gin_df<-as.data.frame(gin)
#rm(gin)
#rm(gin_df)
str(gin_df)

library(stringr)
library(dplyr)
data_base<-read.csv('database.csv',head=T,stringsAsFactor=FALSE)
head(data_base)
total_na<-sum(is.na(data_base))
total_na
total_entries<-nrow(data_base)
data_base<-read.csv('database.csv',header=T,na.strings=c('',' ','/t','\n',NA),stringsAsFactor=FALSE)
data_base[is.na(data_base)]<-0
db<-data_base %>%
  filter(drinks.strIngredient8==0)
db<-db %>%
  select(cocktail.name=drinks.strDrink,drinks.strIngredient1:drinks.strIngredient7,
         drinks.strMeasure1:drinks.strMeasure7)

library(tidyverse)
db1<-db %>%
  gather(~cocktail.name,key='key',value = 'value') %>%
  mutate(type=str_replace())

tc<-read.csv('tidy_cocktails.csv',head=T,stringsAsFactor=FALSE)
tc1<-tc %>%
  count(Ingredient) %>%
  arrange(desc(n)) 
n1<-tc1$n
tc1 %>%
  filter(n>n1[11])

tc %>%
  group_by(Ingredient) %>%
  summarise(N=n()) %>%
  arrange(desc(N)) %>%
  head(10)

library(rvest)
html<-read_html('http://www.imdb.com/title/tt1490017/')
cast<-html %>%
  html_nodes('#titleCast .itemprop ,#titleCast h2') %>%
  html_text()
  

logo_movie<-html('http://www.imdb.com/title/tt1490017/')

# logo_movie %>%
#html_node('strong span') %>%

# make sql requries
library(dbplyr)
library(dplyr)
library(RSQLite)
con<-DBI::dbConnect(RSQLite::SQLite(),path=':memory:')

library(nycflights13)
copy_to(con, nycflights13::flights,'flights',
        temporary=FALSE,
        indexes=list(
          c('year','month','day','carrier','tailnum','dest')
        ))
flights_db<-tbl(con,'flights')
flights_db
avg_del<-flights_db %>%
  group_by(dest) %>%
  summarise(delay=mean(dep_time))
avg_del

# collect() to pull full database
dbDisconnect(con)
