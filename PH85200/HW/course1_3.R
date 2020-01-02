getwd()
lec <- read.csv('lecture_grades.csv', stringsAsFactors = FALSE)
lab <- read.csv('lab_grades.csv', stringsAsFactors = FALSE)
head(lec)
lec[is.na(lec)]<-0
lab[is.na(lab)]<-0
head(lec)
comb<-merge(lec,lab)
write.csv(comb,'lec_lab_grades.csv')

library(tidyverse)
table4a
table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
table4a %>%
  gather(2:3,key='year',value='case')
table4a %>%
  gather(-1,key='year',value='case')
table2
table2 %>%
  spread(key=type,value=count)
table3
table3 %>%
  separate(rate,into=c('case','population'),sep='/')
tidy <- read.csv("lec_lab_grades.csv", stringsAsFactors = FALSE) %>% # read data if you need to
  gather(-c(1:2, Last.Name, First.Name), key = "grade_name", value = "score")
head(tidy)
write.csv(tidy, "scores.csv")
scores <- read.csv('scores.csv', stringsAsFactors = FALSE)
head(scores)
scores %>%
  filter(grade_name == "Lab.2", score == 8)
scores %>%
  filter(grade_name == "Lab.1" | grade_name == "Lab.14", score > 5)
scores %>%
  arrange(desc(score)) %>%
  head(30)
scores %>%
  arrange(Last.Name, First.Name) %>%
  head(30)
scores %>%
  select(First.Name, score) %>%
  head(20)
scores %>%
  select(-(ends_with("Name"))) %>%
  head(20)
scores %>%
  select(-(4:5)) %>%
  head(20)
exams <- scores %>%
  filter(grade_name == "Midterm" | grade_name == "Final.exam") %>%
  mutate(score_norm = score / 10)
head(exams)
exams %>%
  summarise(exam_mean = mean(score), exam_min = min(score), exam_max = max(score))
exams %>%
  group_by(Last.Name, First.Name) %>%
  summarise(mean = mean(score))
scores %>%
  group_by(grade_name) %>%
  filter(score == max(score))
scores %>%
  group_by(grade_name) %>%
  mutate(diff=score-mean(score))
dat <- read.csv("scores.csv", stringsAsFactors = FALSE) 
dat<-dat[,-c(1,2)] 
head(dat)
names(dat)[1]<-'ID'
library(ggplot2)
dat_midterm <- dat %>%
  filter(grade_name == "Midterm")
hist1<-ggplot(data=dat_midterm)+
  geom_histogram(mapping=aes(x=score),binwidth = 5,color='snow',fill='darkblue')
hist1+
  labs(title='midterm score',x='score',y='number')
dat_final <- dat %>%
  filter(grade_name == "Final.exam")
ggplot(data = dat_final) +
  geom_histogram(mapping = aes(x = score), binwidth = 5, color = 'snow', fill = 'orangered') +
  labs(title = "Final Exam Score Distribution", x = "Score", y = "Number of Students")
ggplot(dat_midterm) + #name of the dataset
  geom_col(aes(x = ID, y = score),fill='orange')+
  scale_x_continuous(name='ID',breaks=1:27)+
  ggtitle('midterm')

head(mpg)
?mpg
ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy,color=cty))
ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy))+
  facet_wrap(~class,nrow=2)
ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy))+
  facet_wrap(drv~cyl)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)
tidy <- read.csv("lec_lab_grades.csv", stringsAsFactors = FALSE)
head(tidy)
ggplot(tidy)+
  geom_point(aes(x=Midterm,y=Final.exam))+
  labs(title='correlation',x='mid',y='final')
ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy,color=manufacturer))
ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy))+
  facet_wrap(~manufacturer,nrow=4)

string1 <- "This is a string"
string2 <- "She said \"This is a string\""
writeLines(string2)
string3 <- c("apple", "banana", "carrot")
a <- "apple"
b <- "banana"
c <- "carrot"
string4 <- c(a, b, c)
library(stringr)
str_length(string1)
str_length(string4)
str_c("ba", "nan", "a")
str_c("apple", "banana", "carrot", sep = ", ")
str_c("I have two ", c("apple", "banana", "carrot"), "s")
str_sub(string1, start = 5, end = 10)
str_sub(string4, 1, 3)
str_sub(string4, -3, -1)
str_sub(string4, 1, 1) <- str_to_upper(str_sub(string4, 1, 1))
string4
str_sub(string4, 1, 1) <- str_to_lower(str_sub(string4, 1, 1))
string4
str_view(string4, "an")
str_view(string4, "a.")
str_view_all(string4, "a.")
str_view(c("abc", "a.c", "bef"), "a\\.c")
writeLines("\\")
x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")
x <- c("apple pie", "green apple", "apple")
str_view(x, "^apple")
str_view(x, "apple$")
str_view(x, "^apple$")
str_view(c("2 apples", "5 bananas", "4 carrots"), "\\d")
str_view(c("grey", "gray"), "gr(e|a)y")
str_view(c("color", "colour"), "col(ou|o)r")
str_view(c("color", "colour"), "colo(u)?r")
str_view(c("apple", "banana"), "(na)+")
str_view(c("10 apples", "15 bananas", "40 carrots"), "\\d+")
str_view(c("apple", "banana"), "(na){1}")
str_view(c("apple", "banana"), "p{1,2}")
str_view(c("apple", "banana"), "(na)+?")
str_count("mississippi", "s")
str_count("mississippi", "[sip]")
str_count("mississippi", "sip")
str_split(string1, " ")
x <- c("four apples", "two bananas", "five carrots")
str_replace(x, "a", "A")
str_replace_all(x, c("four" = "4", "two" = "2", "five" = "5"))
grep("p*", c("apple", "orange", "grape", "pineapple"))
grepl("p+", c("apple", "orange", "grape", "pineapple"))
head(words)
str_view(words,'^y',match=TRUE)
str_view(words,'^...$',match=TRUE)
str_view(words, "^[aeiou]", match = TRUE)
str_view(words, "^[^aeiou]{3}", match = TRUE)
str_view(words, "([aeiou][^aeiou]){2,}", match = TRUE)
mean(str_count(words, "[aeiou]"))

library(tidyverse)
scores <- read.csv("scores.csv", stringsAsFactors = FALSE)
head(scores)
scores <- scores[,-c(1,2)]
scores <- scores %>%
  mutate(grade_type = str_replace(grade_name, "\\d+","")) %>%
  mutate(grade_type = str_replace(grade_type, ".Ch.","")) %>% # just to make it prettier
  mutate(grade_type = str_replace(grade_type, ".ch.","")) %>%
  mutate(grade_type = str_replace(grade_type, "Lab.", "Lab"))
lab_ranked <- scores %>%
  group_by(Last.Name, First.Name) %>%
  filter(grade_type == "Lab") %>%
  mutate(rank = row_number(score)) %>%
  arrange(Last.Name, First.Name, rank)
lab_dropped <- lab_ranked %>%
  filter(rank > 2)
scores_lab <- lab_dropped %>%
  summarise(lab_grade = mean(score) * 2) # lab weight 20%
scores_exam <- scores %>%
  group_by(Last.Name, First.Name) %>%
  filter(grade_name == "Midterm" | grade_name == "Final.exam" | grade_name == "Extra.Credit") %>%
  summarise(exam_weighted = sum(score) * 0.25) # 25% weight on midterm and final
scores_hw <- scores %>%
  group_by(Last.Name, First.Name) %>%
  filter(grade_type == "HW") %>%
  summarise(hw_weighted = mean(score) * 2)
scores_quiz <- scores %>%
  group_by(Last.Name, First.Name) %>%
  filter(grade_type == "Quiz") %>%
  summarise(quiz_weighted = mean(score))
df<-list(scores_lab,scores_exam,scores_hw,scores_quiz) %>%
  Reduce(function(dtf1,dtf2) inner_join(dtf1,dtf2,by=c('Last.Name','First.Name')), .)
df$grade_all<- rowSums( df[,3:6] )
df<-df[,-c(3:6)]
df$grade = cut(df$grade_all, 
                    breaks = c(0,40,60,80,90, 100), 
                    labels = c("F", "D", "C", "B", "A"),
                    right = FALSE, 
                    include.lowest = TRUE)
df<-df %>%
  arrange(Last.Name, First.Name)

library(forcats)
x1 <- c("Dec", "Apr", "Jan", "Mar")
sort(x1)
x2 <- c("Dec", "Apr", "Jam", "Mar")
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
y1
sort(y1)
factor(x1)
y2 <- factor(x2, levels = month_levels)
y2
levels(y2)
f1 <- factor(x1, levels = unique(x1))
f2 <- x1 %>% factor() %>% fct_inorder()
f2
head(gss_cat)
gss_cat %>%
  count(marital)
ggplot(gss_cat, aes(marital)) +
  geom_bar(fill = "purple")
gss_cat %>%
  mutate(marital = marital %>% fct_infreq()) %>%
  ggplot(aes(marital)) +
  geom_bar(fill = "purple")
gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar(fill = "purple")
relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(relig_summary, aes(tvhours, relig)) + geom_point()
relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()
gss_cat %>% count(partyid)
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat"
  )) %>%
  count(partyid)
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat",
                              "Other" = "No answer",
                              "Other" = "Don't know",
                              "Other" = "Other party"
  )) %>%
  count(partyid)
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)
gss_cat %>%
  ggplot(aes(rincome)) +
  geom_bar()
gss_cat %>%
  mutate(rincome = fct_recode(rincome,
                              "other" = "No answer",
                              "other" = "Don't know",
                              "other" = "Refused",
                              "other" = "Not applicable"
  )) %>%
  ggplot(aes(rincome)) +
  geom_bar() +
  coord_flip()
dat <- read.csv("scores.csv", stringsAsFactors = FALSE)
dat_lab <- dat %>%
  filter(grepl("^Lab\\.(10|[6789])", grade_name))
dat_lab$grade_name <- factor(dat_lab$grade_name, levels = c("Lab.6", "Lab.7", "Lab.8", "Lab.9","Lab.10"))
ggplot(dat_lab) +
  geom_col(aes(x = grade_name, y = score), fill = "chocolate") +
  facet_wrap(~ X) +
  coord_flip() +
  labs(x = "", y = "score [%]")
