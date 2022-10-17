library(tidyverse)

starwars

#Original data frame
sw_o <- starwars %>% select(name, height, mass, gender)

#Modify data frame___________________________________
sw <- starwars %>% 
  select(name, height, mass, gender) %>% 
  rename(weight = mass) %>% 
  na.omit() %>% 
  mutate(height = height/100) %>% 
  filter(gender %in% c("masculine", "feminine")) %>% 
  mutate(gender = recode(gender, 
                         masculine="M",
                         feminine="F")) %>% 
  mutate(size = height > 1 & weight > 70,
         size = if_else(size == T, "Big", "Small"))
  
View(msleep)

#filter tips_____________________________________

my_data <- msleep %>% 
  select(name, sleep_total, order, bodywt) %>% 
  filter(!sleep_total > 18) %>% #! means the oposite
  filter(order == "Primates" | bodywt > 20) %>% #we can write | to say "or", or use , to say "and"
  filter(name %in% c("Cow", "Dog", "Horse")) %>% 
  filter(between(sleep_total, 2, 4)) %>% #between those numbers
  filter(near(bodywt, 520, tot=2)) #near to that number

my_data <- msleep %>% 
  select(name, sleep_total, conservation) %>% 
  filter(is.na(conservation)) #rows where there is "na" obs

#Exploring data 

starwars
dim(starwars) #87 obs and 14 variables 
str(starwars)
glimpse(starwars)
head(starwars) #first 6 obs
tail(starwars) #last 6 obs

attach(starwars) #this is useful to stop writing starwars$..., now I can write any variable of starwars without specify in which data set I am working
hair_color
names(starwars) #names of my variables
length(starwars) #for a data set length will mean the number of variables
length(hair_color) #for a variable R will tell the number of obs
class(hair_color)
unique(hair_color) #name of unique obs 
#na: data is missing 
#none: hair without a color or there´s no hair
#unknow: we don't know, maybe the character uses a hat, so we don't know the color

table(hair_color)
sort(table(hair_color), decreasing=T)
View(sort(table(hair_color), decreasing=T)) 
barplot(sort(table(hair_color), decreasing=T))

#pipes operators

starwars %>% 
  select(hair_color) %>% 
  count(hair_color) %>% 
  arrange(desc(n)) %>% 
  View()

View(starwars[is.na(hair_color),])  #selecting row where is.na is TRUE

summary(height)
boxplot(height) #boxplot 
hist(height) #histeogram
  
  









