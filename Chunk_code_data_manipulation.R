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
  
#cleaning data--------------------------------------------

library(tidyverse)
data()
View(starwars)
glimpse(starwars)
unique(starwars$gender) #displays data type in the obs of a specific column

starwars$gender <- as.factor(starwars$gender)
class(starwars$gender)#now gender is a factor
class(starwars$gender)

levels(starwars$gender)
starwars$gender <- factor((starwars$gender), levels = c("feminine", "masculine"))
#changing levels

starwars %>% select(name, height, ends_with("color")) %>% 
  names()
unique(starwars$hair_color)

starwars %>% 
  select(name, height, ends_with("color")) %>% 
  filter(hair_color %in% c("blond", "brown") & height < 180)
#%in% works for group more than 1 variable 

#missing data

mean(starwars$height) #we have a NA because there's missin values Na
mean(starwars$height, na.rm = T)

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  na.omit()

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.)) #what obs we deleted

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.)) %>% 
  drop_na(height) 

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hair_color = replace_na(hair_color, "none"))
#replacing all NA values from hair_color

#Duplicates-------------------------------

Names <- c("Peter", "John", "Andrew", "Peter")
Age <- c(22,33,44,22)

friends <- data.frame(Names, Age)
duplicated(friends) #reporting duplicates

friends[!duplicated(friends), ] #the archaic method 

friends %>% distinct() #using tidyverse

#recording variables----------------------------------

starwars %>% select(name, gender)
class(starwars$gender)
starwars$gender <- as.factor(starwars$gender)
class(starwars$gender) #now we can recode the variable
levels(starwars$gender)

starwars %>% 
  select(name, gender) %>% 
  mutate(gender_coded = recode(gender, 
                         "masculine"= 1,
                         "feminine" = 2))

#MANUPULATING--------------------------------------

library(tidyverse)

?msleep
glimpse(msleep)

#rename a variable

msleep %>%  
  rename("conserv"= "conservation") %>% 
  glimpse()

#reorder a variable 

msleep %>%  
  select(vore, name, everything())

#Change a variable name 

class(msleep$vore)
glimpse(msleep)
msleep$vore <- as.factor(msleep$vore)
glimpse(msleep$vore)

msleep %>%  
  mutate(vore = as.character(vore)) %>% 
  glimpse()

#select variables to work with

names(msleep)
msleep %>% 
  select(2:4,
         awake, 
         starts_with("sleep"),
         contains("wt")) %>% 
  names()

#filter and arrange data

unique(msleep$order)

msleep %>%  
  filter((order=="Carnivora" |
          order=="Primates") &
           sleep_total > 8) %>% 
  select(name, order, sleep_total) %>% 
  arrange(-sleep_total) %>% 
  View
  
msleep %>%  
  filter(order %in% c("Carnivora","Primates") &
           sleep_total > 8) %>% 
  select(name, order, sleep_total) %>% 
  arrange(order) %>% 
  View

#change observations (mutate) 

msleep %>% 
  mutate(brainwt_grams = brainwt * 1000) %>% 
  View

#conditional changes(if_else)
#logical vector based on a conditional 

msleep$brainwt > 0.01

size_of_brain <- msleep %>% 
  select(name, brainwt) %>% 
  drop_na(brainwt) %>% 
  mutate(brain_size = if_else(brainwt > 0.01, 
                              "large",
                              "small"))
  View #If it is true then large, if not then small

#Recode data and rename a variable 
##Change obs of "large" and "small" into 

size_of_brain %>%  
  mutate(brain_size = recode(brain_size, 
                             "large"= 1, 
                             "small"=2))

#reshape the data from wide to long or long to wide

install.packages("gapminder")
library(gapminder)
View(gapminder)

data <- select(gapminder, country, year, lifeExp)
data

wide_data <- data %>%  
  pivot_wider(name_from = year, value_from= lifeExp)
View(wide_data)
?pivot_wider







































