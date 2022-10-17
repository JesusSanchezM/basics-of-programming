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




  
  
