#links:
#https://tavareshugo.github.io/2019-01-29-EMBL/
#https://datacarpentry.org/R-ecology-lesson/
#https://tavareshugo.github.io/data_carpentry_extras/recap_intro_r/recap_intro_r.html
#https://pad.carpentries.org/2019-01-29-EMBL

w <- c(10,20,30,40)
a <- c("cat" ,"dog")

heights <- c(63, 69, 60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63, 63, NA, 72, 65, 64, 70, 63, 65)
heights <- heights[!is.na(heights)]
mean(heights)
above_tresh <- length(heights[heights>67])
download.file(url="https://ndownloader.figshare.com/files/2292169", destfile = "data/portal_data_joined.csv")

install.packages("tidyverse")

library(tidyverse)

surveys <- read_csv("data/portal_data_joined.csv")
dim(surveys)
nrow(surveys)
ncol(surveys)
str(surveys)
glimpse(surveys)
print(surveys)
names(surveys)
result =surveys[1,2]
result =surveys[1,]
surveys_part <- surveys[1:20 ,1:3]
summary(surveys[,1])
surveys[,2]
table(surveys[,2])
surveys$month
colna <- "month"
surveys[,colna]



#select columns
select(surveys, month , species_id , weight)
summary(select(surveys, month , species_id , weight))

#select rows
filter(surveys, year == 1995)


result =  filter(surveys, year >= 1995)
write.csv(result , "data/first_excersize.csv")


#For further reading on biological data analysis:
#https://www.huber.embl.de/msmb/
  
#  On statistical modeling (machine learning) for data analysis:
#  http://www-bcf.usc.edu/~gareth/ISL/
  
#  Andy Fields: Introduction to Statistics Using R


surveys %>% filter(weight <5 )
# equal to
filter(surveys , weight<5)

surveys_subset <- surveys %>% filter(weight<5) %>% select(species_id , sex , weight)

#ctrl+shift+m : pipe  %>%  operator to chain commands , the left command is input of right command
c(1.111, 2.222 , 3.3333) %>%  round(digits = 2)

surveys_subset <- surveys %>% filter(year<1995) %>% select(species_id , sex , weight)
#mutate to add columns 

surveys %>% mutate(weight_kg =weight/1000) %>%  select(weight , weight_kg)

excersize3 <-  surveys  %>% mutate(hint_foot_half =hindfoot_length/2) %>% filter(!is.na(hint_foot_half) , hint_foot_half<30 & hint_foot_half>15) %>% select(species_id , hint_foot_half)

#everything() return all the coulumns except the one already we mentioned



 #split-apply-combine

results4 <-  surveys %>%  
              group_by(sex , species_id) %>% 
              summarise(mean_weight = mean(weight , na.rm =TRUE) , var_weight = var(weight , na.rm = TRUE) , n_obs = n()) %>% arrange(desc(mean_weight)) 

complete.cases() # to remove all NA s

surveys %>%  count(sex)
#equal to
surveys %>% group_by(sex) %>% summarise(n_obs=n())


surveys %>%
  count(sex) 
#equal to
surveys %>%
  group_by(sex) %>%
  summarise(count = n())
