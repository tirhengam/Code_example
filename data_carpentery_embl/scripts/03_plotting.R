glimpse(surveys_compelete)


#plotting with ggplot2

# general structure for creating a plot
# ggplot(data = myDataFrame , mapping = aes(<How plot is made) + <type of plot> )

# fiest plot is xy scattered plot
ggplot(data = surveys_compelete , mapping = aes(x= weight , y = hindfoot_length)) +
  geom_point()
# + sign to build plots4
# %>%  sign to pass values in between functions such as select() , filter() etc
# add colour to my plot to distinguish different anomals
ggplot(data = surveys_compelete , mapping = aes(x= weight , y = hindfoot_length)) +
  geom_point(mapping = aes(color = genus))

ggplot(data = surveys_compelete , mapping = aes(x= weight , y = genus)) +
  geom_point(mapping = aes(color = plot_type))
# box plot
# do the genera  have different weights?
surveys_compelete %>% 
  ggplot(mapping = aes(x=genus , y= weight , color = genus)) +
  geom_jitter(height=0,alpha=0.3)+  #jitter only through width not height
  geom_boxplot()
  

#answer to challenges
surveys_compelete %>% 
  ggplot(mapping = aes(x=genus , y= weight , color = genus)) +
  #geom_jitter(height=0,width =0 ,alpha=0.3)+  
  geom_violin() 

?scale_y_log10
surveys_compelete %>% 
  ggplot(aes(genus, weight, color = genus)) +
  geom_violin() +
  scale_y_log10()

#have the genus abundances changed over the years?
#how many observations per genus per year

surveys_compelete %>%  
  group_by(year,genus) %>% 
  summarise(countsPerGenusPerYear=n()) %>% 
  ggplot(mapping = aes(x=year,y=countsPerGenusPerYear))+
  geom_line(mapping = aes(color = genus))+
  geom_point() # drow aline base on year and x

#to add colour to line, color in aes will affect all geom in the plot
surveys_compelete %>%  
  group_by(year,genus) %>% 
  summarise(countsPerGenusPerYear=n()) %>% 
  ggplot(mapping = aes(x=year,y=countsPerGenusPerYear,color = genus))+
  geom_line()+
  geom_point() # drow aline base on year and x


surveys_compelete %>% 
  ggplot(aes(species_id, weight)) +
  geom_violin(aes(fill = genus)) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_y_log10()

#challenge 5 
results8
surveys_compelete %>%  
  group_by( genus ,month) %>% 
  summarise(weightpermonth=mean(weight , na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x=month,y=weightpermonth ,color = genus ))+
  geom_line()


#facets to split one plot into  multiple subplots
surveys_compelete %>%  
  group_by(year,genus) %>% 
  summarise(countsPerGenusPerYear=n()) %>% 
  ggplot(mapping =aes(x=year , y= countsPerGenusPerYear))+
  geom_line()+
  geom_point()+
  facet_wrap(.~genus)


surveys_compelete %>%  
  group_by( genus ,month) %>% 
  summarise(weightpermonth=mean(weight , na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x=month,y=weightpermonth ,color = genus ))+
  geom_line()+
  facet_wrap(.~genus)

year_count_sex <- surveys_compelete %>% 
  group_by(year , genus , sex) %>% 
  summarise(countsPerGenusPerYearPerSex= n())
glimpse(year_count_sex)  
ggplot(data=year_count_sex , 
       mapping = aes(x=year , y=countsPerGenusPerYearPerSex , color=sex)) +
  geom_line()+facet_wrap(.~genus)

#challenge 7  
year_count_sex <- surveys_compelete %>% 
  group_by(year , species_id , sex) %>% 
  summarise(countsPerSpeciesPerYearPerSex= n())
glimpse(year_count_sex)  
ggplot(data=year_count_sex , 
       mapping = aes(x=year , y=countsPerSpeciesPerYearPerSex , color=sex)) +
  geom_line()+facet_wrap(.~species_id)

#challenge8  
surveys_compelete %>%  
  group_by( genus ,year ,sex) %>% 
  summarise(weighPerYear=mean(weight , na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x=year,y=weighPerYear ,color = genus ))+
  geom_line()+
  facet_grid( sex~genus) #?

#numbers of animal for Onychomys","Perognathus change from month to month 

countsPerMonthPerYear <- surveys_compelete %>% 
  filter(genus %in% c("Onychomys","Perognathus")) %>% 
  group_by(year,month,genus) %>% 
  summarise(observationsPerMonthPerGenusPerMonthPerYear=n())
ggplot(data =countsPerMonthPerYear,
       mapping =aes(x=month , y=observationsPerMonthPerGenusPerMonthPerYear))+
  geom_boxplot(aes(group=month))+
  facet_wrap(.~genus)

month.abb
month.abb[2]
month.abb[c(2,4,8)]
countsPerMonthPerYear$month %>% head()
month.abb[countsPerMonthPerYear$month]%>% head()

countsPerMonthPerYear %>% mutate(month_names =month.abb[month]) %>% 
  ggplot(mapping = aes(x=month_names , 
                       y=observationsPerMonthPerGenusPerMonthPerYear ,
                       group=month_names)) +
  geom_boxplot()+
  facet_wrap(.~genus)

#Factors
#create a new factor

sexes <- factor(c("male" , "female" , "female" ,"male"))
levels(sexes)
#changing the order of levels in a factor
sexes <- factor(c("male" , "female" , "female" ,"male") , 
          #ordered =TRUE, #only use when there is a less than order 
          levels=c("male" ,"female"))

sexes
#now we want order the months so not just a vector but also a factor

weightPlot <- countsPerMonthPerYear %>% 
  mutate(monthNames = month.abb[month]) %>% 
  mutate(monthNamesOrdered = factor(monthNames ,
                                    levels=month.abb)) %>% 
  ggplot(aes(x=monthNamesOrdered,
             y=observationsPerMonthPerGenusPerMonthPerYear,
             group=monthNamesOrdered))+
  geom_boxplot()+
  facet_wrap(.~genus)+
  labs(x="Months" ,
       y="Counts" ,
       title="Weigh Change across months",
       tag='A')

# labs meand labels when we wnat to put lables for x and y

ggsave(filename = "figurs/weightPlot.png", plot=weightPlot, width =15, height = 10 , dpi=20)




