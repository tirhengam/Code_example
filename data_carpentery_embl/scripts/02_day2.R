



surveys %>%
  count(plot_type) 
surveys


results5 <-  surveys %>%  
  group_by(species_id) %>% 
  summarise(min = min(hindfoot_length , na.rm =TRUE) ,
            max = max(hindfoot_length , na.rm = TRUE) , 
            count = n() ,
            count2 = sum(!is.na(hindfoot_length))) 


results6 <-  surveys %>%    
  group_by(year) %>% 
  summarise(max = max(weight , na.rm = TRUE)) 


result7 <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weightz == max(weight)) %>%
  select(year, genus, species, weight) %>%
  arrange(year)
 
#prepare data for plotting



surveys_compelete <- surveys %>%  
                      filter(!is.na(weight) , !is.na(hindfoot_length) , !is.na(sex))
species_count <-  surveys_compelete %>%  count(species_id) %>% filter(n>=50)

surveys_compelete <-  surveys_compelete %>%  filter(species_id %in% species_count$species_id)                             
surveys_compelete

write.csv(surveys_compelete ,"data/surveys_compelete.csv")

