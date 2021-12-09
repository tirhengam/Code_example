# Database lesson - data
download.file(url = "https://ndownloader.figshare.com/files/2292171", destfile = "data/portal_mammals.sqlite", mode = "wb")
library(tidyverse)
library(dbplyr)
library(RSQLite)

#connecting to our database
mammals_con <- dbConnect(SQLite() , 
                         "data/portal_mammals.sqlite")
#what table are in db
src_dbi(mammals_con)
tbl(mammals_con , "plots")
tbl(mammals_con , "species")
tbl(mammals_con , "surveys")

surveys <- mammals_con %>% tbl("surveys") %>% 
  filter(weight<5) %>% 
  select(species_id , weight, plot_id) %>% 
  show_query()#a cheat to see the select


plots_table <- tbl(mammals_con , "plots")
#this output is List and can not be used for plotting

surveys %>% left_join(plots_table , by ="plot_id")  %>%  collect()
#output of collect is a data frame that we can be used as input for plotting



