library(tidyverse)
library(dbplyr)
library(RSQLite)
download.file("https://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data/fission_data.RData?raw=true",
              destfile = "data/fission_data.RData",
              mode = "wb")
load("data/fission_data.RData")
glimpse(norm_cts)
class(norm_cts)
#function to get number of rows
nrow(x=sample_info)
sample_info %>% 
  group_by(strain , minute) %>% 
  summarise(nRep_Min_Strain = n()) %>% 
  ggplot(mapping = aes(x=minute , y= nRep_Min_Strain))+
  geom_point()+
  facet_grid(strain ~.) # if we put .~strain facets will be vertically 
glimpse(sample_info)
class(sample_info)

#How many genes do you have gene expression levels for?
str(trans_cts)
nrow(trans_cts)
class(trans_cts)
dim(trans_cts) # get the size of the matrix
trans_cts[c(1,2,3) ,1:4]
tran_cts_tbl <- as_tibble(trans_cts , rownames ="gene")
norm_cts_tbl <- as_tibble(norm_cts , rownames ="gene")


#gather is a function to transform wide data to long data
trans_cts_long <- tran_cts_tbl %>% 
  gather(key = "sample" , value = "cts" , wt_0_r1:mut_180_r3)
trans_cts_long
distict <- trans_cts_long %>% select (sample) %>% distinct()
norm_cts_long <- norm_cts_tbl %>% gather(key = "sample" , value = "cts" , wt_0_r1:mut_180_r3)
norm_cts_long
#to go from long data to wide data we use a funcetion called spread()
norm_cts_long %>%  spread(key=sample , value =cts)

trans_cts_long
sample_info

#joining the sample info data with gene observation
trans_cts_long <- full_join(x=trans_cts_long,
          y=sample_info,
          by="sample")

trans_cts_long

#are the distributions of the cts values the same
#for each replicate with in the strain/minute

trans_cts_long %>% 
  ggplot(mapping = aes(x=cts , color =replicate)) +
  geom_freqpoly(binwidth =1)+
  facet_grid(strain ~ minute)

#is there any different 

trans_cts_long %>% ggplot(mapping= aes(x=replicate ,
                                       y=cts)) +
  geom_boxplot()+
  facet_grid(strain~minute)
  
 
norm_cts_long <- full_join(x=norm_cts_long,
                           y=sample_info,
                           by="sample")
norm_cts_long %>% 
  ggplot(mapping= aes(x=replicate ,
                      y=cts , color=minute)) +
  geom_boxplot()+
  facet_grid(strain~minute)+
  scale_y_log10()



tran_cts_tbl %>% 
  ggplot(aes(x=wt_0_r1 , y= wt_0_r2))+
  geom_point(color="darkblue", shape=4)+ #the shapes of dots can change  
  geom_abline(color = "red")


tran_cts_tbl %>% 
  ggplot(aes(x=wt_0_r1 , y= wt_30_r1))+
  geom_point(color=wt_30_r1, shape=8)+ #the shapes of dots can change  
  geom_abline(color = "white")


install.packages("corrplot")
library("corrplot")
tran_cts[1:5 , 1:3]
trans_cts_cor <- cor(trans_cts , method= "pearson")
trans_cts_cor[1:5 , 1:5]
corrplot(trans_cts_cor , method = "circle" ,
         cl.lime = c(0.8 , 1) ,is.corr = FALSE)

norm_cts_tbl %>% 
  ggplot(aes(wt_0_r1,wt_0_r2)) +
  geom_point()+
#  xlim(0,65000)+
#  ylim(0, 65000)+
  geom_smooth(method="lm")+
  scale_x_continuous(trans="log2" , limits =c(1, 65000)) +
 scale_y_continuous(trans = "log2", limits =c(1, 65000))

?lm
lm(data =norm_cts_tbl , wt_0_r2 ~ wt_0_r1) #y~x
summary(lm(data =norm_cts_tbl , wt_0_r2 ~ wt_0_r1))

#deSeq2
norm_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts) ,
            var_cts = var(cts)) %>% 
  ggplot(aes(x=mean_cts , y= var_cts)) +
  geom_point()+
  geom_abline(color ="brown") +
  scale_x_continuous(trans = "log2")+
  scale_y_continuous(trans = "log2")
#after DEseq2
trans_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts) ,
            var_cts = var(cts)) %>% 
  ggplot(aes(x=mean_cts , y= var_cts)) +
  geom_point()+
  geom_abline(color ="brown")

#pca
sample_pca <- prcomp(t(trans_cts))
names(sample_pc)
library(ggfortify)

install.packages("ggfortify")

autoplot(sample_pca)
autoplot(sample_pca , data = sample_info , colour = "minute")
pcs <- sample_pca$x
autoplot(sample_pca , x=1 , y=2 , data=sample_info , col= "minute", shape=3)
pcs <- sample_pca$x
sample_info %>%  bind_cols(as.tibble(pcs[,1:10]))

install.packages("broom")
library(broom)
tidy(sample_pca, matrix="pcs")


tidy(sample_pca, matrix="pcs") %>%  
  ggplot(aes(x = PC)) +
  geom_col(aes(y = percent))


#Rmarkdown : to creat documents  







  
