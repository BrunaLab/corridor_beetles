# Script to import, clean, and organize EEC's Beetle data
# date: 25 september 2024

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate) # to deal with dates https://lubridate.tidyverse.org/
library(vegan)

# load data ---------------------------------------------------------------

btl_data<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_clean","clean_btl_counts.csv"))


# quick summaries to look over the sampling point information ---------------
# how many points in each block, how many points per treatment, etc.

# names(btl)
# 
# smpl_pts %>% 
#   group_by(block) %>% 
#   tally()
# 
# smpl_pts %>% 
#   group_by(block,patch) %>% 
#   tally()
# # what's up with 52?
# smpl_pts %>% 
#   group_by(block,patch,point) %>% 
#   tally() 
# 
# smpl_pts %>% 
#   group_by(block,patch,point) %>% 
#   tally() %>% 
#   filter(block=="52")
# 
# smpl_pts %>% 
#   group_by(date,block) %>% 
#   tally() 
# 
# smpl_pts %>% 
#   group_by(date,block) %>% 
#   tally() 



# summary data ------------------------------------------------------------


# total beetles captured --------------------------------------------------


total_N<-btl_data %>% summarise(n=sum(sum, na.rm=TRUE))


# total N by species ------------------------------------------------------


N_by_spp<-btl_data %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  select(-c(sample_id,point,month,day,year,sum)) %>% 
  pivot_longer(pvin:ocon,names_to = "species",values_to = "n") %>% 
  arrange(desc(n))
  














# species diversity -------------------------------------------------------



btl_summary<-btl_counts %>% 
  select(-c(sample_id,block,point,month,day)) %>% 
  group_by(patch) %>% 
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>% 
  filter(!is.na(patch)) %>% 
  select(-sum) %>% 
  column_to_rownames("patch")



summarize(sum=sum(c_across(pvin:ocon), na.rm = TRUE)) %>% 
  filter(!is.na(patch))
btls<-btl_summary %>% 
  
  
  
  
  
  btls<-btl_counts %>% 
  select(-c(sum,sample_id,block,point, month, day)) %>% 
  # column_to_rownames("sample_id")
  column_to_rownames("patch")







site_type <- env %>% 
  select(block, patch)
# How speciose are my communities?

# specnumber() will tell you the number of species within each sample. 
# You can then run an analysis of variance to ask if mean species richness 
# is significantly different across sites.

sppr <- specnumber(btl_summary)

# analysis of variance takes the same form as the usual models you'd see in R
# response ~ dependent, data = environmental grouping
# sppr_aov <- aov(sppr ~ patch, data = site_type)
# summary(sppr_aov)

shannondiv <- diversity(btl_summary)
head(shannondiv)

# rarefaction



btl_summary<-btl_counts %>% 
  select(-c(patch,block,point,month,day)) %>% 
  # select(-c(sample_id,block,point,month,day)) %>% 
  # group_by(patch) %>% 
  filter(sum>0) %>% 
  select(-sum) %>% 
  column_to_rownames("sample_id")



spAbund <- rowSums(btl_summary)  #gives the number of individuals found in each plot
spAbund # view observations per plot 
# ufl graduation deadlines graduate school

#rarefaction uses the smallest number of 
# observations per sample to extrapolate the expected number if all other 
# samples only had that number of observations
raremin <- min(rowSums(btl_summary))  
raremin # view smallest # of obs 


sRare <- rarefy(btl_summary, raremin) # now use function rarefy
sRare #give
rarecurve(btl_summary, col = "blue") 


