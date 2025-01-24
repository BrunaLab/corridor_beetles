# Script to import, clean, and organize EEC's Beetle data
# date: 25 september 2024

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate) # to deal with dates https://lubridate.tidyverse.org/
# library(here)
# load data ---------------------------------------------------------------
# here()
btl<-read_csv("./corridor_docs/eric_ms_thesis/ms_data/data_raw/samples.csv")
# dates should always be in standardized format, preferably: yyyymmdd
btl<-btl %>% 
  mutate(year=2024) %>% 
  mutate(date = make_datetime(year, month, day)) %>% 
  mutate(across(where(is.character), ~as.factor(.))) %>% 
  mutate(sample_id=as.factor(sample_id)) %>% 
  mutate(point=as.factor(point)) %>% 
  mutate(month=as.numeric(month)) %>% 
  mutate(day=as.numeric(day)) %>% 
  mutate(year=as.numeric(year))

# fill in the missing info for samples 1:24 -------------------------------




# quick summaries to look over the data -----------------------------------

names(btl)

btl %>% 
  group_by(block) %>% 
  tally()

btl %>% 
  group_by(block,patch) %>% 
  tally()
# what's up with 52?
btl %>% 
  group_by(block,patch,point) %>% 
  tally() 

btl %>% 
  group_by(block,patch,point) %>% 
  tally() %>% 
  filter(block=="52")


btl %>% 
  group_by(date,block) %>% 
  tally() 

btl %>% 
  group_by(date,block) %>% 
  tally() 

# save the clean dataset --------------------------------------------------

write_csv(btl,"./corridor_docs/eric_ms_thesis/ms_data/data_clean/clean_sample_info.csv")


# 
# btl<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","samples.csv"))
#   

# load beetle id data -----------------------------------------------------
library(vegan)
library(ggvegan)
# vegan cheatsheet: https://www.rpubs.com/an-bui/vegan-cheat-sheet



env <- read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","samples.csv")) %>% 
  mutate(sample_id=as.factor(sample_id)) 

btl_counts<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","beetle_counts.csv")) %>% 
  mutate(sample_id=as.factor(sample_id)) %>% 
  rowwise() %>%
  mutate(sum = sum(c_across(pvin:ocon), na.rm = TRUE)) %>% 
  mutate(sum=ifelse(sum==0,NA,sum)) %>% 
  replace(is.na(.), 0) %>% 
  left_join(env) %>% 
  relocate(c(block,patch, point, month, day),.after = sample_id)

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