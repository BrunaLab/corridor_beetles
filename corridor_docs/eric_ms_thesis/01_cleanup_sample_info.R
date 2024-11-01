# Script to import, clean, and organize EEC's Beetle data
# date: 25 september 2024

# load libraries ----------------------------------------------------------

library(tidyverse)
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
