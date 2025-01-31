# Script to import, clean, and organize EEC's Beetle data
# date: 25 september 2024

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate) # to deal with dates https://lubridate.tidyverse.org/
# library(here)
# load data ---------------------------------------------------------------
# here()
smpl_pts<-read_csv("./corridor_docs/eric_ms_thesis/ms_data/data_raw/samples.csv")
# dates should always be in standardized format, preferably: yyyymmdd
smpl_pts<-smpl_pts %>% 
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


# save clean info on sample points ----------------------------------------



write_csv(smpl_pts,"./corridor_docs/eric_ms_thesis/ms_data/data_clean/clean_sample_pts.csv")

btl_counts<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","beetle_counts.csv")) %>% 
  mutate(sample_id=as.factor(sample_id)) %>% 
  rowwise() %>%
  mutate(sum = sum(c_across(pvin:ocon), na.rm = TRUE)) %>% 
  mutate(sum=ifelse(sum==0,NA,sum)) %>% 
  replace(is.na(.), 0) %>% 
  left_join(smpl_pts) %>% 
  relocate(c(block,patch, point, month, day,year,date),.after = sample_id)


write_csv(btl_counts,"./corridor_docs/eric_ms_thesis/ms_data/data_clean/clean_btl_counts.csv")




