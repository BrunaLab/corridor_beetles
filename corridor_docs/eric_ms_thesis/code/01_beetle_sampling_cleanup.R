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
  mutate(point=as.factor(point)) %>% 
  mutate(month=as.numeric(month)) %>% 
  mutate(day=as.numeric(day)) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(month=case_when(
    block=="53n" & sample_id < 24 ~ "7",
    .default = as.character(month)
  )) %>% 
  mutate(day=case_when(
    block=="53n" & sample_id < 13 ~ "1",
    .default = as.character(day)
  )) %>% 
  mutate(day=case_when(
    block=="53n" & (sample_id >12 & sample_id < 25) ~ "5",
    .default = as.character(day)
  )) %>% 
  mutate(sample_id=as.factor(sample_id)) %>% 
  mutate(date=if_else(is.na(date), as.Date(paste(year,month,year,sep="-")),date))

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
  mutate(sum = sum(c_across(pvin:osyl), na.rm = TRUE)) %>% 
  mutate(sum=ifelse(sum==0,NA,sum)) %>% 
#  select(-c("...18","...19")) %>% 
  replace(is.na(.), 0) %>% 
  left_join(smpl_pts) %>% 
  relocate(c(block,patch, point, month, day,year,date),.after = sample_id) 
  
write_csv(btl_counts,"./corridor_docs/eric_ms_thesis/ms_data/data_clean/clean_btl_counts.csv")




