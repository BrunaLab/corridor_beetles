# Script to determine if weights of btls in drying overn have stabilized  
# date: 19 March 2025

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)


# load data ---------------------------------------------------------------

btl_wt<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","btl_weight_equilibrium.csv"))

btl_wt<-btl_wt %>% 
  pivot_longer(wet_weight:weight_5,names_to = "interval") %>% 
  mutate(interval=if_else(interval=="wet_weight","weight_0",interval)) %>% 
  arrange(sample_id,sp_code,interval) %>% 
  group_by(sample_id,sp_code) %>% 
  mutate(perc_loss=((lag(value)-(value))/lag(value)*100)) %>% 
  mutate(sample=paste(sp_code,sample_id,sep="-"))
  
btl_wt


btl_wt %>% 
  # filter(interval!="weight_1") %>%
  # filter(interval!="weight_0") %>%
  ggplot( aes(x=interval, y=value, group=sample, color=sample)) +
  geom_line()



#  btl weights ------------------------------------------------------------


plate_weight<-btl_wt %>% 
  ungroup() %>% 
  filter(interval=="weight_5") %>% 
  filter(sp_code=="plate") %>% 
  select(value)  



corrected_btl_wt<-
btl_wt %>% 
  filter(interval=="weight_5") %>% 
  mutate(plate_weight=plate_weight$value) %>% 
  filter(sp_code!="plate") %>% 
  group_by(sp_code) %>% 
  mutate(corrected_wt=value-plate_weight) %>% 
  summarize(mean_wt=mean(corrected_wt))


btl_wt_clean<-write_csv(corrected_btl_wt,here("corridor_docs","eric_ms_thesis","ms_data","data_clean","corrected_btl_wt.csv"))


