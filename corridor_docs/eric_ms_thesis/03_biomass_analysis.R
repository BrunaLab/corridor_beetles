# Script to import, clean, and organize EEC's Beetle data
# date: 25 september 2024

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
# load data ---------------------------------------------------------------


btl_data<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_clean","clean_btl_counts.csv"))
spp_codes<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","species_codes.csv"))

spp_bmass<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","spp_weights.csv")) %>% 
  group_by(sp_code) %>% 
  summarize(avg_bmass=mean(weight),
            sd=sd(weight),
            n_btls=n_distinct(weight)) %>% 
  arrange(desc(avg_bmass))

biomass<-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>%
  group_by(species,patch,block) %>%
  mutate(patch=case_when(
    patch == "c" ~ "Connected",
    patch == "m" ~ "Matrix",
    patch == "w" ~ "Winged",
    patch == "r" ~ "Rectangle",
    .default = as.character(patch))) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(patch,desc(n)) %>%
  rename(sp_code=species) %>% 
  left_join(spp_bmass,by="sp_code") %>% 
  select(-sd, -n_btls) %>% 
  mutate(spp_biomass=n*avg_bmass)

b2<-biomass %>%
  ungroup() %>% 
  select(-c(sp_code,n,avg_bmass)) %>% 
  group_by(patch,block) %>%
  filter(is.na(spp_biomass)==FALSE) %>% 
  summarise(total_bmass=sum(spp_biomass))


ggplot(b2, aes(
  x=patch, 
  y=total_bmass,
  color=block)
  ) + 
  geom_point()


  mutate(patch_block=paste(patch,block,sep="_")) %>% 
  ungroup() %>% 
  select(-patch,-block) %>% 
  pivot_wider(
    names_from = sp_code,
    values_from = n_patch) 
