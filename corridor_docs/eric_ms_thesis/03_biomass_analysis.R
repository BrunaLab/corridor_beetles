# Script to import, clean, and organize EEC's Beetle data
# date: 25 september 2024

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(lme4)
library(sjPlot)
# load data ---------------------------------------------------------------


btl_data<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_clean","clean_btl_counts.csv"))
spp_codes<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","species_codes.csv"))

spp_bmass<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","spp_weights.csv")) %>% 
  group_by(sp_code) %>% 
  summarize(avg_bmass=mean(weight),
            sd=sd(weight),
            n_btls=n_distinct(weight)) %>% 
  arrange(desc(avg_bmass))



biomass_per_spp<-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>%
  group_by(species) %>%
  mutate(patch=case_when(
    patch == "c" ~ "Connected",
    patch == "m" ~ "Matrix",
    patch == "w" ~ "Winged",
    patch == "r" ~ "Rectangle",
    .default = as.character(patch))) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  rename(sp_code=species) %>% 
  left_join(spp_bmass,by="sp_code") %>% 
  select(-sd, -n_btls) %>% 
  mutate(spp_biomass=n*avg_bmass)

biomass_per_spp<-biomass_per_spp %>% 
  drop_na() %>% 
  ungroup() %>% 
  rename(species=sp_code)


ggplot(biomass_per_spp, 
       aes(
         x=reorder(species,-spp_biomass), 
         y=spp_biomass
         )
       ) + 
  geom_bar(stat = "identity")

biomass_top_6 <- biomass_per_spp %>% 
  filter(species=="cvig"|
           species=="alec"|
           species=="pign"|
           species=="dcar"|
           species=="open"|
           species=="aaeg")



biomass_2<-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>%
  group_by(species, patch,block) %>%
  # group_by(block) %>%
  # group_by(patch,block) %>%
  mutate(patch=case_when(
    patch == "c" ~ "Connected",
    patch == "m" ~ "Matrix",
    patch == "w" ~ "Winged",
    patch == "r" ~ "Rectangle",
    .default = as.character(patch))) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  rename(sp_code=species) %>% 
  left_join(spp_bmass,by="sp_code") %>%
  # select(-n.y) %>% 
  # rename(n=n.x) %>% 
  # select(-sd, -n_btls) %>% 
  mutate(spp_biomass=n*avg_bmass) %>% 
  ungroup() %>% 
  select(-sd) %>% 
  # select(-sp_code,-avg_bmass) %>% 
  drop_na() 


biomass_2 %>% 
  # group_by(patch,block) %>% 
  group_by(block) %>% 
  # group_by(patch) %>% 
  summarize(tot_bm=sum(spp_biomass))



biomass_2 %>% 
  group_by(patch) %>% 
  summarize(tot_bm=sum(spp_biomass))

biomass_2<-biomass_2 %>% 
  mutate(patch=as.factor(patch)) 
biomass_2$patch<-relevel(biomass_2$patch,"Matrix")

patch_type_mean_bm<-
biomass_2 %>% 
  group_by(patch,block) %>% 
  summarize(tot_bm=sum(spp_biomass)) %>% 
  group_by(patch) %>% 
  summarize(avg_bm=mean(tot_bm),
            sd_bm=sd(tot_bm))


ggplot(patch_type_mean_bm, 
       aes(
         x=reorder(patch,-avg_bm), 
         y=avg_bm
       )
) + 
  geom_bar(stat = "identity")

M5 <- lmer(spp_biomass ~ patch + (1 + patch | block), 
            data = biomass_2)

plot_model(M5, type = "pred")

summary(M5)


biomass_2_top_6 <- biomass_2 %>% 
  filter(sp_code=="cvig"|
           sp_code=="alec"|
           sp_code=="pign"|
           sp_code=="dcar"|
           sp_code=="open"|
           sp_code=="aaeg")

M6 <- lmer(spp_biomass ~ patch + (1 + patch | block), 
           data = biomass_2_top_6)

summary(M6)

plot_model(M6, type = "pred")

M7 <- lmer(spp_biomass ~ sp_code + patch + (1 | block), 
     data = biomass_2_top_6)

summary(M7)

plot_model(M7, type = "pred", terms = "sp_code")

plot_model(M7, type = "pred", terms = c("sp_code", "patch"))

M8 <- lmer(spp_biomass ~ n + sp_code + (1| block),
           data = biomass_2_top_6)

summary(M8)

plot_model(M8, type = "pred")

plot_model(M8, type = "pred", terms = c("n", "sp_code"))
 
plot_model(M8, type = "pred", terms = c("sp_code", "n"))

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
