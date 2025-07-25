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

# need to add how many beetles were included in each raw weight (some too small)
btls_per_tray<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","spp_weights.csv")) %>%
  group_by(sp_code) %>% 
  summarize(n_tray=n()) %>% 
  mutate(btls_per_tray=case_when(
    n_tray==15~1,
    n_tray==5~3,
    n_tray==3~5,
    n_tray==1~15,
    .default = as.integer(n_tray)
  ))


spp_bmass<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","spp_weights.csv")) %>% 
  left_join(btls_per_tray,by="sp_code")  %>% 
  mutate(indiv_bmass=weight/btls_per_tray) %>% 
  group_by(sp_code) %>% 
  summarize(avg_ind_bmass=mean(indiv_bmass),
            sd = sd(indiv_bmass))

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
  # select(-sd, -n_btls) %>% 
  mutate(spp_biomass=n*avg_ind_bmass)

biomass_per_spp<-biomass_per_spp %>% 
  drop_na(avg_ind_bmass) %>% 
  ungroup() %>% 
  rename(species=sp_code)



write_csv(biomass_per_spp,here("corridor_docs","eric_ms_thesis","ms_data","data_raw","biomass_per_spp.csv"))

biomass_per_spp<-
biomass_per_spp %>% 
rename(sp_code=species) %>% 
  left_join(spp_codes) %>% 
  mutate(species=tolower(species)) %>% 
  mutate(species=paste(genus, species, sep=" ")) 



# figure of indiv biomass vs abund ----------------------------------------

# are the largest species the least common?



# Basic scatter plot.
p1 <- ggplot(biomass_per_spp, aes(x=avg_ind_bmass, y=n)) + 
  geom_point( color="navyblue") +
  geom_smooth(method=lm , color="black", se=TRUE) +
  theme_classic()
labs(x="Species",y="Total biomass collected")+

sp_biomass_plot<-
ggplot(biomass_per_spp, 
       aes(
         x=reorder(species,-spp_biomass), 
         y=spp_biomass
         )
       ) + 
  labs(x="Species",y="Total biomass collected")+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.text.x = element_text(face = 'italic'))

ggsave("corridor_docs/eric_ms_thesis/images/sp_biomass_plot.png", width = 4, height = 4, units = "in")



sp_abundance_plot<-
ggplot(biomass_per_spp, 
       aes(
         x=reorder(species,-spp_biomass), 
         y=n
       )
) + 
  labs(x="Species",y="Total No. collected")+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.text.x = element_text(face = 'italic'))

ggsave("corridor_docs/eric_ms_thesis/images/sp_abundance_plot.png", width = 4, height = 4, units = "in")




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
  mutate(spp_biomass=n*avg_ind_bmass) %>% 
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


avg_bmass_patch_plot<-
ggplot(patch_type_mean_bm, 
       aes(
         x=reorder(patch,-avg_bm), 
         y=avg_bm
       )
) + 
  geom_errorbar(aes(x=patch, ymin=avg_bm-sd_bm, ymax=avg_bm+sd_bm), 
                width=0.2, colour="black", alpha=0.9, size=0.5) +
  labs(x="Patch Type",y= "Biomass (Mean ± SD)")+
  geom_bar(stat = "identity")+
  theme_classic() 
  


ggsave("corridor_docs/eric_ms_thesis/images/avg_bmass_patch_plot.png", width = 4, height = 4, units = "in")



block_type_mean_bm<-
  biomass_2 %>% 
  group_by(patch,block) %>% 
  summarize(tot_bm=sum(spp_biomass)) %>% 
  group_by(block) %>% 
  summarize(avg_bm=mean(tot_bm),
            sd_bm=sd(tot_bm))



avg_bmass_block_plot<-
  ggplot(block_type_mean_bm, 
         aes(
           x=reorder(block,-avg_bm), 
           y=avg_bm
         )
  ) + 
  geom_errorbar(aes(x=block, ymin=avg_bm-sd_bm, ymax=avg_bm+sd_bm), 
                width=0.2, colour="black", alpha=0.9, size=0.5) +
  labs(x="Block",y= "Biomass (Mean ± SD)")+
  geom_bar(stat = "identity")+
  theme_classic() 


ggsave("corridor_docs/eric_ms_thesis/images/avg_bmass_block_plot.png", width = 4, height = 4, units = "in")


# http://users.stat.umn.edu/~gary/book/RExamples/randommixed-effects.html#random-effects-models
# TOTAL BIOMASS
total_bmass<-
  
  
  
  
biomass_2 %>% 
  group_by(patch,block) %>% 
  summarize(biomass=sum(spp_biomass))

M5_1 <- lmer(biomass ~  patch  + (1 | block), 
           data =total_bmass )
plot_model(M5_1, type = "pred")
summary(M5_1)

M5_1_table<- broom::tidy(M5_1)
write_csv(M5_1_table,"./corridor_docs/eric_ms_thesis/tables/lmerM5_1.csv")



M5_2 <- lmer(spp_biomass ~ sp_code +  patch  + (1 | block), 
            data = biomass_2)
plot_model(M5_2, type = "pred")
summary(M5_2)


M5_2_table<- broom::tidy(M5_2)
write_csv(M5_2_table,"./corridor_docs/eric_ms_thesis/tables/lmerM5_2.csv")

plot_model(M5_2, type = "pred", terms = c("sp_code", "patch"))
plot_model(M5_2, type = "pred", terms = c("patch","sp_code"))



biomass_2_top_6 <- biomass_2 %>% 
  filter(sp_code=="cvig"|
           sp_code=="alec"|
           sp_code=="pign"|
           sp_code=="dcar"|
           sp_code=="open"|
           sp_code=="aaeg")

M6 <- lmer(avg_ind_bmass ~ patch + (1 | block), 
           data = biomass_2_top_6)

M6 <- aov(avg_ind_bmass ~ patch + block, 
           data = biomass_2_top_6)

summary(M6)

plot_model(M6, type = "pred")

M7 <- lmer(spp_biomass ~ sp_code + patch + (1 | block), 
     data = biomass_2_top_6)

summary(M7)

plot_model(M7, type = "pred", terms = "sp_code")

plot_model(M7, type = "pred", terms = c("sp_code", "patch"))

M8 <- lmer(avg_ind_bmass ~ n + (1| block),
           data = biomass_2_top_6)

summary(M8)

plot_model(M8, type = "pred")

plot_model(M8, type = "pred", terms = c("n", "sp_code"))
 
plot_model(M8, type = "pred", terms = c("sp_code", "n"))

b2<-biomass %>%
  ungroup() %>% 
  select(-c(sp_code,n,avg_ind_bmass)) %>% 
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

  

# ind biomass v abundance -------------------------------------------------

  # https://www.sciencedirect.com/science/article/pii/S0169534707000985?via%3Dihub
  
  body_mass_v_abund<-btl_data %>%
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
    mutate(spp_biomass=n*avg_ind_bmass) %>% 
    ungroup() %>% 
    select(-sd) %>% 
    # select(-sp_code,-avg_bmass) %>% 
    drop_na() 
  
  indiv_bmass<-body_mass_v_abund %>% 
    select(sp_code,avg_ind_bmass) %>% 
    distinct() %>% 
    arrange(desc(avg_ind_bmass)) %>% 
    mutate(sp_code = fct_inorder(factor(sp_code))) %>% 
    group_by(sp_code) %>% 
    mutate(shape=cur_group_id()) %>% 
    mutate(shape=as.factor(shape))


# overall species mass v abundance ----------------------------------------

    library(RColorBrewer)
  library(geomtextpath)
  
  
  
  total_n<- body_mass_v_abund %>% group_by(sp_code) %>% 
    summarize(total_n=sum(n))
  total_n<-full_join(indiv_bmass,total_n)
  total_n %>% 
    ggplot(aes(x=avg_ind_bmass, y=total_n, color=sp_code)) + 
    # scale_shape_manual(values = total_n$shape)+
    # ggplot(aes(x=log(avg_bmass), y=log(total_n), color=sp_code,shape=sp_code)) + 
    geom_point()+
    geom_smooth(method=lm , color="darkgray", se=FALSE) +
    theme_classic()
  
  

# block species mass v abundance ------------------------------------------

  
  block_n<- body_mass_v_abund %>% group_by(sp_code,block) %>% 
    summarize(total_n=sum(n))
  block_n<-full_join(indiv_bmass,block_n)
  
  
  
  
  block_n %>% 
    ggplot(aes(x=avg_ind_bmass, y=total_n, color=block,shape=block)) + 
    geom_point()+
    # scale_fill_brewer(palette = "Spectral")+
    # scale_shape_manual(values = total_n$shape)+
    geom_labelsmooth(aes(label = block), fill = "white",
                     method = "lm", formula = y ~ x,
                     size = 6, linewidth = 1, boxlinewidth = 0.6) +
    # geom_smooth(method=lm , color="darkgray", se=FALSE) +
  
    theme_classic()
  
  

# patch species mass v abundance ------------------------------------------

  
  patch_n<- body_mass_v_abund %>% group_by(sp_code,patch) %>% 
    summarize(total_n=sum(n))
  patch_n<-full_join(indiv_bmass,patch_n)
  
  
  patch_n %>% 
    ggplot(aes(x=avg_ind_bmass, y=total_n, shape=patch,color=patch)) + 
    geom_point()+
    scale_fill_brewer(palette = "Spectral")+
    geom_labelsmooth(aes(label = patch), fill = "white",
                     method = "lm", formula = y ~ x,
                     size = 6, linewidth = 1, boxlinewidth = 0.6) +
    
    # scale_fill_brewer(palette = "Spectral") +
    theme_classic()

# 
  
  
  