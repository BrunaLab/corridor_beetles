library(tidyverse)



# load and prep data ------------------------------------------------------


btl_data<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_clean","clean_btl_counts.csv"))
spp_codes<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_raw","species_codes.csv"))


# calclulate Hill Nos. ----------------------------------------------------


# using hillR
# https://github.com/daijiang/hillR/blob/master/README.md
# https://daijiang.r-universe.dev/hillR
library(hillR)


hill_data<-btl_data %>%
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
  rename(sp_code=species,
         n_patch=n) %>%
  mutate(patch_block=paste(patch,block,sep="_")) %>% 
  ungroup() %>% 
  select(-patch,-block) %>% 
  pivot_wider(
    names_from = sp_code,
    values_from = n_patch) 



spp_abund<-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>%
  mutate(patch=case_when(
    patch == "c" ~ "Connected",
    patch == "m" ~ "Matrix",
    patch == "w" ~ "Winged",
    patch == "r" ~ "Rectangle",
    .default = as.character(patch))) %>%
  group_by(species,patch,block) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(patch,desc(n)) %>%
  rename(sp_code=species) %>%
  mutate(patch_block=paste(patch,block,sep="_")) %>% 
  ungroup() %>% 
  select(-patch,-block) %>% 
  separate(patch_block,c("patch_type","block"),sep="_",remove=TRUE) %>% 
  filter(sp_code=="cvig"|
           sp_code=="alec"|
           sp_code=="pign"|
           sp_code=="aaeg")



hill_data<-hill_data %>% column_to_rownames("patch_block")



h_rich<-hill_taxa(hill_data, q = 0) # taxonomic alpha diversity
h_rich<-as.data.frame(h_rich) |>
  rownames_to_column() %>% 
  separate(rowname,c("patch_type","block"),sep="_",remove=TRUE)

# taxonomic shannon diversity
h_shannon<-hill_taxa(hill_data, q = 1)

h_shannon<-as.data.frame(h_shannon) |>
  rownames_to_column() %>% 
  separate(rowname,c("patch_type","block"),sep="_",remove=TRUE)

# taxonomic simpson diversity
h_simpson<-hill_taxa(hill_data, q = 2)

h_simpson<-as.data.frame(h_simpson) |>
  rownames_to_column() %>% 
  separate(rowname,c("patch_type","block"),sep="_",remove=TRUE)


hill_results<-h_rich %>% 
  full_join(h_shannon,by=c("patch_type","block")) %>% 
  full_join(h_simpson,by=c("patch_type","block"))


# ANALYSES ----------------------------------------------------------------



# GLMM Richness -----------------------------------------------------------


# https://rpubs.com/annikanelson/GLMM_Workshop
library(lme4)
library(car)

# LINEAR MEXED MODEL
model <- lmer(h_rich ~ patch_type + (1|block), data = hill_results) # You should only use the 'aov()' for one-way ANOVAs with equal sample sizes
summary(model)
Anova(model)
shapiro.test(resid(model))
qqnorm(resid(model)) # plot of the normal theoretical quantiles against the exponential data quantiles
qqline(resid(model)) # the residuals should fall along this line if they are normally distributed

# GLMM
glmerLaplace <- glmer(formula = h_rich ~ patch_type + (1 | block),
                      data = hill_results,
                      family = poisson(link = "log"),
                      nAGQ = 1)

isSingular(glmerLaplace, tol = 1e-4)
summary(glmerLaplace)


# GLMM 2 ------------------------------------------------------------------


# following tutorial of 
# https://livinglandscapes.github.io/Course_EcologicalModeling/05-B-Generalized-Linear-Mixed-Models.html

require(librarian, quietly = TRUE)
shelf(tidyverse, 
      performance, # For checking model convergence
      MuMIn, # for model selection
      lme4, # For mixed modeling
      # pander,
      lattice,
      broom.mixed,
      DHARMa, # For mixed model diagnostics
      lib = tempdir(),
      quiet = TRUE)


hist(hill_results$h_rich,
     xlab = "h_rich",
     main = "")



ggplot(spp_abund, 
       aes(x = patch_type, 
           y = n,
           # y = h_rich,
           # y = h_shannon,
           color = block)) +
  scale_color_viridis_d(option = "turbo") + 
  # geom_point(position="jitter") + 
  geom_point() + 
  facet_wrap( ~ sp_code) + 
  theme_bw()


# Global model: Take 1
M0 <- glmer(n ~ patch_type * sp_code + (1 + patch_type * sp_code | block), 
            data = spp_abund, 
            family = poisson)
summary(M0)

# Model for richness 
M_rich <- glmer(h_rich ~ patch_type + (1 + patch_type | block),
                data = h_rich,
                family = poisson)
summary(M_rich)

# Model for shannons 
h_shannon

?glmer

M_shannon <- glmer(h_shannon ~ patch_type + (1 + patch_type | block),
                   data = h_shannon,
                   family = gaussian)
summary(M_shannon)


# residuals
simulationOutput <- 
  simulateResiduals(fittedModel = M0, plot = TRUE)



# figures -----------------------------------------------------------------


# RICHNESS

hill_results %>% 
ggplot(aes(x = block, 
           # y = n,
           y = h_rich,
           # y = h_shannon,
           color = block)) +
  scale_color_viridis_d(option = "turbo") + 
  # geom_point(position="jitter") + 
  geom_point() + 
  facet_wrap( ~ patch_type) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.9))
  


hill_results %>% 
  ggplot(aes(x = patch_type, 
             # y = n,
             # y = h_rich,
             y = h_shannon,
             color = block)) +
  scale_color_viridis_d(option = "turbo") + 
  geom_point(position="jitter") + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.9))

