library(tidyverse)
library(here)
library(kableExtra)
library(sjPlot)
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
  separate(patch_block,c("patch_type","block"),sep="_",remove=TRUE) 

spp_abund_top6<-btl_data %>%
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
           sp_code=="dcar"|
           sp_code=="open"|
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
model <- lmer(h_rich ~ patch_type + (1|block), data = hill_results)# You should only use the 'aov()' for one-way ANOVAs with equal sample sizes
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


# foo<-spp_abund %>% group_by(patch_type,block) %>% summarize(n=sum(n))
ggplot(spp_abund_top6, 
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



# Global model: Take 1 abundance, indep of species ID
M0 <- glmer(n ~ patch_type + (1 + patch_type | block), 
            data = spp_abund, 
            family = poisson)

summary(M0)
Anova(M0)
aovM0<-Anova(M0)

plot_model(M0, type = "pred")



# EMILIO ADDITION - EXPORTING TABLE RESULTS TO UPLOAD TO THESIS DOC

# REQUIRES CLEANUP, BUT HERE IS HOW TO GET THE STATS TABLES IN KABLEEXTRA FOR YOUR THESIS
tidyM0<-M0 %>% broom::tidy()
aovM0<-aovM0 %>% broom::tidy()
write_csv(tidyM0,"./corridor_docs/eric_ms_thesis/tables/m0.csv")
write_csv(aovM0,"./corridor_docs/eric_ms_thesis/tables/aovM0.csv")
# library(rempsyc)
# (stats.table <- tidy(M0, conf.int = TRUE))
# nice_table(stats.table, broom = "glmm")
# nice_table(stats.table, highlight = TRUE)



# Model 2
M2 <- glmer(n ~ sp_code + (1 | block), 
            data = spp_abund, 
            family = poisson)
summary(M2)

Anova(M2)

plot_model(M2, type = "pred")


M2 %>% broom::tidy()


# Model 3
# sjplot 
library(sjPlot)

M3 <- glmer(n ~ sp_code * patch_type + (1 | block), 
            data = spp_abund_top6, 
            family = poisson)
summary(M3)

Anova(M3)

plot_model(M3, type = "pred", terms = c("sp_code", "patch_type"))

plot_model(M3, type = "pred", terms = c("patch_type", "sp_code"))


?interaction.plot

interaction.plot(M3)

?plot_model

# Eric trying table

tidyM3<-M3 %>% broom::tidy()
aovM3<-aovM0 %>% broom::tidy()
write_csv(tidyM0,"./corridor_docs/eric_ms_thesis/tables/m0.csv")
write_csv(aovM0,"./corridor_docs/eric_ms_thesis/tables/aovM0.csv")



# Global model: Take 1
M1 <- glmer(n ~ patch_type * sp_code + (1 + patch_type * sp_code | block), 
            data = spp_abund, 
            family = poisson)
summary(M1)

Anova(M1)

# Model for richness 
M_rich <- glmer(h_rich ~ patch_type + (1 + patch_type | block),
                data = h_rich,
                family = poisson)
summary(M_rich)

Anova(M_rich)

plot_model(M_rich, type = "pred")

library(broom.mixed)
tidy_model <- tidy(M_rich, effects = "fixed")

library(gt)
table <- tidy_model |>
  gt() 

library(flextable)
ft <- flextable(tidy_model)
save_as_image(ft, path = "glmer_table.png")

# Model for shannons 
h_shannon

M_shannon

M_shannon <- lmer(h_shannon ~ patch_type + (1 | block),
                  data = h_shannon)

summary(M_shannon)

plot_model(M_shannon, type = "pred")


?glmer

h_simpson

M_simpson <- lmer(h_simpson ~ patch_type + (1 | block),
                   data = h_simpson)
summary(M_simpson)

plot_model(M_simpson, type = "pred")

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
  


# Emilio's suggestions for making snazzy figure. --------------------------

# also includes code block to insert in Rmd

sp_richness_patches<-  
hill_results %>% 
  mutate(patch_type = factor(patch_type,
                             levels = c("Matrix", "Rectangle", "Winged", "Connected"))) %>%   # this changes the order of things on the x axis. suggest this order (Matrixm, then least to most)
  ggplot(aes(x = patch_type, 
             # y = n,
             y = h_rich,
             # y = h_shannon,
             color = block,
             shape = block)) +
  scale_shape_manual(values=c(15:18))+ # change shapes of points
  scale_color_viridis_d(option = "turbo") + 
  geom_point(position=position_jitter(width=0.16, height=0.15))+ # manipulate spread of jitter
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.95,size = 10))+
  labs(x = "Patch Type", 
       y = "Species Richness",
       color="Block",   # edit legend title. be sure to do both shape & color
       shape="Block")+ 
  # theme(
  #   legend.box.background = element_rect(color="black", linewidth = 0.1),
  #   # legend.box.margin = margin(116, 6, 6, 6)
  # )+
  theme(
    legend.box.background = element_rect(color="gray", size=1),
    legend.box.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.position = "top",
    legend.text = element_text(size = 10, colour = "black"),
    legend.title = element_text(size = 10, colour = "black")
  )+
  theme(axis.title.y = element_text(size = 12,face="bold"))+
  theme(axis.title.x =element_text(size = 12,face="bold"))+
  theme(axis.text.y = element_text(size = 10))+
  scale_y_continuous(limits = c(1, 18), breaks = seq(1, 18, by = 2)) # y axis limit and breaks
  

# This saves a png version of file to the images folder
ggsave("corridor_docs/eric_ms_thesis/images/sp_richness_patches.png", width = 4, height = 4, units = "in")
  
# # Include the figure in your markdown document with this code block. 
# # I added it to the rmd so you could see how.
# 
# ```{r  label = sp_richness_patches, echo = FALSE, fig.cap = "Dung beetle secies richness in three different.....and the forest matrix surrounding patches.", out.width = '75%'}
# knitr::include_graphics("corridor_docs/eric_ms_thesis/images/sp_richness_patches.png")
# ```



# end of EB edited figure -------------------------------------------------



# figure attempts 

sp_shannon_patches<-  
  hill_results %>% 
  mutate(patch_type = factor(patch_type,
                             levels = c("Matrix", "Rectangle", "Winged", "Connected"))) %>%   # this changes the order of things on the x axis. suggest this order (Matrixm, then least to most)
  ggplot(aes(x = patch_type, 
             # y = n,
             # y = h_rich,
             y = h_shannon,
             color = block,
             shape = block)) +
  scale_shape_manual(values=c(15:18))+ # change shapes of points
  scale_color_viridis_d(option = "turbo") + 
  geom_point(position=position_jitter(width=0.16, height=0.15))+ # manipulate spread of jitter
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.95,size = 10))+
  labs(x = "Patch Type", 
       y = "Shannon Diversity Index",
       color="Block",   # edit legend title. be sure to do both shape & color
       shape="Block")+ 
  # theme(
  #   legend.box.background = element_rect(color="black", linewidth = 0.1),
  #   # legend.box.margin = margin(116, 6, 6, 6)
  # )+
  theme(
    legend.box.background = element_rect(color="gray", size=1),
    legend.box.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.position = "top",
    legend.text = element_text(size = 10, colour = "black"),
    legend.title = element_text(size = 10, colour = "black")
  )+
  theme(axis.title.y = element_text(size = 12,face="bold"))+
  theme(axis.title.x =element_text(size = 12,face="bold"))+
  theme(axis.text.y = element_text(size = 10))+
  scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 2)) # y axis limit and breaks


# This saves a png version of file to the images folder
ggsave("corridor_docs/eric_ms_thesis/images/sp_shannon_patches.png", width = 4, height = 4, units = "in")

sp_simpson_patches<-  
  hill_results %>% 
  mutate(patch_type = factor(patch_type,
                             levels = c("Matrix", "Rectangle", "Winged", "Connected"))) %>%   # this changes the order of things on the x axis. suggest this order (Matrixm, then least to most)
  ggplot(aes(x = patch_type, 
             # y = n,
             # y = h_rich,
             #y = h_shannon,
             y = h_simpson,
             color = block,
             shape = block)) +
  scale_shape_manual(values=c(15:18))+ # change shapes of points
  scale_color_viridis_d(option = "turbo") + 
  geom_point(position=position_jitter(width=0.16, height=0.15))+ # manipulate spread of jitter
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.95,size = 10))+
  labs(x = "Patch Type", 
       y = "Simpson Diversity Index",
       color="Block",   # edit legend title. be sure to do both shape & color
       shape="Block")+ 
  # theme(
  #   legend.box.background = element_rect(color="black", linewidth = 0.1),
  #   # legend.box.margin = margin(116, 6, 6, 6)
  # )+
  theme(
    legend.box.background = element_rect(color="gray", size=1),
    legend.box.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.position = "top",
    legend.text = element_text(size = 10, colour = "black"),
    legend.title = element_text(size = 10, colour = "black")
  )+
  theme(axis.title.y = element_text(size = 12,face="bold"))+
  theme(axis.title.x =element_text(size = 12,face="bold"))+
  theme(axis.text.y = element_text(size = 10))+
  scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 2)) # y axis limit and breaks


# This saves a png version of file to the images folder
ggsave("corridor_docs/eric_ms_thesis/images/sp_simpson_patches.png", width = 4, height = 4, units = "in")

# tables for model results 
spp_table<-knitr::kable(spp_table_data, 
                        digits = 2,
                        align="lcrcccc",
                        format="latex",
                        row.names = FALSE,
                        booktabs=T,
                        linesep = "", #removes the blank line after every 5 lines
                        caption = "Dung beetle species sampled in the SRS site and their total abundance over the course of the study.") %>%
  kable_styling(bootstrap_options = c("striped"),
                full_width = F,
                latex_options="scale_down",
                font_size = 12,
                position = "center") %>% 
  column_spec(1, italic = T) %>% 
  row_spec(0, bold = T) %>% 
  row_spec(1:15, hline_after = T) 
spp_table

library(broom.mixed)
tidy_model <- tidy(M0, effects = "fixed")
library(knitr)
M0_table <- kable(tidy_model, digits = 3)
M0_table




# emilio's suggestion for stats table -------------------------------------

# broom gets you almost all the way to your final table

M0_table %>% 
kable(digits = 2,
      format = "latex",
      caption = "this is where your caption goes",
      align = "llcccc",
      escape = FALSE,
      row.names = FALSE,
      booktabs = T,
      linesep = ""
) %>%
  kable_styling(
    bootstrap_options = c("hover"),
    # full_width = F,
    latex_options = c("scale_down","hold_position"),
    font_size = 12,
    position = "center"
  ) 



# end of emilio's stats table ---------------------------------------------


