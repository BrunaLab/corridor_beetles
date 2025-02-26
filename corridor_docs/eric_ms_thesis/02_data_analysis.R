# Script to import, clean, and organize EEC's Beetle data
# date: 25 september 2024

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate) # to deal with dates https://lubridate.tidyverse.org/
library(vegan)
library(permute)
library(lattice)
# load data ---------------------------------------------------------------

btl_data<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_clean","clean_btl_counts.csv"))


# quick summaries to look over the sampling point information ---------------
# how many points in each block, how many points per treatment, etc.

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



# summary data ------------------------------------------------------------


# total beetles captured --------------------------------------------------


total_N<-btl_data %>% summarise(n=sum(sum, na.rm=TRUE))


# total N by species ------------------------------------------------------


# total number of beetles sampled each round
total_N_sample<-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(date) %>% 
  summarise(n=sum(n, na.rm=TRUE)) %>% 
  arrange(date)
total_N_sample


# spp in order of no captured ---------------------------------------------


btl_order<-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))
btl_order


btl_order_block_53n <-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(block == "53n") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_block_8 <-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(block == "8") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_block_52 <-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(block == "52") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_block_54 <-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(block == "54") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_m <-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(patch == "m") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_c <-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(patch == "c") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_w <-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(patch == "w") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_r <-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(patch == "r") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

?filter
# abundance by patch ------------------------------------------------------




btl_sums_patch<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(patch) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(patch,desc(n)) %>% 
  mutate(patch=case_when(
    patch == "c" ~ "Corridor",
    patch == "m" ~ "Matrix",
    patch == "w" ~ "Winged",
    patch == "r" ~ "Rectangle",
    .default = as.character(patch))) 

ggplot(btl_sums_patch, aes(x=as.factor(patch),
                           y=n)) +
  geom_bar(stat = "identity") +
  theme_classic()




# avg abundance per patch type --------------------------------------------



btl_avg_patch<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(block,patch) %>%
  summarize(sum=sum(n, na.rm=TRUE)) %>%
  group_by(patch) %>%
  summarize(avg_n=mean(sum, na.rm=TRUE),
            sd_n=sd(sum, na.rm=TRUE)) %>%
  arrange(desc(avg_n)) %>% 
  mutate(patch=case_when(
    patch == "c" ~ "Corridor",
    patch == "m" ~ "Matrix",
    patch == "w" ~ "Winged",
    patch == "r" ~ "Rectangle",
    .default = as.character(patch))) 

ggplot(btl_avg_patch, aes(x=as.factor(patch),
                           y=avg_n)) +
  geom_bar(stat = "identity") +
  theme_classic()




btl_avg_patch2<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(block,patch) %>%
  summarize(sum=sum(n, na.rm=TRUE)) %>%
  mutate(patch=case_when(
    patch == "c" ~ "Corridor",
    patch == "m" ~ "Matrix",
    patch == "w" ~ "Winged",
    patch == "r" ~ "Rectangle",
    .default = as.character(patch)))



ggplot(btl_avg_patch2, aes(x=as.factor(patch),
                           y=sum)) +
  geom_boxplot(stat = "boxplot") +
  geom_jitter(color="blue", size=0.4, alpha=0.9) +
  theme_classic()


# summaries by patch type -------------------------------------------------


btl_sums_patch<-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(species,patch) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(patch,desc(n))


btl_data_sums<-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(species,patch) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(patch,desc(n))

ggplot(btl_data_sums, aes(x=patch, y=n)) + 
  geom_bar(stat = "identity")+
  facet_wrap(vars(species),scales="free",ncol = 5) 



ggplot(btl_data_sums, aes(x=species, y=n)) + 
  geom_bar(stat = "identity")+
  facet_wrap(vars(patch),scales="free",ncol = 4) 




mega_spp<-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(species) %>% 
  summarise(n=sum(n, na.rm=TRUE)) %>% 
  filter(n>500) %>% 
  ungroup() %>% 
  select(species,n)%>% 
  arrange(desc(n))


rare_spp<-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(species) %>% 
  summarise(n=sum(n, na.rm=TRUE)) %>% 
  filter(n<50) %>% 
  ungroup() %>% 
  select(species,n) %>% 
  arrange(desc(n))



plot_n<-btl_data %>%
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  filter(species=="pign"|species=="alec"|species=="cvig")
plot_btl_n <- function(data) {
  btl_n_plot<-ggplot(plot_n,
                     aes(x=date,
                         y=n,
                         group=species,
                         color=species)) +
    geom_line()+
    geom_point()+
    # facet_wrap(vars(habitat))
    facet_grid(vars(species),vars(patch))+
    theme_bw()
}




# code for making a rarefaction curve -------------------------------------

# this pools all the points in a patch over the entire sampling season
# tou can then do a rarefaction curve for each patch in each block
# can also edit the. code to makle it one curve per block (allpatches pooled), 
# per patch type (all blocks polled) etc.

rarefaction_data<-btl_data %>%
  select(-c(month,day,year,sum,point,sample_id)) %>% 
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  mutate(sampling_date = as.integer(factor(date))) %>% 
  relocate(sampling_date,.before=1) %>% 
  # group_by(block,patch,sampling_date,species) %>%
  group_by(block,patch,species) %>%
  summarize(n=sum(n)) %>% 
  pivot_wider(values_from=n,names_from=species) %>% 
  filter(patch=="c") %>% 
  # mutate(row=paste(block,sampling_date,sep="-"),.before=1) %>% 
  mutate(row=paste(patch,block,sep="-"),.before=1) %>% 
  column_to_rownames("row") %>% 
  # select(-block,-patch,-sampling_date) 
  select(-block,-patch) 


#   
#   pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
#   group_by(sample_id,species) %>% 
#   summarize(n=sum(n)) %>% 
#   pivot_wider(values_from=n,names_from=species) %>% 
#   column_to_rownames("sample_id") 
# 
# rarefaction_data<-btl_data %>% 
#   select(-c(month,day,year,date, block, point)) %>% 
#   pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
#   group_by(patch,species) %>% 
#   summarize(n=sum(n)) %>% 
#   pivot_wider(values_from=n,names_from=species) %>% 
#   column_to_rownames("patch") 



### worked example for generating rarefaction curve
#?rarecurve
#?rarefy

#data(BCI)
#S <- specnumber(BCI) # observed number of species
#(Raremax <- min(rowSums(BCI)))

#Srare <- rarefy(BCI, Raremax)
#plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
#abline(0, 1)

#rarecurve(BCI, step = 20, sample = Raremax, col = "blue", cex = 0.6)

# subsetting data so that there are no zeros 
rarefaction_data<-btl_data %>% 
  select(-c(month,day,year,date)) %>% 
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(sample_id,species) %>% 
  summarize(n=sum(n)) %>% 
  pivot_wider(values_from=n,names_from=species) %>% 
  column_to_rownames("sample_id") %>% 
  filter((rowSums(rarefaction_data) > 10)) 

s <- specnumber(rarefaction_data)
s
(raremax <- min(rowSums(rarefaction_data)))

btl.rare <- rarefy(rarefaction_data, raremax)
btl.rare
plot(s,btl.rare,xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")


rarecurve(rarefaction_data, step = 20, sample = raremax, col = "blue", cex = 0.6)

rarecurve(rarefaction_data, 
          step = 20, 
          sample = raremax, 
          col = "blue", 
          cex = 0.6,
          label=FALSE)






# sandbox -----------------------------------------------------------------

#example hill function
#hill_num = renyi(data, hill= TRUE)

#btl_order<-btl_data %>%
  #pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  #group_by(species) %>%
  #summarize(n=sum(n, na.rm=TRUE)) %>%
  #arrange(desc(n)) %>%
  #mutate(species = reorder(species, desc(n)))
#btl_order

#btl_data_sums<-btl_data %>%
 # pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  #group_by(species,patch) %>%
  #summarize(n=sum(n, na.rm=TRUE)) %>%
  #arrange(patch,desc(n))


#btl_data_sums<-btl_data %>%
 # pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  #group_by(species,patch) %>%
  #summarize(n=sum(n, na.rm=TRUE)) %>%
  #arrange(patch,desc(n))

# species list by patch type
btl_spec_patch<- btl_data |>
  pivot_longer(pvin:ostr, names_to = "species", values_to = "n") |>
  group_by(species,patch) |>
  summarise(n=sum(n, na.rm=TRUE)) |>
  arrange(patch,desc(n)) |>
  pivot_wider(values_from=n,names_from=species) |>
  mutate(patch=case_when(
    patch == "c" ~ "Corridor",
    patch == "m" ~ "Matrix",
    patch == "w" ~ "Winged",
    patch == "r" ~ "Rectangle",
    .default = as.character(patch))) |>
  column_to_rownames("patch")
# hill number comparisson 
hill_btl <- renyi(btl_spec_patch, hill = TRUE)

hill_plot<-plot(hill_btl)
hill_plot
# ggsave(hill_plot, 
#        "hill_plot.png")
# bray curtis
#example
#taxa_bray = vegan::vegdist(data, method = "bray", binary = FALSE)
#taxa_bray

taxa_bray <- vegan::vegdist(btl_spec_patch, method = "bray", binary = FALSE)
taxa_bray

btl_spec_patch<- btl_data |>
  pivot_longer(pvin:ostr, names_to = "species", values_to = "n") |>
  group_by(species,patch) |>
  summarise(n=sum(n, na.rm=TRUE)) |>
  arrange(patch,desc(n)) |>
  pivot_wider(values_from=n,names_from=species) |> 
  column_to_rownames("patch")


rarefaction_data<-btl_data %>% 
  select(-c(month,day,year,date)) %>% 
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  group_by(sample_id,species) %>% 
  summarize(n=sum(n)) %>% 
  pivot_wider(values_from=n,names_from=species) %>% 
  column_to_rownames("sample_id") %>% 
  filter((rowSums(rarefaction_data) > 10)) 

s <- specnumber(btl_spec_patch)
s
(raremax <- min(rowSums(btl_spec_patch)))

btl.rare <- rarefy(btl_spec_patch, raremax)
btl.rare
plot(s,btl.rare,xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")


rarecurve(btl_spec_patch, step = 20, sample = raremax, col = "blue", cex = 0.6)

rarecurve(rarefaction_data, 
          step = 20, 
          sample = raremax, 
          col = "blue", 
          cex = 0.6,
          label=FALSE)



N_by_spp<-btl_data %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  select(-c(sample_id,point,month,day,year,sum)) %>% 
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  arrange(desc(n))
  



# total N by date type --------------------------------------------------


N_by_patch<-btl_data %>% 
  group_by(date)


  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  select(-c(sample_id,point,month,day,year,sum)) %>% 
  pivot_longer(pvin:ostr,names_to = "species",values_to = "n") %>% 
  arrange(desc(n)) 















# species diversity -------------------------------------------------------



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


