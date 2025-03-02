# Script to import, clean, and organize EEC's Beetle data
# date: 25 september 2024

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate) # to deal with dates https://lubridate.tidyverse.org/
library(vegan)
library(permute)
library(lattice)
library(ggmosaic)
library(iNEXT)
# load data ---------------------------------------------------------------

btl_data<-read_csv(here("corridor_docs","eric_ms_thesis","ms_data","data_clean","clean_btl_counts.csv"))



# TO DO -------------------------------------------------------------------

# total beetles captured --------------------------------------------------
total_N<-btl_data %>% 
  summarise(n=sum(sum, na.rm=TRUE))
total_N
# How many species total?
total_spp<-btl_data %>% 
  select(-sum) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n))) %>% 
  summarise(n_spp=n_distinct(species))
total_spp

# Number captured per species (all data pooled) ---------------------------
btl_order<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))
btl_order


# Number captured per block -----------------------------------------------
N_block<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(block) %>%
  summarize(n=sum(n, na.rm=TRUE))
N_block

# number species per block ------------------------------------------------

N_spp_block<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(n>0) %>% 
  group_by(block) %>%
  summarize(n_spp=n_distinct(species, na.rm=TRUE))
N_spp_block


# count per species per block ---------------------------------------------

btl_order_block<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(block,species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(block,desc(n)) %>%
  pivot_wider(names_from = block, 
              values_from = n,
              names_prefix = "block_")
btl_order_block


# number species per patch type --------------------------------------------

N_spp_patch<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(n>0) %>% 
  group_by(patch) %>%
  summarize(n_spp=n_distinct(species, na.rm=TRUE)) %>% 
  arrange(desc(n_spp))
N_spp_patch


# count per species per patch type -----------------------------------------

btl_order_patch<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(patch,species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  pivot_wider(names_from = patch, 
              values_from = n) %>% 
  arrange(desc(c)) %>%
  mutate(rank_c=row_number()) %>% 
  relocate(rank_c,.after="c") %>% 
  mutate(rank_c=if_else(c==0,NA,rank_c)) %>% 
  arrange(desc(r)) %>%
  mutate(rank_r=row_number()) %>% 
  mutate(rank_r=if_else(r==0,NA,rank_r)) %>% 
  relocate(rank_r,.after="r") %>% 
  arrange(desc(w)) %>%
  mutate(rank_w=row_number()) %>% 
  mutate(rank_w=if_else(w==0,NA,rank_w)) %>% 
  relocate(rank_w,.after="w") %>% 
  arrange(desc(m)) %>%
  relocate(m,.after="species") %>% 
  mutate(rank_m=row_number()) %>% 
  relocate(rank_m,.after="m") %>% 
  mutate(rank_m=if_else(m==0,NA,rank_m)) 
btl_order_patch



# total number of sample points -------------------------------------------

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



# total number of beetles sampled each round
total_N_sample<-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(date) %>% 
  summarise(n=sum(n, na.rm=TRUE)) %>% 
  arrange(date)
total_N_sample



btl_order_block_53n <-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(block == "53n") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_block_8 <-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(block == "8") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_block_52 <-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(block == "52") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_block_54 <-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(block == "54") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_m <-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(patch == "m") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_c <-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(patch == "c") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_w <-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(patch == "w") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

btl_order_r <-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  filter(patch == "r") %>%
  group_by(species) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  mutate(species = reorder(species, desc(n)))

?filter
# abundance by patch ------------------------------------------------------




btl_sums_patch<-btl_data %>%
  select(-sum) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(species,patch) %>%
  summarize(n=sum(n, na.rm=TRUE)) %>%
  arrange(patch,desc(n))


btl_data_sums<-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(species) %>% 
  summarise(n=sum(n, na.rm=TRUE)) %>% 
  filter(n>500) %>% 
  ungroup() %>% 
  select(species,n)%>% 
  arrange(desc(n))


rare_spp<-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  group_by(species) %>% 
  summarise(n=sum(n, na.rm=TRUE)) %>% 
  filter(n<50) %>% 
  ungroup() %>% 
  select(species,n) %>% 
  arrange(desc(n))



plot_n<-btl_data %>%
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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
#   pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
#   group_by(sample_id,species) %>% 
#   summarize(n=sum(n)) %>% 
#   pivot_wider(values_from=n,names_from=species) %>% 
#   column_to_rownames("sample_id") 
# 
# rarefaction_data<-btl_data %>% 
#   select(-c(month,day,year,date, block, point)) %>% 
#   pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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
#rarefaction_data<-btl_data %>% 
  #select(-c(month,day,year,date)) %>% 
  #pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  #group_by(sample_id,species) %>% 
  #summarize(n=sum(n)) %>% 
  #pivot_wider(values_from=n,names_from=species) %>% 
  #column_to_rownames("sample_id") %>% 
  #filter((rowSums(rarefaction_data) > 10)) 

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
  #pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  #group_by(species) %>%
  #summarize(n=sum(n, na.rm=TRUE)) %>%
  #arrange(desc(n)) %>%
  #mutate(species = reorder(species, desc(n)))
#btl_order

#btl_data_sums<-btl_data %>%
 # pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  #group_by(species,patch) %>%
  #summarize(n=sum(n, na.rm=TRUE)) %>%
  #arrange(patch,desc(n))


#btl_data_sums<-btl_data %>%
 # pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  #group_by(species,patch) %>%
  #summarize(n=sum(n, na.rm=TRUE)) %>%
  #arrange(patch,desc(n))

# species list by patch type
btl_spec_patch<- btl_data |>
  pivot_longer(pvin:osyl, names_to = "species", values_to = "n") |>
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
  pivot_longer(pvin:osyl, names_to = "species", values_to = "n") |>
  group_by(species,patch) |>
  summarise(n=sum(n, na.rm=TRUE)) |>
  arrange(patch,desc(n)) |>
  pivot_wider(values_from=n,names_from=species) |> 
  column_to_rownames("patch")


rarefaction_data<-btl_data %>% 
  select(-c(month,day,year,date)) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
  arrange(desc(n))
  


#df = structure(list(effect = structure(c(2L, 2L, 1L, 1L), .Label = c("yes", 
#                                                                     "no"), class = "factor"), sex = structure(c(1L, 2L, 1L, 2L), 
#                                                                                                               .Label = c("f",  "m"), class = "factor"), n = c(8, 3, 8, 12)), 
#               row.names = c(NA, -4L), class = "data.frame")

#ggplot(df, aes(effect, y=n, fill = sex)) +
#  geom_col(position="fill")

head(btl_data_sums)

mosaic_data <- filter(btl_data_sums, .by )

ggplot(btl_data_sums, aes(species, y = n, fill = patch)) +
  geom_col(position = "fill")

# functions for ES TES and plot.TES

ES <-  function (x,m=1,method=c("a","b"))
{ 
  method <- match.arg(method, c("a", "b"))
  
  if (m<1){warning("m must be a positive value");break}
  if (m%%1!=0)warning("results may be meaningless because m is not an integer")
  if (any(x < 0, na.rm = TRUE)) 
  {warning("data have negative entries");break}
  if (any(is.na(x))) 
  {x [is.na(x)] <- 0; warning("empty data were replaced by '0' values")} 
  if(!identical(all.equal(as.integer(x),  as.vector(x)), TRUE)) 
    warning("results may be meaningless with non-integer data in method")
  x <- as.vector(x)
  x <- x[x>0]
  Ni <- sum(x)
  if (m>Ni){warning("m can not be larger than the total sample size");break}
  
  if(method  == "a")
  {
    ESSii <- sum(1 - exp(lchoose(Ni - x, m)- lchoose(Ni, m)))
  }
  
  if(method  == "b")
  {
    ESSii <- sum(1-(1-x/sum(x))^m)
  }
  return(ESSii)
}


TES <- function(x,knots=40){
  TESab <- function (x,knots=40,method=c("a","b"))
  {
    method <- match.arg(method, c("a", "b"))
    x <- as.vector(x) 
    if (any(x < 0, na.rm = TRUE)) 
    {warning("data have negative entries");break}
    if (any(is.na(x))) 
    {x [is.na(x)] <- 0; warning("empty data were replaced by '0' values")} 
    if(!identical(all.equal(as.integer(x),  as.vector(x)), TRUE)) 
      warning("results may be meaningless with non-integer data in method")
    
    nm <- seq(from=1,to=log(sum(x)),length=knots) 
    fm <- unique(floor(exp(nm)))
    
    result <- data.frame(value = sapply(fm, function(fm) ES(x,m=fm,method = method)),
                         Logm=log(fm))
    
    a <- NA#Set a=NA if there is insufficient data to do the modelling
    Error_four <- FALSE #Set Error_four as FALSE
    xmax <- NA
    
    parameter='Weibull'
    tryCatch(
      {md <- nls(value ~ SSweibull(Logm, Asym, Drop, lrc, pwr),data=result) #Use selfStart model evaluates the Weibull model for growth curve data and its gradient. Logm is the "x" value at which to evaluate the model, while Asym, Drop, lrc, and pwr are model parameters. Model expression: Asym-Drop*exp(-exp(lrc)*x^pwr)      
      Coe <- summary(md)$coefficients
      a <- Coe[1,1]
      s.d <- sqrt(Coe[1,2]^2*(nrow(result)-4)) 
      b <- Coe[2,1]
      c <- exp(Coe[3,1])
      d <- Coe[4,1]
      xmax <-  (-(log(0.1*a/b))/c)^(1/d)},#The 1/2 max value of x axis in plotting, at the value of y=0.9*a
      error  = function(e){Error_four  <<- TRUE}
    ) #Assign TRUE to Error_four
    
    if(Error_four) #If an error occur for four parameter model, then run three parameter model
    {
      parameter='logistic'
      tryCatch({md <- nls(value~SSlogis(Logm, Asym, xmid, scal),data= result) #Use selfStart model evaluates the logistic function and its gradient. Logm is the "x" value at which to evaluate the model, while Asym, xmid, and scal are model parameters. Model expression: Asym/(1+exp((xmid-x)/scal))
      Coe <- summary(md)$coefficients
      a <- Coe[1,1]
      s.d <- sqrt(Coe[1,2]^2*(nrow(result)-3)) 
      xmax <-  1.8*Coe[2,1]},
      error  = function(e){parameter  <<- NA })
    }
    if (is.na(a))
    {
      s.d <- NA
      warning("Insufficient data to provide reliable estimators and associated s.e.")
    }
    if (!is.na(xmax)){
      Predx <- seq(0, 2*xmax, length = 1000)
      Predy <- predict(md, list(Logm = Predx))
      attr(Predy, 'gradient') <- NULL
      z <- list(par = c(a=round(a, 2), a.sd = round(s.d, 2),Model.par = parameter),
                result = result,
                xmax = xmax,
                Predx = Predx,
                Predy = Predy)
    } else {
      z <- list(par = c(a=a, a.sd = s.d,Model.par = parameter),
                result = result)
    }
    return(z)
  }  
  TESa <- TESab(x,knots=knots,method="a")
  TESb <- TESab(x,knots=knots,method="b")
  tbl <- as.data.frame(rbind(TESa = TESa$par, TESb = TESb$par))
  tbl[, 1:2] <- apply(tbl[, 1:2], 1:2, as.numeric)
  tbl[3, 1] <- round(mean(tbl[, 1]), 2)
  tbl[3, 2] <- round((sqrt(tbl[1, 2] ^ 2 + tbl[2, 2] ^ 2))/2, 2)
  rownames(tbl)[3] <- 'TESab'
  return(list(summary = tbl,
              TESa = TESa,
              TESb = TESb))
}

plot.TES <- function(TES_output){
  TESa <- TES_output$TESa
  TESb <- TES_output$TESb
  par(mfrow = c(1, 2),mgp=c(2.5,1,0),las=1,mar=c(4,4,2,1))
  if (is.na(TESa$par[1])) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="(a)")
    text(1, 1, 'NA')
  } else {
    plot(x=TESa$result$Logm,y=TESa$result$value,xlim=c(0,2*TESa$xmax),ylim=c(0,1.2*as.numeric(TESa$par[1])), xlab = "ln(m)", ylab = "Value", main="(a)")
    lines(TESa$Predx, TESa$Predy, col="red")
    abline(h=TESa$par[1],lty=2)
  }
  
  if (is.na(TESb$par[1])) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, 'NA')
  } else {
    plot(x=TESb$result$Logm,y=TESb$result$value,xlim=c(0,2*TESb$xmax),ylim=c(0,1.2*as.numeric(TESb$par[1])), xlab = "ln(m)", ylab = "Value", main="(b)")
    lines(TESb$Predx, TESb$Predy, col="red")
    abline(h=TESb$par[1],lty=2)
  }
}

ESa.c <- ES(btl_order_c$n,m = 14, method = "a")
ESb.c <- ES(btl_order_c$n, method = "b")

TES_c <- TES(btl_order_c$n)
plot.TES(TES_c)

ESa.m <- ES(btl_order_m$n,m = 20, method = "a")
ESb.m <- ES(btl_order_m$n, method = "b")

TES_m <- TES(btl_order_m$n)
plot.TES(TES_m)

ESa.w <- ES(btl_order_w$n,m = 20, method = "a")
ESb.w <- ES(btl_order_w$n, method = "b")

TES_w <- TES(btl_order_w$n)
plot.TES(TES_w)

ESa.r <- ES(btl_order_r$n,m = 20, method = "a")
ESb.r <- ES(btl_order_r$n, method = "b")

TES_r <- TES(btl_order_r$n)
plot.TES(TES_r)


# iNEXT example

data(spider)
out1 <- iNEXT(spider, q=c(0,1,2), datatype="abundance")

data(bird)
out2 <- iNEXT(bird, q=0, datatype="abundance")
out2

div.data <- btl_sums_patch |>
  column_to_rownames(var = "sp_code") 


div.btl <- iNEXT(div.data, q = c(0,1,2), datatype = "abundance")


z <- iNEXT(div.data, q=c(0,1,2), datatype="abundance")
ggiNEXT(z, facet.var="Order.q", color.var="Assemblage")
ggiNEXT(z, type = 2, se = TRUE)
ggiNEXT(z, facet.var="Both", color.var="Both")

div.btl$DataInfo
div.btl$AsyEst
# total NTES_c# total N by date type --------------------------------------------------


N_by_patch<-btl_data %>% 
  group_by(date)


  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  select(-c(sample_id,point,month,day,year,sum)) %>% 
  pivot_longer(pvin:osyl,names_to = "species",values_to = "n") %>% 
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


