## WHONDRS SSS
## TOC PROCESSING
## 
## Kaizad F. Patel & Sophia McKever
## August 2023

#################### #
#################### #

library(tidyverse)
library(lubridate)
library(readxl)



# RUN-1 ONLY --------------------------------------------------------------
## running just one replicate from each set for a general understanding of C values

toc_sss = read.csv("sss/toc/2023-01-05_whondrs_spatial.csv") %>% janitor::clean_names()
mapping = read_xlsx("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ICON_ModEx_SSS/08_CN/01_RawData/20221101_Data_Raw_CN_SBR_RC2_SSS/20221101_Mapping_Raw_CN_SBR_RC2_SSS.xlsx")

## process toc data ----
toc_processed = 
  toc_sss %>% 
  rename(n_percent = n,
         c_percent = c) %>% 
  dplyr::select(info, name, weight_mg, 
                n_area, n_percent, n_factor, 
                c_area, c_percent, c_factor, 
                memo) %>% 
  # remove samples with oxygen breakthrough ("bl") - these have been repeated
  #filter(!info %in% "bl") %>% 
  #filter(!memo %in% "skip") %>% 
  force()

## samples
toc_samples = 
  toc_processed %>% 
  filter(grepl("SSS", name),
         !grepl("-rep",name)) %>% 
  dplyr::select(name, c_percent, n_percent, memo) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent,
         Sample_ID = name) %>% 
  left_join(mapping, by = c('Sample_ID')) %>% 
  select(Sample_ID, toc_percent, tn_percent, Randomized_ID) %>% 
  mutate(Parent_ID = str_extract(Randomized_ID, ".{6}(?=_)"))



## QA-QC ----
### 1. blanks
blanks = 
  toc_processed %>% 
  dplyr::select(name, c_area, c_percent, n_area, n_percent) %>% 
  filter(grepl("MT", name, ignore.case = TRUE)) 

# blanks for C
blanks %>% 
  ggplot(aes(x = "blank", y = c_percent)) + 
  geom_violin()+
  geom_jitter(width = 0.1)

blanks %>% 
  ggplot(aes(x = "blank", y = c_area)) + 
  geom_violin()+
  geom_jitter(width = 0.1)


# blanks for N
blanks %>% 
  filter(n_area < 2000) %>% 
  ggplot(aes(x = "blank", y = n_percent)) + 
  geom_violin()+
  geom_jitter(width = 0.1)

blanks %>% 
  filter(n_area < 2000) %>% 
  ggplot(aes(x = "blank", y = n_area)) + 
  geom_violin()+
  geom_jitter(width = 0.1)


### 2. reps
reps_c = 
toc_samples %>% 
  group_by(Parent_ID) %>% 
  summarise(mean = mean(c(rep, toc_percent)),
            sd = round(sd(c(rep, toc_percent)),3),
            cv = round(sd/mean,3))


reps_c = 
  toc_processed %>% 
  dplyr::select(name, c_percent) %>% 
  filter(grepl("rep", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep = c_percent) %>% 
  left_join(toc_samples %>% dplyr::select(name, toc_percent)) %>% 
  rowwise() %>% 
  mutate(mean = mean(c(rep, toc_percent)),
         sd = round(sd(c(rep, toc_percent)),3),
         cv = round(sd/mean,3))

reps_n = 
  toc_processed %>% 
  dplyr::select(name, n_percent) %>% 
  filter(grepl("rep", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep = n_percent) %>% 
  left_join(toc_samples %>% dplyr::select(name, tn_percent)) %>% 
  rowwise() %>% 
  mutate(mean = mean(c(rep, tn_percent)),
         sd = round(sd(c(rep, tn_percent)),3),
         cv = round(sd/mean,3))

reps_c %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

reps_n %>% 
  ggplot(aes(x = cv))+
  geom_histogram()


### 3. aspartics/standards
aspartics = 
  toc_processed %>% 
  filter(grepl("aspartic", name))

aspartics %>% 
  filter(weight_mg > 0.4 & weight_mg < 0.65) %>% 
  ggplot(aes(y = c_factor, x = "aspartic"))+
  geom_violin()+
  geom_jitter(width = 0.2)

aspartics %>% 
  filter(weight_mg > 0.4 & weight_mg < 0.65) %>% 
  ggplot(aes(y = n_factor, x = "aspartic"))+
  geom_violin()+
  geom_jitter(width = 0.2)



## export
toc_samples %>% write.csv("eca/processed/toc_eca_run1_2023-03-06.csv", row.names = F)


