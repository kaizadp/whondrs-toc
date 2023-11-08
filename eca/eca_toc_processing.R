## WHONDRS ECA
## TOC PROCESSING
## 
## Kaizad F. Patel
## March 2023

#################### #
#################### #

library(tidyverse)
library(lubridate)
library(readxl)

mapping <- read_xlsx("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ECA/CN/01_RawData/20230306_Mapping_SCN_ECA_EC_R1-169.xlsx")%>% 
  mutate(Parent_ID = str_extract(Sample_ID, ".{5}(?=_)"))

# RUN-1 ONLY --------------------------------------------------------------
## running just one replicate from each set for a general understanding of C values

toc_run1 = read.csv("toc_data/2023-02-21-stegen_eca_run1.csv") %>%
  janitor::clean_names()

## process toc data ----
toc_processed = 
  toc_run1 %>% 
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
  mutate(name = as.numeric(name),
         name = as.character(name)) %>% 
  filter(!is.na(name)) %>% 
  dplyr::select(name, c_percent, n_percent, memo) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent,
         Randomized_ID = name) %>% 
  # some samples were re-run, so we want only the most recent ones
  arrange(as.numeric(Randomized_ID))

#joining mapping with run 1
toc_mapping <- mapping %>% 
  full_join(toc_samples) %>% 
  filter(!is.na(toc_percent)) %>% 
  select(Sample_ID, Randomized_ID, 'CN Notes', Parent_ID, toc_percent, tn_percent, memo)

## export
toc_samples %>% write.csv("eca/processed/toc_eca_run1_2023-03-06.csv", row.names = F)
toc_mapping %>% write.csv("processed/toc_eca_run1_mapping.csv", row.names = F)

# RUN-2 ONLY --------------------------------------------------------------
## running just one replicate from each set for a general understanding of C values

toc_run2 = read.csv("toc_data/2023-02-21-stegen_eca_run1-contd.csv") %>% 
  janitor::clean_names() %>% 
  filter(!grepl("2/21/2023|2/22/2023|2/23/2023|2/24/2023", date_time)) 



## process toc data ----
toc_processed2 = 
  toc_run2 %>% 
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
toc_samples2 = 
  toc_processed2 %>% 
  filter(!is.na(name),
         !grepl("runin|aspartic|MT|moran-LOW|moran-MED|moran-low|moran-med", name)) %>% 
  dplyr::select(name, c_percent, n_percent, memo) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent) %>% 
  # some samples were re-run, so we want only the most recent ones
  arrange(as.numeric(name))

## export
toc_samples2 %>% write.csv("processed/toc_eca_run2_2023-11-01.csv", row.names = F)

