## WHONDRS ECA
## TOC PROCESSING
## 
## Kaizad F. Patel
## March 2023

#################### #
#################### #

library(tidyverse)
library(lubridate)



# RUN-1 ONLY --------------------------------------------------------------
## running just one replicate from each set for a general understanding of C values

toc_run1 = read.csv("eca/toc_data/2023-02-21-stegen_eca_run1.csv") %>% janitor::clean_names()

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
         tn_percent = n_percent) %>% 
  # some samples were re-run, so we want only the most recent ones
  arrange(as.numeric(name))

## export
toc_samples %>% write.csv("eca/processed/toc_eca_run1_2023-03-06.csv", row.names = F)

# RUN-2 ONLY --------------------------------------------------------------
## running just one replicate from each set for a general understanding of C values

toc_run2 = read.csv("eca/toc_data/2023-02-21-stegen_eca_run1-contd.csv") %>% janitor::clean_names()

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
  mutate(name = as.numeric(name),
         name = as.character(name)) %>% 
  filter(!is.na(name)) %>% 
  dplyr::select(name, c_percent, n_percent, memo) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent) %>% 
  # some samples were re-run, so we want only the most recent ones
  arrange(as.numeric(name))

## export
toc_samples2 %>% write.csv("eca/processed/toc_eca_run2_2023-11-01.csv", row.names = F)

