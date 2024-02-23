## WHONDRS ECA
## TOC PROCESSING
## 
## Kaizad F. Patel & Sophia McKever
## March 2023

#################### #
#################### #

library(tidyverse)
library(lubridate)
library(readxl)


# Sample Mapping --------------------------------------------------------------
b1_mapping <- read_xlsx("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ECA/CN/01_RawData/20230306_Mapping_SCN_ECA_EC_R1-169.xlsx")%>% 
  janitor::clean_names() %>% 
  mutate(parent_id = str_extract(sample_id, "[:digit:]+(?=_)"),
         parent_id = case_when(nchar(parent_id) ==3 ~ parent_id, 
                               nchar(parent_id) ==2 ~ str_c(0, parent_id)),
         parent_id = paste0("EC_", parent_id)) %>% 
  filter(!is.na(sample_id),
         !grepl("Ran in EMSL", cn_notes))

b2_mapping <- read_xlsx("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ECA/CN/01_RawData/20230628_Mapping_SCN_EC_EC_R1-288.xlsx")%>% 
  janitor::clean_names() %>% 
  mutate(parent_id = str_extract(sample_id, ".{6}(?=_)")) %>% 
  filter(!is.na(sample_id))

full_mapping <- bind_rows(b1_mapping, b2_mapping)

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
  select(Sample_ID, Randomized_ID, 'CN Notes', parent_id, toc_percent, tn_percent, memo)

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



# RUN-3 ONLY --------------------------------------------------------------
## full run of batch 1 and 2 ECA samples 

toc_run3 = read.csv("toc_data/2023-02-21-stegen_eca_run1-v3.csv") %>% 
  janitor::clean_names()

toc_run4 = read.csv("toc_data/2023-11-07_stegen_eca_part2.csv") %>% 
  janitor::clean_names()

## process toc data of batch 2 ----
toc_processed3 = 
  toc_run3 %>% 
  rename(n_percent = n,
         c_percent = c) %>% 
  dplyr::select(info, name, weight_mg, 
                n_area, n_percent, n_factor, 
                c_area, c_percent, c_factor, 
                memo) %>% 
  # remove samples with oxygen breakthrough ("bl") - these have been repeated
  #filter(!info %in% "bl") %>% 
  filter(!memo %in% "skip") %>% 
  force()

## samples
toc_samples3 = 
  toc_processed3 %>% 
  filter(!is.na(name),
         !grepl("runin|RunIn|aspartic|MT|mt|moran-LOW|moran-MED|moran-low|moran-med", name)) %>% 
  dplyr::select(name, c_percent, n_percent, memo) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent,
         randomized_id = name) 

#toc_samples3$randomized_id = str_remove(toc_samples3$randomized_id, "EC_R|EC_|R")


## process toc data of batch 1 ----
toc_processed4 = 
  toc_run4 %>% 
  rename(n_percent = n,
         c_percent = c) %>% 
  dplyr::select(info, name, weight_mg, 
                n_area, n_percent, n_factor, 
                c_area, c_percent, c_factor, 
                memo) %>% 
  # remove samples with oxygen breakthrough ("bl") - these have been repeated
  #filter(!info %in% "bl") %>% 
  filter(!memo %in% "skip") %>% 
  force()

## samples
toc_samples4 = 
  toc_processed4 %>% 
  filter(!is.na(name),
         !grepl("runin|RunIn|aspartic|MT|mt|moran-LOW|moran-MED|moran-low|moran-med", name)) %>% 
  dplyr::select(name, c_percent, n_percent, memo) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent,
         randomized_id = name) 

full_toc <- bind_rows(toc_samples3, toc_samples4)

#joining mapping with batch 2 run
toc_mapping <- full_mapping %>% 
  full_join(full_toc) %>% 
  filter(!is.na(toc_percent)) %>% 
  select(sample_id, randomized_id, cn_notes, parent_id, toc_percent, tn_percent, memo)








