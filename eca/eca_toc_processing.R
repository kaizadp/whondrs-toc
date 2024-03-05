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

toc_run4 = read.csv("toc_data/2023-11-07_stegen_eca_part2-v3.csv") %>% 
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

full_sample_toc <- bind_rows(toc_samples3, toc_samples4)

#joining mapping with both runs
toc_mapping <- full_mapping %>% 
  full_join(full_sample_toc) %>% 
  filter(!is.na(toc_percent)) %>% 
  select(sample_id, randomized_id, cn_notes, parent_id, toc_percent, tn_percent, memo)


# QAQC --------------------------------------------------------------
full_processed_toc <- bind_rows(toc_processed3, toc_processed4)

### 1. blanks  ----
blanks = 
  full_processed_toc %>% 
  dplyr::select(name, c_area, c_percent, n_area, n_percent, memo) %>% 
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


### 2. reps ----

reps_c_analytical = 
  full_processed_toc %>% 
  dplyr::select(name, c_percent) %>% 
  filter(grepl("rep", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep = c_percent) %>% 
  left_join(full_processed_toc %>% dplyr::select(name, c_percent)) %>% 
  rowwise() %>% 
  mutate(mean = mean(c(rep, c_percent)),
         sd = round(sd(c(rep, c_percent)),3),
         cv = round(sd/mean,3))


reps_n_analytical = 
  full_processed_toc %>% 
  dplyr::select(name, n_percent) %>% 
  filter(grepl("rep", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep = n_percent) %>% 
  left_join(full_processed_toc %>% dplyr::select(name, n_percent)) %>% 
  rowwise() %>% 
  mutate(mean = mean(c(rep, n_percent)),
         sd = round(sd(c(rep, n_percent)),3),
         cv = round(sd/mean,3))

reps_c_analytical %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

reps_n_analytical %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

reps_n_analytical %>% 
  ggplot(aes(x = cv, y = mean))+
  geom_point()

reps_n_analytical %>% 
  filter(cv > 0.16) %>% 
  ggplot(aes(x = cv, y = mean))+
  geom_point()



### 3. aspartics/standards ----
aspartics = 
  full_processed_toc %>% 
  filter(name == "aspartic")

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


### 4. reps by site ----

reps_c_by_site = 
  toc_mapping %>% 
  mutate(treat = case_when(grepl("W",sample_id)~"Wet",
                            grepl("D", sample_id) ~"Dry")) %>% 
  filter(!grepl("rerunning for TN only|power bump", cn_notes)) %>% 
  group_by(parent_id, treat) %>% 
  summarise(mean = mean(toc_percent),
            sd = round(sd(toc_percent),3),
            cv = round(sd/mean,3))

reps_c_by_site %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

reps_n_by_site = 
  toc_mapping  %>% 
  group_by(parent_id) %>% 
  summarise(mean = mean(tn_percent),
            sd = round(sd(tn_percent),3),
            cv = round(sd/mean,3))

reps_n_by_site %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

