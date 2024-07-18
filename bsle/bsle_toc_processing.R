## RC3 BSLE
## TOC PROCESSING
## 
## Kaizad F. Patel & Sophia McKever
## July 2024

#################### #
#################### #

library(tidyverse)
library(lubridate)
library(readxl)
library(viridis)


#published data set of BSLE

pub_bsle = read.csv("toc/BSLE_CN.csv", skip = 2) %>% 
  janitor::clean_names() %>% 
  select(sample_name, x01463_c_percent, x01472_n_percent) %>% 
  filter(grepl("BSLE", sample_name)) %>% 
  rename(toc_percent3 = x01463_c_percent,
         tn_percent3 = x01472_n_percent)

#second test run of BSLE

tc_bsle = read.csv("toc/2024-07-16_sm_bsle.csv") %>% 
  janitor::clean_names()
#toc_sss = as.data.frame("2023_01_05_whondrs_spatial") %>% janitor::clean_names()

## process toc data --
toc_processed2 = 
  tc_bsle %>% 
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
  filter(grepl("BSLE|sed", name),
         !grepl("-rep",name)) %>% 
  dplyr::select(name, c_percent, n_percent) %>% 
  rename(toc_percent2 = c_percent,
         tn_percent2 = n_percent,
         sample_name = name)

toc_samples2$sample_name <- paste0(toc_samples2$sample_name,"-solid")

#First run of BSLE 

toc_bsle = read.csv("toc/2023-10-20-kfp_BSLE.csv") %>% 
  janitor::clean_names()
#toc_sss = as.data.frame("2023_01_05_whondrs_spatial") %>% janitor::clean_names()

## process toc data --
toc_processed = 
  toc_bsle %>% 
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
  filter(grepl("BSLE", name),
         !grepl("-rep",name)) %>% 
  dplyr::select(name, c_percent, n_percent) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent,
         Sample_ID = name) %>% 
  mutate(sample_name = str_remove(Sample_ID, "s$")) %>% 
  select(sample_name, toc_percent, tn_percent)

toc_samples$sample_name <- paste0(toc_samples$sample_name,"-solid")




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


### 2. aspartics/standards
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


final_toc = pub_bsle %>% 
  left_join(toc_samples, by = "sample_name") %>% 
  left_join(toc_samples2, by = "sample_name") %>% 
  relocate(toc_percent, .after = toc_percent3) %>% 
  relocate(toc_percent2, .after = toc_percent) %>% 
  rename(initial_toc = toc_percent,
         test_tc = toc_percent2,
         pub_toc = toc_percent3,
         initial_tn = tn_percent,
         test_tn = tn_percent2,
         pub_tn = tn_percent3)



## export
final_toc %>% write.csv("toc/20240718_BSLE_TOC_TN_Data.csv", row.names = F)













