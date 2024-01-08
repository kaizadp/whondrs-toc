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
library(viridis)



# RUN-1 ONLY --------------------------------------------------------------
## running just one replicate from each set for a general understanding of C values

toc_sss = read.csv("toc/2023-01-05_whondrs_spatial-contd.csv") %>% janitor::clean_names()
#toc_sss = as.data.frame("2023_01_05_whondrs_spatial") %>% janitor::clean_names()
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

toc_samples_summary = 
  toc_samples %>% 
  group_by(Parent_ID) %>% 
  dplyr::summarise(toc_percent = mean(toc_percent),
                   tn_percent = mean(tn_percent),
                   toc_percent = round(toc_percent, 2),
                   tn_percent = round(tn_percent, 2),
                   n = n()) %>% 
  mutate(Method_Deviation = case_when(n >= 3 ~ "CN_000", n == 1 ~ "N/A"))

toc_samples_summary$Parent_ID <- paste0(toc_samples_summary$Parent_ID,"_SCN")

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

reps_c_analytical = 
  toc_processed %>% 
  dplyr::select(name, c_percent) %>% 
  filter(grepl("rep", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep = c_percent) %>% 
  left_join(toc_processed %>% dplyr::select(name, c_percent)) %>% 
  rowwise() %>% 
  mutate(mean = mean(c(rep, c_percent)),
         sd = round(sd(c(rep, c_percent)),3),
         cv = round(sd/mean,3))


reps_n_analytical = 
  toc_processed %>% 
  dplyr::select(name, n_percent) %>% 
  filter(grepl("rep", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep = n_percent) %>% 
  left_join(toc_processed %>% dplyr::select(name, n_percent)) %>% 
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


### 4. reps by site

reps_c_by_site = 
  toc_samples %>% 
  group_by(Parent_ID) %>% 
  summarise(mean = mean(toc_percent),
            sd = round(sd(toc_percent),3),
            cv = round(sd/mean,3))

reps_c_by_site %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

reps_n_by_site = 
  toc_samples %>% 
  group_by(Parent_ID) %>% 
  summarise(mean = mean(tn_percent),
            sd = round(sd(tn_percent),3),
            cv = round(sd/mean,3))

reps_n_by_site %>% 
  ggplot(aes(x = cv))+
  geom_histogram()


highlighted_parent_ids <- c("SSS012", "SSS001", "SSS020", "SSS030", "SSS046", 
                            "SSS006", "SSS025", "SSS004", "SSS023", "SSS047", 
                            "SSS016", "SSS008")


toc_distribution <- ggplot(data = toc_samples, aes(x = toc_percent)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(data = toc_samples[toc_samples$Parent_ID %in% highlighted_parent_ids, ],
             aes(xintercept = toc_percent, color = Parent_ID), 
             linetype = "dashed", size = 0.8) +
  scale_color_manual(name = "Parent ID",
                     values = c(SSS012 = "red", SSS001 = "blue", SSS020 = "green",
                                SSS030 = "purple", SSS046 = "orange", SSS006 = "pink",
                                SSS025 = "brown", SSS004 = "cyan", SSS023 = "gray",
                                SSS047 = "violet", SSS016 = "yellow", SSS008 = "darkgreen")) +
  labs(x = "TOC Percent", y = "Frequency", title = "Distribution of TOC Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))


bio_reps <- toc_samples %>% 
  filter(grepl("SSS012|SSS001|SSS020|SSS030|SSS046|SSS006|SSS025|SSS004|SSS023|SSS047|SSS016|SSS008", Parent_ID)) 

bio_reps_plot <- ggplot(data = bio_reps, aes(x = Parent_ID, y = toc_percent))+
  geom_point()



## export
toc_samples %>% write.csv("sss/processed/toc.csv", row.names = F)


#Data Package ----
#ICON Data Package set, only publishing samples that dont need reruns. Remainder will be published in Jan. 2024 version.

toc_dp <- toc_samples_summary %>% 
  filter(!grepl("SSS001|SSS006|SSS025|SSS004|SSS023|SSS047|SSS016|SSS008", Parent_ID)) %>% #removing samples with any reps over 1 percent toc
  select(Parent_ID, toc_percent, tn_percent, Method_Deviation) %>% 
  mutate(Material = "Sediment") %>% 
  relocate(Material, .after = Parent_ID) %>% 
  rename(Sample_Name = Parent_ID,
         '01395_C_percent_per_mg' = toc_percent,
         '01397_N_percent_per_mg' = tn_percent,
         Methods_Deviation = Method_Deviation) 
  
#formatted for data package file 
toc_dp %>% write.csv("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ICON_ModEx_SSS/08_CN/02_FormattedData/SSS_CN_ReadyForBoye_11-08-2023.csv", row.names = F)  
  
  
  
  
  
  
  





















