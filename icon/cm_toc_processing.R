## WHONDRS ICON
## TOC PROCESSING
## 
## Kaizad F. Patel & Sophia McKever
## March 2024

#################### #
#################### #

library(tidyverse)
library(readxl)
library(viridis)

#setwd("C:/GitHub/s19s/icon")

#ICON RUN --------------------------------------------------------------

toc_cm = read.csv("toc/2024-02-16-sm-cm-v2.csv") %>% 
  janitor::clean_names() %>% 
  filter(!grepl("2/23/2024|2/26/2024|4/5/2024", date_time))

mapping = read_xlsx("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ICON_ModEx_SSS/08_CN/01_RawData/20230524_Data_Raw_SCN_SBR_RC4_CM_R1-89/20230524_Mapping_Raw_SCN_SBR_RC4_CM_R1-129.xlsx") %>% 
  #read_xlsx("C:/Users/laan208/PNNL/Core Richland and Sequim Lab-Field Team - Documents/Data Generation and Files/ICON_ModEx_SSS/08_CN/01_RawData/20230524_Data_Raw_SCN_SBR_RC4_CM_R1-89/20230524_Mapping_Raw_SCN_SBR_RC4_CM_R1-129.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(parent_id = str_extract(sample_id, ".{6}(?=_)")) %>% 
  filter(!is.na(sample_id))


## process toc data ----
toc_processed = 
  toc_cm %>% 
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
  filter(grepl("CM", name),
         !grepl("-rep",name)) %>% 
  dplyr::select(name, c_percent, n_percent, memo) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent,
         randomized_id = name) %>% 
  left_join(mapping, by = c('randomized_id')) %>% 
  select(sample_id, toc_percent, tn_percent, randomized_id, parent_id)


toc_samples_singles =
  toc_samples %>% 
  group_by(parent_id) %>% 
  filter(n()== 1) %>% 
  select(c(parent_id, toc_percent, tn_percent)) %>% 
  mutate(Method_Deviation = "N/A")


# QA-QC --------------------------------------------------------------

### 1. blanks ----
blanks = 
  toc_processed %>% 
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

reps_n_analytical %>% 
  ggplot(aes(x = cv, y = mean))+
  geom_point()


### 3. aspartics/standards ----
aspartics = 
  toc_processed %>% 
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
  toc_samples %>% 
  group_by(parent_id) %>% 
  summarise(mean = mean(toc_percent),
            sd = round(sd(toc_percent),3),
            cv = round(sd/mean,3))

reps_c_by_site %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

reps_n_by_site = 
  toc_samples %>% 
  group_by(parent_id) %>% 
  summarise(mean = mean(tn_percent),
            sd = round(sd(tn_percent),3),
            cv = round(sd/mean,3))

reps_n_by_site %>% 
  ggplot(aes(x = cv))+
  geom_histogram()


### 5. acid vs no acid test run ----
#pulling in data for test run that included analytical reps acidified and non acidified to see if TN values differ

test_cm = read.csv("toc/2024-02-16-sm-cm-v3.csv") %>% 
  janitor::clean_names() %>% 
  filter(grepl("4/29/2024|5/1/2024|5/2/2024", date_time))

test_processed = 
  test_cm %>% 
  rename(n_percent = n,
         c_percent = c) %>% 
  dplyr::select(info, name, weight_mg, 
                n_area, n_percent, n_factor, 
                c_area, c_percent, c_factor, c_n_ratio,
                memo) %>% 
  # remove samples with oxygen breakthrough ("bl") - these have been repeated
  #filter(!info %in% "bl") %>% 
  #filter(!memo %in% "skip") %>% 
  force()

test_blanks = 
  test_processed %>% 
  dplyr::select(name, c_area, c_percent, n_area, n_percent, memo) %>% 
  filter(grepl("MT", name, ignore.case = TRUE)) 



test_c_analytical = 
  test_processed %>% 
  dplyr::select(name, c_percent) %>% 
  filter(grepl("CM", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep = c_percent) 

test_c_analytical$extracted_name <- gsub("\\d$", "", test_c_analytical$name) 

test_c_data =
  test_c_analytical %>% 
  dplyr::select(extracted_name, rep) %>% 
  group_by(extracted_name) %>% 
  mutate(mean = mean(c(rep),3),
         sd = round(sd(c(rep)),3),
         cv = round(sd/mean,3))


test_n_analytical = 
  test_processed %>% 
  dplyr::select(name, n_percent) %>% 
  filter(grepl("CM", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep = n_percent) 

test_n_analytical$extracted_name <- gsub("\\d$", "", test_n_analytical$name) 

test_n_data =
  test_n_analytical %>% 
  dplyr::select(extracted_name, rep) %>% 
  group_by(extracted_name) %>% 
  mutate(mean = mean(c(rep), 3),
         sd = round(sd(c(rep)),3),
         cv = round(sd/mean,3))

test_c_data %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

test_n_data %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

test_n_data %>% 
  ggplot(aes(x = cv, y = mean, color = extracted_name))+
  geom_point()

test_n_data %>% write.csv("C:/GitHub/s19s/icon/tn_test_data.csv", row.names = F)


# Maggi Dist Matrix  --------------------------------------------------------------

#calculate mean and cv for replicate samples
outliers <- toc_samples %>% 
  group_by(parent_id) %>% 
  mutate(mean_toc_all = mean(toc_percent)) %>% 
  mutate(cv_toc_all = (sd(toc_percent)/mean(toc_percent))*100) %>% 
  filter(n() > 1) %>% 
  ungroup() 

outliers$flag <- NA

#make final data frame
toc_final <- as.data.frame(matrix(NA, ncol = 10, nrow = 1))

colnames(toc_final) = c("toc.temp", "sample_id",  "tn_percent", "randomized_id", "parent_id", "mean_toc_all", "cv_toc_all", "mean_toc_rem", "cv_toc_rem", "flag")

#Number of unique parent IDs with replicates to loop through
unique.samples = unique(outliers$parent_id)

for (i in 1:length(unique.samples)) {
  
  ## Subset replicates
  data_subset = subset(outliers, outliers$parent_id == unique.samples[i])
  
  ## Pull out toc percent values
  toc.temp = as.numeric(data_subset$toc_percent)
  
  ## Calculate standard deviation, average, and coefficient of variation of rates
  toc.temp.sd <- sd(toc.temp)
  toc.temp.mean <- mean(toc.temp)
  CV = abs((toc.temp.sd/toc.temp.mean)*100)
  
  #looping to get 3 best samples
  for (sample.reduction in 1:5)  {
    
    if (length(toc.temp) > 3) {
      
      dist.temp = as.matrix(abs(dist(toc.temp)))
      dist.comp = numeric() 
      
      for(toc.now in 1:ncol(dist.temp)) {
        
        dist.comp = rbind(dist.comp,c(toc.now,sum(dist.temp[,toc.now])))
        
      }
      
      dist.comp[,2] = as.numeric(dist.comp[,2])
      toc.temp = toc.temp[-which.max(dist.comp[,2])]
      
      toc.temp.sd <- sd(toc.temp)
      toc.temp.mean <- mean(toc.temp)
      toc.temp.cv <- abs((toc.temp.sd/toc.temp.mean)*100)
      CV = toc.temp.cv
      toc.temp.range <- max(toc.temp) - min(toc.temp)
      range = toc.temp.range
      
    } 
  }
  
  if (length(toc.temp) >= 3) {
    
    toc.combined <- as.data.frame(toc.temp)
    
    toc.removed <- merge(toc.combined, data_subset, by.x = "toc.temp", by.y = "toc_percent", all.x = TRUE)
    
    toc.removed <- toc.removed[!duplicated(toc.removed$sample_id), ]
    
    toc.removed$cv_toc_rem = as.numeric(abs((sd(toc.temp)/mean(toc.temp))*100))
    
    toc.removed$mean_toc_rem = as.numeric(mean(toc.temp))
    
  }
  
  toc_final = rbind(toc.removed, toc_final)
  
  rm('toc.temp')
}

## This data frame has removed samples
toc_final_flag <- toc_final %>% 
  mutate(flag = if_else(cv_toc_all < cv_toc_rem, "Issue in dropping sample", "N/A")) %>% 
  rename(toc_percent = toc.temp) 

### End Sample Removal ####

## Joined to see what sample is being removed

toc_merged_removals <- left_join(outliers, toc_final_flag, by = c("sample_id", "randomized_id", "tn_percent", "parent_id", "mean_toc_all", "cv_toc_all")) %>% 
  mutate(flag_removal = if_else(is.na(toc_percent.y), "Outlier to remove", "N/A")) %>% 
  left_join(toc_samples, by = c("sample_id", "randomized_id", "tn_percent", "parent_id")) %>% 
  mutate(toc_percent = ifelse(flag_removal == "Outlier to remove", "N/A", toc_percent.x)) %>% 
  select(-c(flag.x, flag.y, toc_percent.x, toc_percent.y)) %>% 
  relocate(toc_percent, .after = sample_id)


clean_replicates <- 
  toc_merged_removals %>% 
  filter(flag_removal == "N/A") %>% 
  group_by(parent_id) %>% 
  mutate(toc_percent = as.numeric(toc_percent)) %>% 
  dplyr::summarise(toc_percent = mean(toc_percent),
                   tn_percent = mean(tn_percent),
                   toc_percent = round(toc_percent, 2),
                   tn_percent = round(tn_percent, 2),
                   n = n()) %>% 
  mutate(Method_Deviation = case_when(n >= 3 ~ "CN_000", n == 1 ~ "N/A")) %>% 
  select(c(parent_id, toc_percent, tn_percent, Method_Deviation))

toc_final_samples <- 
  rbind(clean_replicates, toc_samples_singles)


toc_final_samples$parent_id <- paste0(toc_final_samples$parent_id,"_SCN")


#Data Package ----
#ICON Data Package set

toc_dp <- toc_final_samples %>% 
  select(parent_id, toc_percent, tn_percent, Method_Deviation) %>% 
  mutate(Material = "Sediment") %>% 
  relocate(Material, .after = parent_id) %>% 
  rename(Sample_Name = parent_id,
         '01395_C_percent_per_mg' = toc_percent,
         '01397_N_percent_per_mg' = tn_percent,
         Methods_Deviation = Method_Deviation) 
  
#formatted for data package file 
toc_dp %>% write.csv("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ICON_ModEx_SSS/08_CN/02_FormattedData/CM_CN_ReadyForBoye_05-28-2024.csv", row.names = F)  
  
  
  
  
  
  
  





















