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
#mapping = read_xlsx("C:/Users/laan208/PNNL/Core Richland and Sequim Lab-Field Team - Documents/Data Generation and Files/ICON_ModEx_SSS/08_CN/01_RawData/20221101_Data_Raw_CN_SBR_RC2_SSS/20221101_Mapping_Raw_CN_SBR_RC2_SSS.xlsx")

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


toc_samples_singles =
  toc_samples %>% 
  group_by(Parent_ID) %>% 
  filter(n()== 1) %>% 
  select(c(Parent_ID, toc_percent, tn_percent)) %>% 
  mutate(Method_Deviation = "N/A")


### Maggi Dist Matrix ####

#calculate mean and cv for replicate samples
outliers <- toc_samples %>% 
  group_by(Parent_ID) %>% 
  mutate(mean_toc_all = mean(toc_percent)) %>% 
  mutate(cv_toc_all = (sd(toc_percent)/mean(toc_percent))*100) %>% 
  filter(n() > 1) %>% 
  ungroup() 

outliers$flag <- NA

#make final data frame
toc_final <- as.data.frame(matrix(NA, ncol = 10, nrow = 1))

colnames(toc_final) = c("toc.temp", "Sample_ID",  "tn_percent", "Randomized_ID", "Parent_ID", "mean_toc_all", "cv_toc_all", "mean_toc_rem", "cv_toc_rem", "flag")

#Number of unique parent IDs with replicates to loop through
unique.samples = unique(outliers$Parent_ID)

for (i in 1:length(unique.samples)) {
  
  ## Subset replicates
  data_subset = subset(outliers, outliers$Parent_ID == unique.samples[i])
  
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
    
    toc.removed <- toc.removed[!duplicated(toc.removed$Sample_ID), ]
    
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

toc_merged_removals <- left_join(outliers, toc_final_flag, by = c("Sample_ID", "Randomized_ID", "tn_percent", "Parent_ID", "mean_toc_all", "cv_toc_all")) %>% 
  mutate(flag_removal = if_else(is.na(toc_percent.y), "Outlier to remove", "N/A")) %>% 
  left_join(toc_samples, by = c("Sample_ID", "Randomized_ID", "tn_percent", "Parent_ID")) %>% 
  mutate(toc_percent = ifelse(flag_removal == "Outlier to remove", "N/A", toc_percent.x)) %>% 
  select(-c(flag.x, flag.y, toc_percent.x, toc_percent.y)) %>% 
  relocate(toc_percent, .after = Sample_ID)


clean_replicates <- 
  toc_merged_removals %>% 
  filter(flag_removal == "N/A") %>% 
  group_by(Parent_ID) %>% 
  mutate(toc_percent = as.numeric(toc_percent)) %>% 
  dplyr::summarise(toc_percent = mean(toc_percent),
                   tn_percent = mean(tn_percent),
                   toc_percent = round(toc_percent, 2),
                   tn_percent = round(tn_percent, 2),
                   n = n()) %>% 
  mutate(Method_Deviation = case_when(n >= 3 ~ "CN_000", n == 1 ~ "N/A")) %>% 
  select(c(Parent_ID, toc_percent, tn_percent, Method_Deviation))

toc_final_samples <- 
  rbind(clean_replicates, toc_samples_singles)

toc_final_samples$Parent_ID <- paste0(toc_final_samples$Parent_ID,"_SCN")


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

toc_dp <- toc_final_samples %>% 
  # filter(!grepl("SSS001|SSS006|SSS025|SSS004|SSS023|SSS047|SSS016|SSS008", Parent_ID)) %>% #removing samples with any reps over 1 percent toc
  select(Parent_ID, toc_percent, tn_percent, Method_Deviation) %>% 
  mutate(Material = "Sediment") %>% 
  relocate(Material, .after = Parent_ID) %>% 
  rename(Sample_Name = Parent_ID,
         '01395_C_percent_per_mg' = toc_percent,
         '01397_N_percent_per_mg' = tn_percent,
         Methods_Deviation = Method_Deviation) 
  
#formatted for data package file 
toc_dp %>% write.csv("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ICON_ModEx_SSS/08_CN/02_FormattedData/SSS_CN_ReadyForBoye_01-12-2024.csv", row.names = F)  
  
  
  
  
  
  
  





















