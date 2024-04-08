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

full_mapping <- bind_rows(b1_mapping, b2_mapping) %>% 
  filter(!grepl("skip", cn_notes))

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

toc_run4 = read.csv("toc_data/2023-11-07_stegen_eca_part2-v4.csv") %>% 
  janitor::clean_names()

full_toc <- bind_rows(toc_run3, toc_run4)

## process toc data of batch 2 ----
toc_processed = 
  full_toc %>% 
  rename(n_percent = n,
         c_percent = c) %>% 
  dplyr::select(info, name, weight_mg, 
                n_area, n_percent, n_factor,
                c_area, c_percent, c_factor, 
                memo) %>% 
  # remove samples with oxygen breakthrough ("bl") - these have been repeated
  #filter(!info %in% "bl") %>% 
  #filter(!memo %in% "skip") %>% 
  filter(!grepl("skip", memo)) %>% 
  force()

## samples
toc_samples = 
  toc_processed %>% 
  filter(!is.na(name),
         !grepl("runin|aspartic|MT|moran", name, ignore.case = TRUE)) %>% 
  dplyr::select(name, c_percent, n_percent, memo) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent,
         randomized_id = name) 

#joining mapping with both runs
toc_mapping <- 
  toc_samples %>% 
  left_join(full_mapping) %>% 
  select(sample_id, randomized_id, cn_notes, parent_id, toc_percent, tn_percent, memo) %>% 
  mutate(tn_percent = case_when(grepl("TC only", cn_notes) ~ NA, TRUE ~ tn_percent),
         toc_percent = case_when(grepl("TN only", cn_notes) ~ NA, TRUE ~ toc_percent)) %>% 
  dplyr::select(sample_id, parent_id, toc_percent, tn_percent) %>% 
  pivot_longer(cols = c(toc_percent, tn_percent),
               names_to = "Parameter",
               values_to = "Percent") %>%
  filter(!is.na(Percent)) %>%
  filter(!is.na(sample_id)) %>% 
  pivot_wider(names_from = Parameter,
              values_from = Percent)

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
  toc_mapping %>% 
  dplyr::select(sample_id, toc_percent) %>% 
  filter(grepl("rep", sample_id)) %>% 
  mutate(sample_id = str_remove(sample_id, "-rep")) %>% 
  rename(rep = toc_percent) %>% 
  left_join(toc_mapping %>% dplyr::select(sample_id, toc_percent)) %>% 
  rowwise() %>% 
  mutate(mean = mean(c(rep, toc_percent)),
         sd = round(sd(c(rep, toc_percent)),3),
         cv = round(sd/mean,3))


reps_n_analytical = 
  toc_mapping %>% 
  dplyr::select(sample_id, tn_percent) %>% 
  filter(grepl("rep", sample_id)) %>% 
  mutate(sample_id = str_remove(sample_id, "-rep")) %>% 
  rename(rep = tn_percent) %>% 
  left_join(toc_mapping %>% dplyr::select(sample_id, tn_percent)) %>% 
  rowwise() %>% 
  mutate(mean = mean(c(rep, tn_percent)),
         sd = round(sd(c(rep, tn_percent)),3),
         cv = round(sd/mean,3)) %>% 
  mutate(parent_id = str_extract(sample_id, "[:digit:]+(?=_)"),
         parent_id = case_when(nchar(parent_id) ==3 ~ parent_id, 
                               nchar(parent_id) ==2 ~ str_c(0, parent_id)),
         parent_id = paste0("EC_", parent_id)) 


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

analytical_test <- toc_mapping %>% 
  mutate(group_id = gsub("-rep$", "", sample_id))

analytical_test_with_reps <- analytical_test %>% 
  group_by(group_id) %>%
  filter(n() > 1) %>%
  ungroup()

analytical_test_with_reps %>% 
  ggplot(aes(x = sample_id, y = tn_percent, group = sample_id, color = sample_id)) +
  geom_point(show.legend = FALSE) +
  geom_text(data = reps_n_analytical, aes(label = paste("cv", round(cv, 2))), color = "black", hjust = 0.5) + 
  labs(x = "Parent ID", y = "TN Percentage", title = "TN Percentage by Parent ID") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = "bold"))

##make a bar graph of the reps with error bars

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
  group_by(parent_id, treat) %>% 
  summarise(mean = mean(toc_percent),
            sd = round(sd(toc_percent),3),
            cv = round(sd/mean,3))

reps_c_by_site %>% 
  ggplot(aes(x = cv))+
  geom_histogram()

reps_n_by_site = 
  toc_mapping  %>% 
  mutate(treat = case_when(grepl("W",sample_id)~"Wet",
                           grepl("D", sample_id) ~"Dry")) %>% 
  group_by(parent_id, treat) %>% 
  summarise(mean = mean(tn_percent),
            sd = round(sd(tn_percent),3),
            cv = round(sd/mean,3))

reps_n_by_site %>% 
  ggplot(aes(x = cv))+
  geom_histogram()




highlighted_parent_ids <- c("EC_042", "EC_069", "EC_052", "EC_089", "EC_094", 
                            "EC_091", "EC_093", "EC_073", "EC_065", "EC_051", 
                            "EC_090", "EC_076", "EC_009", "EC_082", "EC_075", 
                            "EC_071", "EC_095", "EC_080", "EC_084", 
                            "EC_081", "EC_017", "EC_066", "EC_021", "EC_024", 
                            "EC_038", "EC_022", "EC_064", "EC_039", "EC_034", 
                            "EC_035", "EC_041", "EC_013", "EC_032")


tn_distribution <- ggplot(data = toc_mapping, aes(x = tn_percent)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(data = toc_mapping[toc_mapping$parent_id %in% highlighted_parent_ids, ],
             aes(xintercept = tn_percent, color = parent_id), 
             linetype = "dashed", size = 0.8) +
  scale_color_manual(name = "Parent ID",
                     values = c("EC_042" = "#FF0000", "EC_069" = "#0000FF", "EC_052" = "#008000",
                               "EC_089" = "#800080", "EC_094" = "#FFA500", "EC_091" = "#FFC0CB",
                               "EC_093" = "#A52A2A", "EC_073" = "#00FFFF", "EC_065" = "#808080",
                               "EC_051" = "#EE82EE", "EC_090" = "#FFFF00", "EC_076" = "#006400",
                               "EC_009" = "#FA8072", "EC_082" = "#87CEEB", "EC_075" = "#FF00FF",
                               "EC_071" = "#DA70D6", "EC_095" = "#FFD700", "EC_080" = "#FF8C00",
                               "EC_084" = "#00008B", "EC_081" = "#32CD32", "EC_017" = "#008080",
                               "EC_066" = "#40E0D0", "EC_021" = "#808000", "EC_024" = "#A0522D",
                               "EC_038" = "#CD853F", "EC_022" = "#708090", "EC_064" = "#FF6347",
                               "EC_039" = "#DAA520", "EC_034" = "#6495ED", "EC_035" = "#F08080",
                               "EC_041" = "#FFA07A", "EC_013" = "#90EE90", "EC_032" = "#FFB6C1")) +
  labs(x = "TN Percent", y = "Frequency", title = "Distribution of TN Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

library(viridis)

tn <- ggplot(data = toc_mapping, aes(x = tn_percent)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_point(aes(x = tn_percent, y = 0), size = 2)+
  # geom_segment(data = toc_mapping[toc_mapping$parent_id %in% highlighted_parent_ids, ],
  #              aes(x = tn_percent, xend = tn_percent, y = 0, yend = 50, color = parent_id), 
  #              linetype = "dashed", size = 0.8) +
  scale_color_manual(name = "Parent ID", values = viridis(length(highlighted_parent_ids))) +
  labs(x = "TN Percent", y = "Frequency", title = "Distribution of TN Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

