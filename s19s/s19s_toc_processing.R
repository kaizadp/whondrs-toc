## WHONDRS S19S 
## TOC PROCESSING
## 
## Kaizad F. Patel
## October 2022

#################### #
#################### #

library(tidyverse)
library(lubridate)


# PART I: TOC concentrations (EA results) ---------------------------------

## import ----
import_toc_data = function(FILEPATH){
  # this function will identify all "toc" files in the directory and import and combine them
  filePaths_toc <- list.files(path = FILEPATH, pattern = "toc", full.names = TRUE)
  toc_dat <- do.call(bind_rows, lapply(filePaths_toc, function(path) {
    df <- read.csv(path, header=TRUE, check.names = F)
    
    df}))
  
}
toc_data = import_toc_data(FILEPATH = "data/toc") %>% janitor::clean_names()


## process toc data ----
toc_processed = 
  toc_data %>% 
  mutate(date_time = lubridate::mdy_hm(date_time),
         date_started = case_when(name == "runin" ~ lubridate::as_date(date_time))) %>% 
  fill(date_started) %>% 
  group_by(date_started) %>% 
  mutate(run_number = cur_group_id()) %>% 
  ungroup() %>% 
  dplyr::select(info, name, weight_mg, 
                n_area, n_percent, n_factor, 
                c_area, c_percent, c_factor, 
                memo, date_started, run_number) %>% 
  # remove samples with oxygen breakthrough ("bl") - these have been repeated
  filter(!info %in% "bl") %>% 
  # this initial dataset was run in December, and we do not want to include the more recent results;
  # those will be run under toc_redo, see below.
  filter(date_started < as.Date("2022-12-01")) %>% 
  force()

## samples
toc_samples = 
  toc_processed %>% 
  mutate(name = as.numeric(name),
         name = as.character(name)) %>% 
  filter(!is.na(name)) %>% 
  dplyr::select(name, c_percent, n_percent, date_started) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent) %>% 
  # some samples were re-run, so we want only the most recent ones
  group_by(name) %>% 
  dplyr::mutate(keep = date_started == max(date_started)) %>% 
  filter(keep) %>% 
  dplyr::select(-keep) %>% 
  arrange(as.numeric(name))


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

## KFP note: blanks looked good - low C counts/areas, C ~ 0%

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

## KFP note: blanks looked good. the first N-blank is always high, especially if tubes have been changed.
## so those have been excluded from the graphs

### 2. reps
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

## KFP note: reps are not as good as hoped. 
## < 15 % CV is good, we had some samples with 20-50 % variation among reps.


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

## KFP note: c_factor and n_factor should be close to 1. Most of the runs were 0.80-1, which is good.


### checking linearity of standards
aspartics = 
  aspartics %>% 
  filter(!run_number %in% c(13, 3)) %>% 
  mutate(c_mg = weight_mg * 36.09/100,
         n_mg = weight_mg * 10.52/100)

aspartics %>% 
  filter(!info %in% "bl") %>% 
  ggplot(aes(x = c_mg, y = c_area))+
  geom_point(size = 4, shape = 1, stroke = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  #  facet_wrap(~run_number)+
  NULL

aspartics %>% 
  filter(!info %in% "bl") %>% 
  ggplot(aes(x = n_mg, y = n_area))+
  geom_point(size = 4, shape = 1, stroke = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  #  facet_wrap(~run_number)+
  NULL


### redoing calibration curve for C (not used for the calculations)
### setting it in a function so it does not disrupt the rest of the script
redo_calibration_curve = function(){
  # get slope and intercept for c_area ~ c_mg
  # y = mx + c
  cal_lm = lm(c_area ~ c_mg, data = aspartics)
  slope = cal_lm$coefficients["c_mg"]
  intercept = cal_lm$coefficients["(Intercept)"]
  
  # apply slope and intercept to calculate c_mg from c_area
  # x = (y-c)/m
  toc_processed = 
    toc_processed %>% 
    mutate(c_mg_recalculated = (c_area - intercept)/slope,
           c_percent_recalculated = (c_mg_recalculated/weight_mg)*100,
           c_percent_recalculated = round(c_percent_recalculated, 2))
  
  # compare instrument-calculated TOC with re-calculated TOC
  # they are comparable, so not recalculating TOC
  toc_processed %>% 
    mutate(name = as.numeric(name)) %>% 
    filter(!is.na(name)) %>% 
    ggplot(aes(x = c_percent, y = c_percent_recalculated))+
    geom_point()+
    geom_smooth(method = "lm")
  
  
  # repeat for TN
  cal_lm = lm(n_area ~ n_mg, data = aspartics)
  slope = cal_lm$coefficients["n_mg"]
  intercept = cal_lm$coefficients["(Intercept)"]
  
  # apply slope and intercept to calculate n_mg from n_area
  toc_processed = 
    toc_processed %>% 
    mutate(n_mg_recalculated = (n_area - intercept)/slope,
           n_percent_recalculated = (n_mg_recalculated/weight_mg)*100,
           n_percent_recalculated = round(n_percent_recalculated, 2))
  
  # compare instrument-calculated TOC with re-calculated TN
  # they are comparable, so not recalculating TN
  toc_processed %>% 
    mutate(name = as.numeric(name)) %>% 
    filter(!is.na(name)) %>% 
    ggplot(aes(x = n_percent, y = n_percent_recalculated))+
    geom_point()+
    geom_smooth(method = "lm")
}
## TOC-redo ----
## Some samples were re-run in December 2022, in triplicate.
## These are processed below.
## KFP, 2023-01-04

toc_redo = read.csv("data/toc/S19S_toc_redo.csv") %>% janitor::clean_names()

toc_redo_processed = 
  toc_redo %>% 
  rename(n_percent = n,
         c_percent = c) %>% 
  mutate(date_time = lubridate::mdy_hm(date_time),
         date_started = case_when(name == "runin" ~ lubridate::as_date(date_time))) %>% 
  fill(date_started) %>% 
  group_by(date_started) %>% 
  mutate(run_number = cur_group_id()) %>% 
  ungroup() %>% 
  dplyr::select(info, name, weight_mg, 
                n_area, n_percent, n_factor, 
                c_area, c_percent, c_factor, 
                memo, date_started, run_number) %>% 
  # remove samples with oxygen breakthrough ("bl") - these have been repeated
  filter(!info %in% "bl") %>% 
  force()

## samples
toc_redo_samples = 
  toc_redo_processed %>% 
  mutate(name = as.numeric(name),
         name = as.character(name)) %>% 
  filter(!is.na(name)) %>% 
  dplyr::select(name, c_percent, n_percent, date_started) %>% 
  rename(toc_percent = c_percent,
         tn_percent = n_percent) %>% 
  # some samples were re-run, so we want only the most recent ones
  group_by(name) %>% 
  dplyr::mutate(keep = date_started == max(date_started)) %>% 
  filter(keep) %>% 
  dplyr::select(-keep) %>% 
  arrange(as.numeric(name))

toc_redo_samples_qaqc = 
  toc_redo_samples %>% 
  group_by(name) %>% 
  dplyr::summarise(toc_cv = 100 * sd(toc_percent)/mean(toc_percent),
                   tn_cv = 100 * sd(tn_percent)/mean(tn_percent))

toc_redo_samples_mean = 
  toc_redo_samples %>% 
  group_by(name, date_started) %>% 
  dplyr::summarise(toc_percent = round(mean(toc_percent), 2),
                   tn_percent = round(mean(tn_percent),2))


## Now, combine the original and the redo samples

toc_all_samples = 
  toc_samples %>% 
  rbind(toc_redo_samples_mean) %>% 
  group_by(name) %>% 
  # keep only the most recent sample
  dplyr::mutate(keep = date_started == max(date_started)) %>% 
  filter(keep) %>% 
  dplyr::select(-keep) %>% 
  rename(date_run = date_started) %>% 
  arrange(as.numeric(name))


#
## export TOC and TN data ----
toc_all_samples %>% 
  arrange(as.numeric(name)) %>% 
  write.csv("data/processed/toc_tn_processed_2022-01-04.csv", row.names = FALSE)


#
#


# PART II: IRMS results ---------------------------------------------------

## import ----
import_irms_data = function(FILEPATH){
  filePaths_irms <- list.files(path = FILEPATH, pattern = c("irms", "csv"), full.names = TRUE)
  
  # the headers are split across 2 rows, so we need to first combine them,
  # and then apply them to the dataframe
  headers <- do.call(bind_rows, lapply(filePaths_irms, function(path) {
    df <- read.csv(path, skip = 3, nrows = 2, header=FALSE, check.names = F)
    df})) %>% head(2)
  headers_names <- sapply(headers,paste,collapse="_")
  
  data <- do.call(bind_rows, lapply(filePaths_irms, function(path) {
    df <- read.csv(path, skip = 5, header=FALSE, check.names = F)
    names(df) <- headers_names  
    df %>% mutate(source = path)
  }))
  data
}
irms_data = import_irms_data(FILEPATH = "data/toc")

#
## process irms data ----
irms_data_processed = 
  irms_data %>% 
  janitor::clean_names() %>% 
  mutate(date = str_extract(source, "2022-[0-9]{2}-[0-9]{2}"),
         date = lubridate::ymd(date)) %>% 
  dplyr::select(id, name, sample_group, d15n_air, d13c_vpdb, date) %>% 
  mutate_at(vars(starts_with(c("d13c", "d15n"))), as.numeric)

irms_samples_all = 
  irms_data_processed %>% 
  filter(grepl("sample", sample_group)) %>% 
  mutate(d15n_air = case_when(sample_group == "samples-C" ~ NA_real_,
                              TRUE ~ d15n_air),
         d13c_vpdb = case_when(sample_group == "samples-N" ~ NA_real_,
                              TRUE ~ d13c_vpdb))

irms_samples_filtered = 
  irms_samples_all %>% 
  group_by(name) %>% 
  mutate(keep = id == max(id)) %>% 
  ungroup() %>% 
  filter(keep) %>% 
  dplyr::select(-keep) %>% 
  mutate(name = as.character(as.numeric(name))) %>% 
  filter(!is.na(name)) %>% 
  dplyr::select(-sample_group, -id)


## QA-QC ----

### replicates
# because the δ values are positive as well as negative, the CV calculation does not make sense
# therefore we calculate just the mean and SD here, not the CV

reps_irms = 
  irms_samples_all %>% 
  dplyr::select(name, date, d13c_vpdb, d15n_air) %>% 
  filter(grepl("rep", name)) %>% 
  mutate(name = str_remove(name, "-rep")) %>% 
  rename(rep_d13c = d13c_vpdb,
         rep_d15n = d15n_air) %>% 
  left_join(irms_samples_filtered %>% dplyr::select(name, date, d13c_vpdb, d15n_air)) %>% 
  dplyr::select(name, contains("d13c"), contains("d15n"), everything()) %>% 
  rowwise() %>% 
  mutate(mean_d13c = mean(c(rep_d13c, d13c_vpdb)),
         sd_d13c = round(sd(c(rep_d13c, d13c_vpdb)),3),
         #cv_d13c = round(sd_d13c/mean_d13c,3),
         
         mean_d15n = mean(c(rep_d15n, d15n_air)),
         sd_d15n = round(sd(c(rep_d15n, d15n_air)),3),
         #cv_d15n = round(sd_d15n/mean_d15n,3)
         )

# export ----
irms_samples_filtered %>% 
  arrange(as.numeric(name)) %>% 
  write.csv("data/processed/irms_processed_2023-02-17.csv", row.names = FALSE)

