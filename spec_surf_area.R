#### Calculating specific surface area from WP4C Template Script

#### Sophia McKever, Kaizad Patel & Kenton Rod
#### Month Year

# S = specific surface area (m^2/kg)
# p = density of water (1000 kg/m^2)
# k = hamaker constant (-6 * 10^-20 J)
# y = water potential (J/kg) 
  # NOTE - water potential measured as MPa on WP4C
  # 1 J/kg = 1 kPa at p = 1000 kg/m^2
# = water content (g/g)

#1. Set working directories and bring in data sheet
#2. Set the constants within the equation
#3. Using data and constants, calculate ssa for each sample
#4. Run outlier test for the replicates to determine if any samples need to have additional samples ran.
#5. Calculate CV for SSA, discuss with team to determine acceptable CV threshold. 

library(tidyverse)
library(readxl)

#1. Set directories and bring in data
pnnl.user = 'guil098'

input.path = paste0("C:/Users/",pnnl.user,"/OneDrive - PNNL/Data Generation and Files/ICON_Modex_SSS/16_SSA/01_RawData/2023_Data_Raw_SSA_RC4_CM.xlsx")

ssa <- read_excel(input.path) %>% 
  mutate(Parent_ID = str_extract(Sample_ID, ".{5}(?=_)")) %>% 
  filter(!grepl("skip",Method_Notes)) #filtered out samples that had notated issues during run


#2. setting constants
p = 1000 #density of water (1000 kg/m^3)
k = as.numeric(-6*(10^-20)) #hamaker constant (-6 * 10^-20 J)
g = 9.8 #gravity 9.8 m/s^2

#3. Calculate SSA
ssa_calc <- ssa %>% 
  mutate(gwc = (tray_soil_wt_g - tray_soil_od_wt_g)/(tray_soil_od_wt_g - tare_wt_g), #gwc = gravimetric water content
         Water_Potential_m = (Water_Potential_MPa*102.1788), #converting the given water potential from the instrument into water potential in m 
         ssa_m2_kg = ((gwc) / ((k/(6 * pi* p * Water_Potential_m * g))^(1/3))/p), #equation of ssa in m2/kg
         ssa_m2_g = ssa_m2_kg/1000, #converting ssa to m2/g
         ssa_m2_g = round(ssa_m2_g, 3)) %>% #rounding final number to the thousandths
  filter(ssa_m2_g > 0) #eliminate any values below 0 

#4. Outlier test to determine if samples need to have more samples ran, this helps determine if there were replicates that were most different from each other
#outlier test using Dixon's Q

library(outliers)

ssa_cv =
  ssa_calc %>% 
  group_by(Parent_ID) %>% 
  dplyr::mutate(
    mean = mean(ssa_m2_g, na.rm = T),
    sd = round(sd(ssa_m2_g, na.rm = T),3),
    cv = round(sd/mean,3),
    n = n()) %>% 
  dplyr::select(Sample_ID, Parent_ID, ssa_m2_g, cv, n)


remove_outliers = function(dat){
  
  fit_dixon = function(dat){
    dixon.test(dat %>% pull(ssa_m2_g), opposite = F, two.sided = F) %>% 
      broom::tidy() %>% 
      filter(`p.value` <= 0.1) %>% 
      mutate(ssa_m2_g = parse_number(alternative),
             ssa_m2_g = round(ssa_m2_g, 3),
             outlier = TRUE) %>% 
      dplyr::select(ssa_m2_g, outlier)
  }

 outliers =  
   dat %>% 
   group_by(Parent_ID) %>% 
   filter(n > 2) %>% 
   filter(cv >= 0.3) %>% 
   do(fit_dixon(.)) %>% 
   ungroup()

 ssa_outliers = 
   dat %>% 
   ungroup() %>% 
   left_join(outliers, by = c("Parent_ID", "ssa_m2_g"))
 
 #ssa_outliers_removed = 
   ssa_outliers %>% 
   filter(!outlier %in% "TRUE")
 
}

ssa_outliers_removed = 
  remove_outliers(ssa_cv) %>% 
  rename(cv_old = cv) %>% 
  group_by(Parent_ID) %>% 
  dplyr::mutate(
    mean = mean(ssa_m2_g, na.rm = T),
    sd = round(sd(ssa_m2_g, na.rm = T),3),
    cv = round(sd/mean,3),
    n = n()) %>% 
  dplyr::select(Sample_ID, Parent_ID, ssa_m2_g, cv_old, cv, n)

ssa_outliers_removed2 = 
  remove_outliers(ssa_outliers_removed)


ssa_outliers_removed2 %>% 
  filter(cv >= 0.3) %>% 
  ggplot(aes(x = Parent_ID, y = ssa_m2_g, color = cv))+
  geom_point()+
  scale_color_viridis_c()+
  theme(axis.text.x = element_text(angle = 90))


##


#5. Calculating Cv 
ssa_m2_g_by_site =
  ssa_calc %>% 
  group_by(Parent_ID) %>% 
  summarise(mean = mean(ssa_m2_g, na.rm = T),
            sd = round(sd(ssa_m2_g, na.rm = T),3),
            cv = round(sd/mean,3),
            range = max(ssa_m2_g, na.rm = T)-min(ssa_m2_g, na.rm = T))

ssa_hist <- ggplot(data = ssa_m2_g_by_site, aes(x = mean, y = cv, color = Parent_ID)) +
  geom_jitter()

#importing grain size
##if grain size is available, this is useful to help with QAQC 
grain_size <- read.csv("data/v2_CM_SSS_Sediment_Grain_Size.csv", skip = 2, na=c("-9999", "N/A")) %>% 
  select(Sample_Name, Percent_Fine_Sand, Percent_Med_Sand, Percent_Coarse_Sand, Percent_Tot_Sand, Percent_Clay, Percent_Silt) %>% 
  mutate(Percent_Fine_Sand=as.numeric(Percent_Fine_Sand),
         Percent_Med_Sand=as.numeric(Percent_Med_Sand),
         Percent_Coarse_Sand=as.numeric(Percent_Coarse_Sand),
         Percent_Tot_Sand=as.numeric(Percent_Tot_Sand),
         Percent_Clay=as.numeric(Percent_Clay),
         Percent_Silt=as.numeric(Percent_Silt),
         Sample_Name=str_remove(Sample_Name, "_GRN"),
         Parent_ID = str_extract(Sample_Name, "\\d{2}$")) %>% 
  filter(!is.na(Sample_Name),
         !grepl("SSS", Sample_Name))
  
grain_size$Parent_ID <- paste0("EC_", grain_size$Parent_ID)

#joining grain size with ssa
gs_ssa <- grain_size %>% 
  left_join(ssa_m2_g_by_site, by = c('Parent_ID')) %>% 
  filter(!is.na(cv))

gs_ssa2 <- grain_size %>% 
  left_join(ssa_calc, by = c('Parent_ID')) %>% 
  filter(!is.na(ssa_m2_g))


#Exploratory plots that can be used to further help QAQC


ssa_plot <- ggplot(data = ssa_calc, aes(x = Parent_ID, y = ssa_m2_g)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Specific Surface Area (m2/g)")

ggsave(
  "data/01-ssa_plot.pdf",
  ssa_plot,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)

ssa_plot2 <- ggplot(data = ssa_calc %>% 
                      filter(Parent_ID != "EC_81"), aes(x = Parent_ID, y = ssa_m2_g)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Specific Surface Area (m2/g)")

ggsave(
  "data/04-ssa_plot2.pdf",
  ssa_plot2,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)


#histogram to show cv distribution
ssa_cv <- ggplot(data = ssa_m2_g_by_site, aes(x = cv)) +
  geom_histogram(binwidth = 0.05, aes(fill = factor(Parent_ID)), color = "black", alpha = 0.7)


ssa_cv <- ggplot(data = ssa_m2_g_by_site, aes(x = Parent_ID, y = cv)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Coefficient of Variation")

ggsave(
  "data/02-ssa_cv.pdf",
  ssa_cv,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)

ssa_cv2 <- ggplot(data = ssa_m2_g_by_site %>% 
                    filter(Parent_ID != "EC_21"), aes(x = Parent_ID, y = cv)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Coefficient of Variation")

ggsave(
  "data/05-ssa_cv2.pdf",
  ssa_cv2,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)

ssa_range <- ggplot(data = ssa_m2_g_by_site, aes(x = Parent_ID, y = range)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Range")

ggsave(
  "data/03-ssa_range.pdf",
  ssa_range,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)


ssa_range2 <- ggplot(data = ssa_m2_g_by_site %>% 
                       filter(Parent_ID != "EC_81"), aes(x = Parent_ID, y = range)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Range")

ggsave(
  "data/06-ssa_range2.pdf",
  ssa_range2,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)

ssa_range_cv <- ggplot(data = ssa_m2_g_by_site, aes(x = range, y = cv)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Coefficient of Variation")

ggsave(
  "data/07-ssa_range_cv.pdf",
  ssa_range_cv,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)

ssa_range_cv2 <- ggplot(data = ssa_m2_g_by_site %>% 
                          filter(!(Parent_ID %in% c("EC_21", "EC_81"))), aes(x = range, y = cv)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Coefficient of Variation")

ggsave(
  "data/08-ssa_range_cv2.pdf",
  ssa_range_cv2,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)

ssa_range_cv3 <- ggplot(data = ssa_m2_g_by_site %>% 
                          filter(!(Parent_ID %in% c("EC_21", "EC_81"))) %>% 
                          filter(cv < 1), aes(x = range, y = cv)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Coefficient of Variation")

ggsave(
  "data/09-ssa_range_cv3.pdf",
  ssa_range_cv3,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)

ssa_mean_cv <- ggplot(data = ssa_m2_g_by_site, aes(x = mean, y = cv)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Coefficient of Variation")

ggsave(
  "data/10-ssa_mean_cv.pdf",
  ssa_mean_cv,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)

ssa_mean_cv2 <- ggplot(data = ssa_m2_g_by_site %>% 
                         filter(mean > 0) %>% 
                         filter(Parent_ID != "EC_81"), aes(x = mean, y = cv, color = range)) +
  geom_point(size = 2)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_gradient(low = "blue", high = "red")+
  ylab("Coefficient of Variation")

ggsave(
  "data/11-ssa_mean_cv2.pdf",
  ssa_mean_cv2,
  device = 'pdf',
  width = 15,
  height = 7,
  units = 'in',
  dpi = 300
)


#grain size and cv plots
silt_cv_plot <- ggplot(data = gs_ssa, aes(x = cv, y = Percent_Silt, color = Parent_ID)) +
  geom_jitter()

clay_cv_plot <- ggplot(data = gs_ssa, aes(x = cv, y = Percent_Clay, color = Parent_ID)) +
  geom_jitter()
  
sand_cv_plot <- ggplot(data = gs_ssa, aes(x = cv, y = Percent_Tot_Sand, color = Parent_ID)) +
  geom_jitter() 
  


### Data Package Preparation ----

ssa2 <- read_excel(input.path) %>% 
  mutate(Parent_ID = str_extract(Sample_ID, ".{5}(?=_)")) 

ssa_dp <-  
  ssa_outliers_removed2 %>%
  full_join(ssa2, by = "Sample_ID") %>% 
  select(Study_Code, Sample_ID, ssa_m2_g) %>% 
  mutate(Material = "Sediment",
         Study_Code = str_replace_all(Study_Code, "EC", "CM"),
         Sample_ID = str_replace_all(Sample_ID, "EC", "CM")) %>% 
  relocate(Material, .after = Sample_ID) %>% 
  rename(Sample_Name = Sample_ID) 


ssa_dp %>% 
  write.csv("C:/Users/guil098/OneDrive - PNNL/Data Generation and Files/ECA/SSA/02_FormattedData/CM_SSA_ReadyForBoye_11-19-2023.csv", row.names = F)  

  
  
















