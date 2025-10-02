# Reproducible Research Fundamentals 
# 01. Data processing

### Libraries
# library(haven)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(labelled)

### Loading data ----

# Load the dataset
data_path <- "C:/Users/Ariyasuren.Baldansen/Desktop/wb-training-reproducibility/day-1/Transparent and Credible Analytics/Course Materials/DataWork/Data"
#data      <- read_dta(file.path(data_path, "Raw/TZA_CCT_baseline.dta"))

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
#setting working directory
#setwd("C:/Users/Ariyasuren.Baldansen/Desktop/wb-training-reproducibility/day-1/Transparent and Credible Analytics/Course Materials/DataWork/Data")


#uploading data primary 
tza_cct_baseline <- read_dta(file.path(data_path, "raw/TZA_CCT_baseline.dta")) 
treat_status <- read_dta(file.path(data_path, "raw/treat_status.dta")) 

#uploading data secondary
tza_amenity <- read.csv(file.path(data_path, "raw/TZA_amenity.csv")) 




#Exploring baseline data - checking the duplicates
length(unique(tza_cct_baseline$hhid))


#Cleaning the data
data_clean <- tza_cct_baseline %>% distinct(hhid, .keep_all=TRUE)

#Tidying the data
data_tidy_hh <- data_clean %>%
    select(vid,
           hhid,
           enid,
           floor:n_elder,
           food_cons:last_col())


data_tidy_hh_member <- data_clean %>%
    select(vid, hhid, enid,
           starts_with("gender"),
           starts_with("age"),
           starts_with("clinic_visit"),
           starts_with("sick"),
           starts_with("days_sick"),
           starts_with("treat_fin"),
           starts_with("treat_cost"),
           starts_with("read"),
           starts_with("ill_impact"),
           starts_with("days_impact")) %>%
    pivot_longer(cols=-c(vid, hhid, enid),
                 names_to = c(".value", "member"),
                 names_pattern = "(.*)_(\\d+)")


data_tidy_hh_member1 <- data_clean %>%
    select(vid, hhid, enid, matches("_([12])$"))%>%
    pivot_longer(cols = -c(vid, hhid, enid), 
                 names_to = c(".value", "member"),
                 names_pattern = "(.*)_(\\d+)")


data_clean_hh <- data_tidy_hh %>%
    mutate(submissiondate = as.Date(submissionday, format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(duration = as.numeric(duration)) %>%
    mutate(ar_unit = as.factor(ar_farm_unit)) %>%
    mutate(crop_other = str_to_title(crop_other)) %>%
    mutate(crop = case_when(
        str_detect(crop_other, "Coconut") ~ 40,
        str_detect(crop_other, "Sesame") ~ 41,
        TRUE ~ crop)) %>%
    mutate(across(where(is.numeric), ~replace(., . == -88, NA))) %>%
    set_variable_labels(
        duration = "Duration of the interview (minutes)",
        submissiondate = "Submission date",
        ar_unit = "Farm area unit?")


data_clean_hh_member <- data_tidy_hh_member %>%
    filter(!is.na(gender)) %>%
    set_variable_labels(
        member = "HH member ID",
        age = "Age",
        clinic_visit = "In the past 12 months, how many times has the member attended the clinic?",
        treat_cost = "How much did the treatment cost?",
        days_impact = "No. of days member was unable to perform daily activities due to illness"
    )


#Processing secondary data
secondary_data <- tza_amenity %>%
    pivot_wider(names_from = amenity,
                values_from = n,
                names_prefix = "n_")


#saving the result
write_dta(data_clean_hh, file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))
write_dta(data_clean_hh_member, file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))
write_dta(secondary_data, file.path(data_path, "Intermediate/secondary.dta"))



