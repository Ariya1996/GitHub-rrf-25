# Reproducible Research Fundamentals 
# 02. Data construction

library(tidyverse)
library(labelled)
library(haven)
# RRF - 2024 - Construction

#### Read data ----

data_path <- "C:/Users/Ariyasuren.Baldansen/Desktop/wb-training-reproducibility/day-1/Transparent and Credible Analytics/Course Materials/DataWork/Data"
#data      <- read_dta(file.path(data_path, "Raw/TZA_CCT_baseline.dta"))

# Preliminary - Load data
# Load HH data
hh_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))
# Load HH-member data
mem_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))
# Load secondary data
secondary_data <- read_dta(file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))

# Exercise 1: Plan construction outputs ----
# 1 Area in acres - hh
# 2 Household consumption (food and nonfood) in USD -hh
# 3 Any HH member sick - member
# 4 Any HH member can read or write 
# 5 Average sick days
# 6 Total treatment cost in USD
# 7 Total medical facilities - secondary amenities

# Exercise 2: Standardize conversion values ----
acre_conv <- ....
usd <- ....

# Data construction: HH 

# Area in acres (Convert units for farming area)
hh_data <- hh_data %>%
    mutate(...... = case_when(
      ...... == 1 ~ ......,               # If unit is acres
      ...... == 2 ~ ...... * ......     # Convert hectare to acres
    )) %>%
    mutate(...... = replace_na(......, 0)) %>% 
    set_variable_labels(...... = "Area farmed in acres")

# Consumption in USD (for food and nonfood)
hh_data <- hh_data %>%
    mutate(across(c(......, ......), 
                  ~ .x * usd, 
                  .names = "{.col}......"))

# Exercise 3: Handle outliers ----

# customized function:
winsor_function <- function(dataset, var, min = 0.00, max = 0.95){
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Winsorize selected variables in the dataset
win_vars <- c("......", "......", "......")

# Apply the custom winsor_function to each variable in win_vars
for (var in win_vars) {
    hh_data <- winsor_function(hh_data, var)
}

# Update the labels to reflect that winsorization was applied
hh_data <- hh_data %>%
    mutate(across(ends_with("......"), 
                  ~ labelled(.x, label = paste0(attr(.x, "label"), 
                                                " (Winsorized 0.05)"))))

# Exercise 4.1: Create indicators at HH level ----

# Collapse HH-member data to HH level
hh_mem_collapsed <- mem_data %>%
    group_by(......) %>%
    summarise(
      ...... = max(......, na.rm = TRUE),  # Any member was sick
      ...... = max(......, na.rm = TRUE),  # Any member can read/write
        # If all values of days_sick are NA, return NA; otherwise, calculate mean
      ...... = if_else(all(is.na(......)), NA_real_, mean(......, na.rm = TRUE)),
        # If all values of treat_cost are NA, return NA; otherwise, calculate sum in USD
      ...... = if_else(all(is.na(......)), NA_real_, sum(......, na.rm = TRUE) * usd)
    ) %>%
    ungroup() %>%
    # Replace missing treat_cost_usd with the average of non-missing values
    mutate(...... = if_else(is.na(......), 
                            mean(......, na.rm = TRUE), 
                            ......)) %>%
    # Apply labels to the variables
    set_variable_labels(
      ...... = "Any member can read/write",
      ...... = "Any member was sick in the last 4 weeks",
      ...... = "Average sick days",
      ...... = "Total cost of treatment (USD)"
    )

# Exercise 4.2: Data construction: Secondary data ----

# Calculate the total number of medical facilities
secondary_data <- secondary_data %>%
    mutate(...... = rowSums(select(., ......, ......), 
                               na.rm = TRUE)) %>% 
    rename(...... = ......)

# Apply label to the new column
var_label(secondary_data$......) <- "No. of medical facilities"

# Exercise 5: Merge HH and HH-member data ----

# Merge HH and HH-member datasets
final_hh_data <- hh_data %>%
    left_join(hh_mem_collapsed, by = "......")

# Load treatment status and merge
treat_status <- read_dta(file.path(data_path, "Raw/treat_status.dta"))

final_hh_data <- final_hh_data %>%
    left_join(treat_status, by = "......") 

# Exercise 6: Save final dataset ----

# Save the final merged data for analysis
write_dta(final_hh_data, file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# Save the final secondary data for analysis
write_dta(secondary_data, file.path(data_path, "Final/TZA_amenity_analysis.dta"))
