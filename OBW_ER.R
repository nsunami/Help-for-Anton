# This code converts preprocessed xlsx file to a wide format 
# install.packages("tidyverse")
library(tidyverse)
library(readxl)


filenames <- list.files("./raw", full.names = TRUE)

# initialize the container
all_df <- tibble()

anton_df <- read_csv(filenames[1])

# # Checking
# anton_df %>% select(`Task Type`, Navon_corr, OBW_overestimation_percent,
#                     orientation_response, size_error,
#                     `Reaction Time`) %>% View()

for(i in filenames){
  print(i)
  anton_df <- read_csv(i)
  anton_df <- anton_df %>% 
    # Task Types are:
    # 1. Navon
    # 2. OBW
    # 3. Orientation
    # 4. Size
    mutate("Task Type" = case_when(!is.na(Navon_corr) ~ "Navon",
                                   !is.na(OBW_overestimation_percent) ~ "OBW",
                                   !is.na(orientation_response) ~ "Orientation",
                                   !is.na(size_error) ~ "Size")) %>% 
    # Dependent Variables
    # RT only relevant only for Navon
    mutate("Reaction Time" = case_when(`Task Type` == "Navon" ~ Navon_rt,
                                       `Task Type` == "OBW" ~ OBWtrialKey.rt)) %>%
    # Accuracy 
    mutate("Accuracy" = case_when(`Task Type` == "Navon" ~ Navon_corr)) %>%
    # Overestimation - values are percent
    mutate("Overestimation Percent" = case_when(`Task Type` == "OBW" ~ OBW_overestimation_percent,
                                        `Task Type` == "Size" ~ size_error_percent)) %>%
    # Orientation Task - values are in angles (0-360)
    mutate("Error Degrees" = case_when(`Task Type` == "Orientation" ~ orientation_error)) %>% 

    
    
    ### Independent Variables
    mutate("Navon - Consistency" = case_when(`Task Type` == "Navon" ~ congruence),
           "Navon - Size" = case_when(`Task Type` == "Navon" ~ report),
           "Navon - Condition" = case_when(`Task Type` == "Navon" ~ paste0(`Navon - Consistency`, "-", `Navon - Size`)),
           "OBW Condition" = case_when(`Task Type` == "OBW" ~ subcond)) %>% 
    
    # Size conditions - the row after the last indicates the size condition for Size Task
    fill(size_cond, .direction = "up") %>%
    mutate("Size Condition" = case_when(`Task Type` == "Size" ~ size_cond)) %>%
    
    select(`Task Type`, `Reaction Time`, `Accuracy`, `Overestimation Percent`, `Error Degrees`,
           `Navon - Consistency`, `Navon - Size`, `Navon - Condition`, `OBW Condition`, `Size Condition`,
           everything())

# Outlier Detection (flag)
  anton_df_flagged <- anton_df %>%
    group_by(`Task Type`) %>%
    # Reaction
    mutate("Mean within Condition" = mean(`Reaction Time`, na.rm = TRUE), "SD within Condition" = sd(`Reaction Time`, na.rm = TRUE),
           "Reaction Time Outlier" = case_when(`Reaction Time` >= `Mean within Condition` + 3 * `SD within Condition` ~ TRUE,
                                 `Reaction Time` <= `Mean within Condition` - 3 * `SD within Condition` ~ TRUE,
                                 TRUE ~ FALSE)) %>%
    # Accuracy
    mutate("Accuracy Mean" = mean(Accuracy, na.rm = TRUE), "Accuracy SD" = sd(Accuracy, na.rm = TRUE),
           "Accuracy Outlier" = case_when(Accuracy >= `Accuracy Mean` + 3 * `Accuracy SD` ~ TRUE,
                                          Accuracy <= `Accuracy Mean` - 3 * `Accuracy SD` ~ TRUE,
                                          TRUE ~ FALSE)) %>% 
    # Overestimation
    mutate("Overestimation Percent Mean" = mean(`Overestimation Percent`, na.rm = TRUE),
           "Overestimation Percent SD" = sd(`Overestimation Percent`, na.rm = TRUE),
           "Overestimation Outlier" = case_when(`Overestimation Percent` >= `Overestimation Percent Mean` + 3 * `Overestimation Percent SD` ~ TRUE,
                                          `Overestimation Percent` <= `Overestimation Percent Mean` - 3 * `Overestimation Percent SD` ~ TRUE,
                                          TRUE ~ FALSE)) %>%
    # Error Degrees
    mutate("Error Degrees Mean" = mean(`Error Degrees`, na.rm = TRUE),
           "Error Degrees SD" = sd(`Error Degrees`, na.rm = TRUE),
           "Error Degrees Outlier" = case_when(`Error Degrees` >= `Error Degrees Mean` + 3 * `Error Degrees SD` ~ TRUE,
                                               `Error Degrees` <= `Error Degrees Mean` - 3 * `Error Degrees SD` ~ TRUE,
                                               TRUE ~ FALSE)) %>%
    select(`Mean within Condition`, `SD within Condition`, `Reaction Time Outlier`, everything())

# 
anton_df_flagged_summary <- anton_df_flagged %>%
  mutate("Delete for Outlier" = case_when(
    `Task Type` == "Navon" ~ `Reaction Time Outlier`,
    `Task Type` == "OBW" ~ `Accuracy Outlier`,
    `Task Type` == "OBW" ~ `Reaction Time Outlier`,
    `Task Type` == "Orientation" ~ `Overestimation Outlier`,
    `Task Type` == "Size" ~ `Error Degrees Outlier`,
    TRUE ~ FALSE
  )) %>%
  filter(!`Delete for Outlier`) %>%
  # Group by to get summary statistics within levels
  group_by(`Task Type`, `Navon - Consistency`, `Navon - Size`, `Navon - Condition`, `OBW Condition`, `Size Condition`) %>%
  summarize("Mean RT" = mean(`Reaction Time`, na.rm = TRUE),
            "Number of Responses" = n(),
            "Mean Accuracy" = mean(Accuracy, na.rm = TRUE),
            "Mean Overestimation" = mean(`Overestimation Percent`, na.rm = TRUE),
            "Mean Error Degrees" = mean(`Error Degrees`, na.rm = TRUE)) %>% 
  mutate(key = case_when(`Task Type` == "Navon" ~ paste0(`Task Type`, " - ", `Navon - Condition`),
                         `Task Type` == "OBW" ~ paste0(`Task Type`, " - ", `OBW Condition`),
                         `Task Type` == "Orientation" ~ "Orientation",
                         `Task Type` == "Size" ~ `Size Condition`))

my_longest_table <- anton_df_flagged_summary %>%
  ungroup() %>%
  select(`Mean RT`, `Mean Accuracy`, `Mean Overestimation`, `Mean Error Degrees`, key)


#### TO WIDE !!!!!
wide_output <- my_longest_table %>% filter(!is.na(key)) %>% pivot_longer(-key) %>% 
  mutate(myname = paste0(key, " - ", name)) %>%
  select(-key, -name) %>%
  pivot_wider(names_from = myname, values_from = value) 

# colnames(wide_output)

# wide_output <- wide_output %>%
#   select(!any(is.nan(.))
#   ) %>%
#   mutate("Filename" = i) %>% 
#   select(Filename, everything())


#write_csv(wide_output, paste0(i, "- wide.csv"))

all_df <- all_df %>%
  bind_rows(wide_output)
}

# Clear columns that have only NA's
# https://stackoverflow.com/a/45383054/6205282
all_df <- all_df %>% select_if(~sum(!is.na(.)) > 0)

write_csv(all_df, "Everything.csv")
