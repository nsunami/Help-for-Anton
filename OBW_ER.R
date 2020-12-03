# The code to convert multiple .csv files from psychopy 
# to a wide format (one row par articipant)

library(tidyverse)
library(readxl)
library(here)

filenames <- list.files(here("raw"), full.names = TRUE)

# initialize the container for all data frames
all_df <- tibble()

all_split_df <- tibble()

# Loop through each file 
for(i in filenames){
  print(paste("Processing", i))
  participant_df <- read_csv(i, col_types = list())
  participant_id <- participant_df$participant[1]
  participant_df <- participant_df %>% 
    # Task Types are:
    # 1. Navon
    # 2. OBW
    # 3. Orientation
    # 4. Size
    mutate("Task Type" = case_when(!is.na(Navon_corr) ~ "Navon",
                                   !is.na(OBW_overestimation_percent) ~ "OBW",
                                   !is.na(orientation_response) ~ "Orientation",
                                   !is.na(size_error_percent) ~ "Size")) %>% 
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
    
    # # Size conditions - the row after the last indicates the size condition for Size Task
    # fill(size_cond, .direction = "up") %>%
    # mutate("Size Condition" = case_when(`Task Type` == "Size" ~ size_cond)) %>%
    # No more size conditions in the new file
    
    select(`Task Type`, `Reaction Time`, `Accuracy`, `Overestimation Percent`, `Error Degrees`,
           `Navon - Consistency`, `Navon - Size`, `Navon - Condition`, `OBW Condition`,
           everything())
  
  # Outlier Detection (flag)
  participant_df_flagged <- participant_df %>%
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
  
  # Add column for outliers
  participant_df_flagged <- participant_df_flagged %>% 
    mutate("Delete for Outlier" = case_when(
      `Task Type` == "Navon" ~ `Reaction Time Outlier`,
      `Task Type` == "OBW" ~ `Accuracy Outlier`,
      `Task Type` == "OBW" ~ `Reaction Time Outlier`,
      `Task Type` == "Orientation" ~ `Overestimation Outlier`,
      `Task Type` == "Size" ~ `Error Degrees Outlier`,
      TRUE ~ FALSE
    ))
  
  # Collapse the conditions within each task type?
  
  
  ### Split-Half Reliability 
  
  # First assign ids for each trial
  participant_df_flagged <- participant_df_flagged %>% 
    ungroup() %>%
    filter(!`Delete for Outlier`, !is.na(`Task Type`)) %>% 
    mutate(trial_id = row_number())
  
  # splitting for the first sample
  set.seed(1234)
  participant_df_split1 <- participant_df_flagged %>%
    group_by(`Task Type`, `Navon - Consistency`, `Navon - Size`, `Navon - Condition`, `OBW Condition`) %>%
    mutate(sample_number = n()/2) %>% 
    # Sample half of each groups 
    sample_frac(0.5)
  
  # Create another table with trials that are not in split 1 table
  participant_df_split2 <- participant_df_flagged %>% 
    filter(!(trial_id %in% participant_df_split1$trial_id))
  
  # Checking the work
  # participant_df_split1 %>% summarise(n = n())
  # patritipant_df_split2 %>% 
  #   group_by(`Task Type`, `Navon - Consistency`, `Navon - Size`, `Navon - Condition`, `OBW Condition`) %>%
  #   summarise(n = n())

  
  
  # Summary table function
  summarise_participant <- function(participant_df){
    temp_df <- participant_df %>%
      filter(!`Delete for Outlier`) %>%
      # Group by to get summary statistics within levels
      group_by(`Task Type`, `Navon - Consistency`, `Navon - Size`, `Navon - Condition`, `OBW Condition`) %>%
      summarize("Mean RT" = mean(`Reaction Time`, na.rm = TRUE),
                "Number of Responses" = n(),
                "Mean Accuracy" = mean(Accuracy, na.rm = TRUE),
                "Mean Overestimation" = mean(`Overestimation Percent`, na.rm = TRUE),
                "Mean Error Degrees" = mean(`Error Degrees`, na.rm = TRUE)) %>% 
      mutate(key = case_when(`Task Type` == "Navon" ~ paste0(`Task Type`, " - ", `Navon - Condition`),
                             `Task Type` == "OBW" ~ paste0(`Task Type`, " - ", `OBW Condition`),
                             `Task Type` == "Orientation" ~ "Orientation"))
    return(temp_df)
  }
  
  # Summary for the main
  participant_df_flagged_summary <- participant_df_flagged %>% summarise_participant()
  long_df <- participant_df_flagged_summary %>%
    ungroup() %>%
    select(`Mean RT`, `Mean Accuracy`, `Mean Overestimation`, `Mean Error Degrees`, key)
  
  
  # Summary for Split 1
  summary_split1 <- participant_df_split1 %>% summarise_participant
  long_split1 <- summary_split1 %>% ungroup() %>%
    select(`Mean RT`, `Mean Accuracy`, `Mean Overestimation`, `Mean Error Degrees`, key)

  
  # Summary for Split 2 
  summary_split2 <- participant_df_split2 %>% summarise_participant
  long_split2 <- summary_split2 %>% ungroup() %>%
    select(`Mean RT`, `Mean Accuracy`, `Mean Overestimation`, `Mean Error Degrees`, key)
  
  #### TO WIDE with only one row
  # Function for wide
  to_wide <- function(long_df){
    temp_df <- long_df %>% filter(!is.na(key)) %>% pivot_longer(-key) %>% 
      mutate(myname = paste0(key, " - ", name)) %>%
      select(-key, -name) %>%
      pivot_wider(names_from = myname, values_from = value) 
    return(temp_df)
  }
  wide_output <- long_df %>% to_wide() %>% 
    mutate(participant = participant_id) %>% 
    select(participant, everything())
  
  # Wide for split 1
  wide_split1 <- long_split1  %>% 
    filter(!is.na(key)) %>% pivot_longer(-key) %>% 
    mutate(myname = paste0(key, " - ", name, " s1")) %>%
    select(-key, -name) %>%
    pivot_wider(names_from = myname, values_from = value) 
  
  # Wide for split 2
  wide_split2 <- long_split2 %>% 
    filter(!is.na(key)) %>% pivot_longer(-key) %>% 
    mutate(myname = paste0(key, " - ", name, " s2")) %>%
    select(-key, -name) %>%
    pivot_wider(names_from = myname, values_from = value) 
  
  # combine Split 1 & 2
  split_wide <- wide_split1 %>% add_column(wide_split2)
  # delete columns with NAs
  split_wide <- split_wide %>% select_if(~sum(!is.na(.)) > 0) %>% 
    # add participant ID
    mutate(participant = participant_id) %>% 
    select(participant, everything())
  
  # Bind to the all df
  all_df <- all_df %>%
    bind_rows(wide_output)
  
  # Bind to all split df
  all_split_df <- all_split_df %>%
    bind_rows(split_wide)
}

# Clear columns that have only NA's
# https://stackoverflow.com/a/45383054/6205282
all_df <- all_df %>% select_if(~sum(!is.na(.)) > 0)

# Write out the output
write_csv(all_df, here("output/Everything.csv"))

# Write out split file
write_csv(all_split_df, here("output/Split.csv"))
