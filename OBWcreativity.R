# This code converts preprocessed xlsx file to a wide format 
# install.packages("tidyverse")
library(tidyverse)
library(readxl)


filenames <- list.files("./raw", full.names = TRUE)

# initialize the container
all_df <- tibble()


temp_df <- read_csv(filenames[1])

for(i in filenames){
  print(i)
  anton_df <- read_csv(i)
  anton_df <- anton_df %>% 
  mutate("Task Type" = case_when(row_number() >= 1 & row_number() <= 71 ~ "Cue",
                                 row_number() >= 72 & row_number() <= 121 ~ "CRA",
                                 row_number() >= 122 & row_number() <= 188 ~ "Navon",
                                 row_number() >= 189 & row_number() <= 218 ~ "Faces",
                                 row_number() >= 219 & row_number() <= 314 ~ "OBW")) %>%
  # Dependent Variables
  mutate("Reaction Time" = case_when(`Task Type` == "Cue" ~ cueTaskResponseKey.rt,
                                     `Task Type` == "CRA" ~ CRApresentationResponseKey.rt,
                                     `Task Type` == "Navon" ~ resp.rt,
                                     `Task Type` == "Faces" ~ facesResponseKey.rt,
                                     `Task Type` == "OBW" ~ 0)) %>%
  mutate("Keypress" = case_when(`Task Type` == "CRA" ~ insightReportKey.keys, 
                                # `Task Type` == "Navon" ~ resp.keys))
                                `Task Type` == "Faces" ~ facesRatingKey.keys)) %>%
  mutate("Accuracy" = case_when(`Task Type` == "Cue" ~ cueTaskResponseKey.corr,
                                `Task Type` == "CRA" ~ answerKey.corr,
                                `Task Type` == "Navon" ~ resp.corr)) %>%
  mutate("OBW - Overestimation %" = case_when(`Task Type` == "OBW" ~ (curSep - spacing)/spacing)) %>%

  # Independent Variables
  mutate("Cue Condition" = case_when(`Task Type` == "Cue" ~ cueCondition),
         "Navon - Consistency" = case_when(`Task Type` == "Navon" ~ congruence),
         "Navon - Size" = case_when(`Task Type` == "Navon" ~ report),
         "Navon - Condition" = case_when(`Task Type` == "Navon" ~ paste0(`Navon - Consistency`, "-", `Navon - Size`)),
         "OBW Condition" = case_when(`Task Type` == "OBW" ~ subcond)) %>% 
  mutate(`Cue Condition` = case_when(`Cue Condition` == 0 ~ 0,
                                     `Cue Condition` == 1 ~ 1,
                                     `Cue Condition` == 2 ~ 1,
                                     `Cue Condition` == 3 ~ 0)) %>% 
  mutate(`Cue Focus` = case_when(cueFocus == "big" ~ 0,
                                 cueFocus == "small" ~ 1)) %>%
  select(`Task Type`, `Reaction Time`, `Keypress`, `Accuracy`, `OBW - Overestimation %`,
         `Cue Condition`, `Navon - Consistency`, `Navon - Size`, `Navon - Condition`, `OBW Condition`,
         everything())

# Outlier Detection (flag)
anton_df_flagged <- anton_df %>%
  group_by(`Task Type`) %>%
  mutate("Mean within Condition" = mean(`Reaction Time`, na.rm = TRUE), "SD within Condition" = sd(`Reaction Time`, na.rm = TRUE),
         "Outlier" = case_when(`Reaction Time` >= `Mean within Condition` + 3 * `SD within Condition` ~ TRUE,
                               `Reaction Time` <= `Mean within Condition` - 3 * `SD within Condition` ~ TRUE,
                               TRUE ~ FALSE)) %>%
  # Delete trials with instructions, specified by position
  # Cue Task
  # filter(!(row_number() %in% c(9,18,27,36,45,54,63))) %>%
  # # Navon
  # filter(!(row_number() %in% c(138,155,172))) %>%
  # Delete the practice rows for Navon task: 161-193
  #filter(case_when(cueTaskResponseKey.corr != 0 ~ TRUE,
  #                 is.na(cueTaskResponseKey.corr) ~ TRUE)) %>%
  #filter(case_when(resp.corr != 0 ~ TRUE,
  #                 is.na(resp.corr) ~ TRUE)) %>%
  mutate(cueTaskResponseKey.rt = case_when(cueTaskResponseKey.corr == 0 ~ NaN,
                                           TRUE ~ cueTaskResponseKey.rt)) %>%
  mutate(resp.rt = case_when(resp.corr == 0 ~ NaN,
                             TRUE ~ resp.rt)) %>%
  select(`Mean within Condition`, `SD within Condition`, `Outlier`, everything())

# 


anton_df_flagged_summary <- anton_df_flagged %>%
  mutate("Delete for RT or Outlier" = case_when(is.na(`Reaction Time`) & `Task Type` != "OBW" ~ TRUE & 'Task Type' != "CRA",
                                     Outlier == TRUE & `Task Type` != "OBW" ~ TRUE & 'Task Type' != "CRA",
                                     TRUE ~ FALSE)) %>%
  filter(!`Delete for RT or Outlier`) %>%
  group_by(`Task Type`, `Cue Condition`, `Cue Focus`, `Navon - Consistency`, `Navon - Size`, `Navon - Condition`, `OBW Condition`) %>%
  summarize("Mean RT" = mean(`Reaction Time`, na.rm = TRUE),
            "Number of Responses" = n(),
            "Mean Response" = mean(`Keypress`, na.rm = TRUE),
            # "Global Responses" = sum(`Response` == "Global", na.rm = TRUE),
            # "Local Responses" = sum(`Response` == "Local", na.rm = TRUE),
            # "Proportion - Global" = `Global Responses` / `Number of Responses`,
            "Mean Accuracy" = mean(Accuracy, na.rm = TRUE),
            "Mean Overestimation" = mean(`OBW - Overestimation %`, na.rm = TRUE)) %>% 
  mutate(key = case_when(`Task Type` == "Navon" ~ paste0(`Task Type`, " - ", `Navon - Condition`),
                         `Task Type` == "Cue" ~ paste0(`Task Type`, " - ", `Cue Condition`, " - Focus ", `Cue Focus`),
                         `Task Type` == "OBW" ~ paste0(`Task Type`, " - ", `OBW Condition`),
                         `Task Type` == "Faces" ~ paste0(`Task Type`),
                         `Task Type` == "CRA" ~ paste0(`Task Type`)))

my_longest_table <- anton_df_flagged_summary %>%
  ungroup() %>%
  select(`Mean RT`, `Mean Accuracy`, `Mean Overestimation`, `Mean Response`, key)


#### TO WIDE !!!!!
wide_output <- my_longest_table %>% filter(!is.na(key)) %>% pivot_longer(-key) %>% 
  mutate(myname = paste0(key, " - ", name)) %>%
  select(-key, -name) %>%
  pivot_wider(names_from = myname, values_from = value) 

wide_output <- wide_output %>%
  select(`CRA - Mean RT`,
         `CRA - Mean Accuracy`,
         `CRA - Mean Response`,
         `Cue - 0 - Focus 0 - Mean RT`,
         `Cue - 0 - Focus 0 - Mean Accuracy`,
         `Cue - 0 - Focus 1 - Mean RT`,
         `Cue - 0 - Focus 1 - Mean Accuracy`,
         `Cue - 1 - Focus 0 - Mean RT`,
         `Cue - 1 - Focus 0 - Mean Accuracy`,
         `Cue - 1 - Focus 1 - Mean RT`,
         `Cue - 1 - Focus 1 - Mean Accuracy`,
         `Faces - Mean RT`,
         `Faces - Mean Response`,
         # Navon tasks
         `Navon - consistent-small - Mean RT`,
         `Navon - consistent-big - Mean RT`,
         `Navon - conflicting-small - Mean RT`,
         `Navon - conflicting-big - Mean RT`,
         `Navon - consistent-small - Mean Accuracy`,
         `Navon - consistent-big - Mean Accuracy`,
         `Navon - conflicting-small - Mean Accuracy`,
         `Navon - conflicting-big - Mean Accuracy`,
         # OBW
         `OBW - none - Mean Overestimation`,
         `OBW - object - Mean Overestimation`) %>%
  mutate("Filename" = i) %>% 
  select(Filename, everything())


#write_csv(wide_output, paste0(i, "- wide.csv"))

all_df <- all_df %>%
  bind_rows(wide_output)
}

write_csv(all_df, "Everything.csv")
