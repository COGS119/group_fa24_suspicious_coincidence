library(here)
library(tidyverse)
library(jsonlite)
library(tidyjson)
source("helper.R")

processed_data_directory <- here("..","data","processed_data")
file_name <- "suspicious_coincidence"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv"))) %>%
  rename(participant_id=participant)

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#filter and select relevant data
processed_data <- exp_data %>%
  filter(trial_type=="html-button-response") %>%
  select(participant_id, random_id,trial_index,time_elapsed,condition:final_rt) %>%
  group_by(participant_id) %>%
  mutate(
    trial_number=seq(n())
  ) %>%
  relocate(
    trial_number,.after="trial_index"
  ) %>%
  mutate(choices = map(final_choice_array, ~ convert_array(.,column_name="test_choice"))) %>%
  unnest(choices) %>%
  mutate(
   choice_type = case_when(
      str_detect(test_choice,"sup") ~ "superordinate",
      str_detect(test_choice,"bas") ~ "basic",
      str_detect(test_choice,"sub") ~ "subordinate"
    )
  ) %>%
  mutate(
    choice_category = case_when(
      str_detect(test_choice,"c1") ~ "vegetables",
      str_detect(test_choice,"c2") ~ "vehicles",
      str_detect(test_choice,"c3") ~ "animals"
    )
  ) %>%
  mutate(choice_match_category = ifelse(choice_category == category,1,0)) %>%
  mutate(
    choice_type_training_consistent = case_when(
      level == "one-subordinate" &  choice_type %in% c("subordinate") & choice_match_category == 1 ~ 1,
      level == "subordinate" &  choice_type %in% c("subordinate") & choice_match_category == 1 ~ 1,
      level == "basic" &  choice_type %in% c("subordinate","basic") & choice_match_category == 1 ~ 1,
      level == "superordinate" &  choice_type %in% c("subordinate","basic","superordinate")& choice_match_category == 1 ~ 1,
      TRUE ~ 0
    )
  )
  

#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
