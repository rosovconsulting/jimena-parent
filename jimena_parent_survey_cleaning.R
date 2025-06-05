library(haven)
library(tidyverse)
library(janitor)
library(summarytools)
library(labelled)


view_summary_browser <- function(data, name = deparse(substitute(data))){
  summarytools::view(summarytools::dfSummary(data), method = "browser", report.title = name)
}


## Read in .sav file downloaded from Qualtrics
raw_survey_data <- read_sav("Data/277-04_JIMENA_RAW_Parent_Survey_data_20250605.sav") %>% 
  clean_names() %>% 
  as_factor()

## Filter incomplete responses
survey_data_1 <- raw_survey_data %>% 
  filter(progress > 70) %>% 
  select(response_id, school, q1:q3_4)

## save variable labels for later
variable_labels <- var_label(survey_data_1)

## Recode -99 to NA
survey_data_2 <- survey_data_1 %>% 
  mutate(across(everything(), ~na_if(., "-99"))) %>% 
  mutate(across(everything(), ~fct_recode(., NULL = "-99")))


## Calculate inclusion score for each parent response

positive_inclusion_items <- c(
  "qid22_1",
  "qid22_2",
  "qid22_3",
  "qid22_4",
  "qid24_1",
  "qid24_3",
  "qid25_2",
  "qid25_3",
  "qid25_5",
  "qid25_6",
  "qid26_1",
  "qid26_2",
  "qid26_3",
  "qid26_4",
  "qid26_5",
  "qid27_1",
  "qid28_1",
  "qid28_2",
  "qid28_3",
  "qid28_4"
)

survey_data_3 <- survey_data_2 %>%
  mutate(across(
    all_of(positive_inclusion_items),
    ~ case_match(
      .,
      "Strongly agree" ~ 2,
      "Agree" ~ 1,
      "Neither agree nor disagree" ~ 0,
      "Disagree" ~ -1,
      "Strongly disagree" ~ -2,
      .default = NA
    ),
    .names = "{.col}_score"
  )) %>% 
  mutate(q35_3_score = case_match(
    q35_3,
    "Strongly agree" ~ -2,
    "Agree" ~ -1,
    "Neither agree nor disagree" ~ 0,
    "Disagree" ~ 1,
    "Strongly disagree" ~ 2,
    .default = NA
  ))

survey_data_4 <- survey_data_3 %>% 
  mutate(inclusion_score = rowSums(select(., ends_with("_score")), na.rm = FALSE)) %>% 
  select(-matches("\\d_score"))
  

## Put variable labels back on
var_label(survey_data_4) <- variable_labels

write_rds(survey_data_4, "Data/277-04_JIMENA_Cleaned_Parent_Survey_Data_20250605.rds")

