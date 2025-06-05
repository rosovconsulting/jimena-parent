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

## Recode -99 to NA
survey_data_2 <- survey_data_1 %>% 
  mutate(across(everything(), ~na_if(., "-99"))) %>% 
  mutate(across(everything(), ~fct_recode(., NULL = "-99")))


write_rds(survey_data_2, "Data/277-04_JIMENA_Cleaned_Parent_Survey_Data_20250605.rds")

