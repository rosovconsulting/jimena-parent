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

survey_data_1 <- raw_survey_data %>% 
  filter(progress > 70) %>% 
  select(response_id, school, q1:q3_4)


write_rds(survey_data_1, "Data/277-04_JIMENA_Cleaned_Parent_Survey_Data_20250605.rds")

