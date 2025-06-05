library(haven)
library(tidyverse)
library(janitor)
library(summarytools)
library(labelled)

view_summary_browser <- function(data, name = deparse(substitute(data))){
  summarytools::view(summarytools::dfSummary(data), method = "browser", report.title = name)
}


## Read in .sav file downloaded from Qualtrics
raw_survey_data <- read_sav("Data/277-04_Jimena_RAW_Parent_Survey_data_20250605.sav") %>% 
  clean_names() %>% 
  as_factor()

