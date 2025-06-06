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
  

## create mutually exclusive Jewish Ethnicity variable

survey_data_5 <- survey_data_4 %>% 
  mutate(q4_3 = case_when(
    q4_4_text == "Perisan" ~ "Mizrahi",
    q4_4_text == "Persian" ~ "Mizrahi",
    q4_4_text == "Persian jew" ~ "Mizrahi",
    .default = q4_3
  )) %>% 
  mutate(q4_4 = case_when(
    q4_4_text == "Perisan" ~ NA,
    q4_4_text == "Persian" ~ NA,
    q4_4_text == "Persian jew" ~ NA,
    .default = q4_4
  )) %>% 
  mutate(q4_parent_ethnicity = case_when(
    q4_5 == "Not Jewish" ~ "Not Jewish",
    q4_1 == "Ashkenazi" & is.na(q4_2) & is.na(q4_3) & is.na(q4_4) ~ "Only Ashkenazi",
    q4_1 == "Ashkenazi" & (q4_2 == "Sephardi" | q4_3 == "Mizrahi") ~ "Mixed Heritage",
    q4_2 == "Sephardi" | q4_3 == "Mizrahi" ~ "Sephardi/Mizrahi",
  )) %>% 
  mutate(q4_parent_ethnicity = factor(q4_parent_ethnicity, levels = c("Only Ashkenazi", "Sephardi/Mizrahi", "Mixed Heritage", "Not Jewish")))


survey_data_5 <- survey_data_4 %>% 
  mutate(q5_3 = case_when(
    q5_4_text == "Per" ~ "Mizrahi",
    q5_4_text == "Persian" ~ "Mizrahi",
    q5_4_text == "Persian Jews" ~ "Mizrahi",
    .default = q5_3
  )) %>% 
  mutate(q5_4 = case_when(
    q5_4_text == "Per" ~ NA,
    q5_4_text == "Persian" ~ NA,
    q5_4_text == "Persian Jews" ~ NA,
    .default = q5_4
  )) %>% 
  mutate(q5_1 = case_when(
    q5_4_text == "Ashkenazi and Sephardi" ~ "Ashkenazi",
    q5_4_text == "Mixed Ashkenazi and Sephardic" ~ "Ashkenazi",,
    .default = q5_1
  )) %>% 
  mutate(q5_2 = case_when(
    q5_4_text == "Ashkenazi and Sephardi" ~ "Sephardi",
    q5_4_text == "Mixed Ashkenazi and Sephardic" ~ "Sephardi",
    .default = q5_2
  )) %>% 
  mutate(q5_child_ethnicity = case_when(
    q5_5 == "Not Jewish" ~ "Not Jewish",
    q5_1 == "Ashkenazi" & is.na(q5_2) & is.na(q5_3) & is.na(q5_4) ~ "Only Ashkenazi",
    q5_1 == "Ashkenazi" & (q5_2 == "Sephardi" | q5_3 == "Mizrahi") ~ "Mixed Heritage",
    q5_2 == "Sephardi" | q5_3 == "Mizrahi" ~ "Sephardi/Mizrahi",
  )) %>% 
  mutate(q5_child_ethnicity = factor(q5_child_ethnicity, levels = c("Only Ashkenazi", "Sephardi/Mizrahi", "Mixed Heritage", "Not Jewish")))


## Create mutually exclusive ancestral origin variable
survey_data_6 <- survey_data_5 %>% 
  mutate(q6_ancestral_origin_israel = case_when(
      q6_9 == "Israel (immigration to US 1970s or later)" ~ "Israel",
      .default = NA
  )) %>% 
  mutate(q6_ancestral_origin_european = case_when(
    q6_3 == "Central or Eastern Europe (Ashkenazi)" ~ "European Diaspora",
    q6_5 == "Georgia (in eastern Europe)" ~ "European Diaspora",
    q6_6 == "Greece (Romaniote)" ~ "European Diaspora",
    q6_10 == "Italy (Italian rite)" ~ "European Diaspora",
    q6_14 == "Ottoman Sephardi (Greece, Turkey, Rhodes, Bulgaria, Yugoslavia)" ~ "European Diaspora",
    q6_16 == "Russia (immigration to US 1970s or later)" ~ "European Diaspora",
  )) %>% 
  mutate(q6_ancestral_origin_mena = case_when(
    q6_2 == "Bukharia (Uzbekistan, Tajikistan)" ~ "MENA Diaspora",
    q6_4 == "Ethiopia" ~ "MENA Diaspora",
    q6_7 == "India" ~ "MENA Diaspora",
    q6_8 == "Iraq" ~ "MENA Diaspora",
    q6_11 == "Juhuro (Azerbaijan, Dagestan)" ~ "MENA Diaspora",
    q6_12 == "Kurdistan" ~ "MENA Diaspora",
    q6_13 == "North Africa (Morocco, Algeria, Tunisia, Libya, Egypt)" ~ "MENA Diaspora",
    q6_15 == "Persia/Iran" ~ "MENA Diaspora",
    q6_17 == "Syria/Lebanon" ~ "MENA Diaspora",
    q6_18 == "Yemen" ~ "MENA Diaspora"
  )) %>% 
  mutate(q6_ancestral_origin_none = case_when(
    q6_1 == "None (no Jewish ancestors)" ~ "No Jewish Ancestry",
  )) %>% 
  mutate(q6_ancestral_origin = case_when(
    q6_ancestral_origin_israel == "Israel" ~ "Israel",
    q6_ancestral_origin_none == "No Jewish Ancestry" ~ "No Jewish Ancestry",
    q6_ancestral_origin_european == "European Diaspora" & q6_ancestral_origin_mena == "MENA Diaspora" ~ "Both European and MENA Diaspora",
    q6_ancestral_origin_european == "European Diaspora" ~ "European Diaspora",
    q6_ancestral_origin_mena == "MENA Diaspora" ~ "MENA Diaspora",
    .default = NA
  )) %>% 
  mutate(q6_ancestral_origin = factor(q6_ancestral_origin, levels = c("Israel", "Both European and MENA Diaspora", "European Diaspora", "MENA Diaspora", "No Jewish Ancestry")))

## Put variable labels back on
var_label(survey_data_6) <- variable_labels

write_rds(survey_data_6, "Data/277-04_JIMENA_Cleaned_Parent_Survey_Data_20250605.rds")

