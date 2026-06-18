library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)

# Creating a dataset: "merged_male_female.csv"
# setwd("/Users/hp/Dissertation_value_demo_data/study2")
# pvq <- read.csv("pvq.users.data.csv") #N = 5436 (Study1 stats. Female: 3,141, Male: 2,020, Other:192)
# demo <- read.csv("complete_responses_noipdupes.csv") #N= 5879
# 
# demo <- demo %>%
#   mutate(
#     gender_g = str_trim(Gender_Select),
#     gender_g = recode(
#       gender_g,
#       "Female (including transgender women)" = "female",
#       "Male (including transgender men)" = "male"
#     ),
#     gender_g = case_when(
#       gender_g %in% c("female", "male") ~ gender_g,
#       TRUE ~ "other"
#     )
#   )
# 
# # Merge two datasets: N = 5436/ Female = 3,033 (valid email = 1,817), Male = 1963, Other = 271, NA = 169
# all <- pvq %>% left_join(demo, by= "ResponseId")

# # Filter by gender & value scores: N = 261, valid email = 134 (NA = 127)
# all_ver1 <- all %>%
#   filter(gender_g == "female",
#          SELF.DIRECTIONscore >=5,
#          POWERscore <= 2)
# 
# # Filter by gender & value scores: N = 1448, valid email = 806 (NA = 642)
# all_ver2 <- all %>%
#   filter(gender_g == "female",
#          SELF.DIRECTIONscore >=4,
#          POWERscore <= 3)

# Save a dataset
#write.csv(all, "merged_male_female.csv", row.names = TRUE)

# Import data
demo_value <- read.csv("merged_male_female.csv", header = TRUE)
survey <- read.csv("final_zooniverse_survey.csv", header = TRUE)

# Clean data
## Demo_value
demo_value <- demo_value %>% mutate(email = str_trim(RaffleEmail))
demo_value$email <- tolower(str_trim(demo_value$email))

## Survey
survey <- survey[-c(1,2),]

#which(names(survey) == "Q26_1")
#survey <- survey %>% rename(email = Q26_1)

survey <- survey %>%
  mutate(email = str_trim(Q26_1))

survey$email <- tolower(str_trim(survey$email))

# Merge data
merged <- survey %>% left_join(demo_value, by = "email")

# Select rows
data <- merged %>% filter(email !="")

# Check meta-data
metadata <- data[,c(4,14,15,59,61,110,114,119,120)] 

which(duplicated(data$email)) #row 5, 51 -> choose the first response
data <- data[c(-5,-51),]
sum(is.na(data$ResponseId.y)) #N = 2 (submitted email addresses different from the previous ones)

# Emails not linked to value scores
#gg2032@tc.columbia.edu
#katie.burnette@ucr.edu

# Create a dataset for analysis
analysis <- data[,c(9,21:59,62:71,94:97,125,126,128,131,135,276)]

# Recode Likert scale
analysis <- analysis %>%
  mutate(across(
    c(Q78_1, Q78_2, Q78_3),
    ~ ifelse(. == "7", "I do not know", .)
  ))

# Recode age, ethnicity, education, income level

### ETH 
single_eth <- c("White (A person having origins in any of the original peoples of Europe, the Middle East, or North Africa.)","Asian (A person having origins in any of the original peoples of the Far East, Southeast Asia, or the Indian subcontinent)","Black (A person having origins in any of the black racial groups of Africa.)","Hispanic/Latino/Spanish origin (A person of Cuban, Mexican, Puerto Rican, South or Central American, or other Spanish culture or origin, regardless of race)","Another race - Please specify","Prefer not to answer")

analysis$single_eth <- 
  ifelse(!analysis$Ethnicity_Select %in% single_eth,"Multi-racial",
         paste0(analysis$Ethnicity_Select))


analysis$single_eth <- dplyr::recode(analysis$single_eth, `White (A person having origins in any of the original peoples of Europe, the Middle East, or North Africa.)` = "White",`Asian (A person having origins in any of the original peoples of the Far East, Southeast Asia, or the Indian subcontinent)` =  "Asian", `Black (A person having origins in any of the black racial groups of Africa.)`= "Black", `Hispanic/Latino/Spanish origin (A person of Cuban, Mexican, Puerto Rican, South or Central American, or other Spanish culture or origin, regardless of race)` ="Hispanic", `Another race - Please specify` = "Other", `Prefer not to answer`="No Response")


analysis$single_eth <- factor(analysis$single_eth, levels=c('Asian', 'Black', 'Hispanic', 'Multi-racial','White', 'Other',"No Response"))

### AGE 
analysis <- analysis %>% 
  mutate(AgeGroup = case_when(
    Age > 64 ~ 'Above 64',                                           
    Age >= 55  & Age <= 64 ~ '55-64',
    Age >= 45  & Age <= 54 ~ '45-54',
    Age >= 35  & Age <= 44 ~ '35-44',
    Age >= 25  & Age <= 34 ~ '25-34',
    Age >= 18  & Age <= 24 ~ '18-24')) 

# Schooling
analysis$Education <- dplyr::recode(analysis$SchoolHighestEdu, 
                                                   `Short-cycle tertiary education, e.g. vocational program` = "Vocational", 
                                                   `Post-secondary non-tertiary education` = "Vocational",
                                                   `Lower secondary education` = "< Bachelors",
                                                   `Primary education` = "< Bachelors",
                                                   `Upper secondary education` = "< Bachelors",
                                                   `Early childhood Education` = "< Bachelors",
                                                   `Bachelor's or equivalent` = "Bachelor's",
                                                   `Master's or equivalent` = "Master's",
                                                   `Doctoral or equivalent` = "Doctorate",
                                                   `Prefer not to answer` = "No Response"
)

analysis$Education <- factor(analysis$Education, levels=c('< Bachelors', 'Bachelor\'s','Vocational',
                                                                                        'Master\'s','Doctorate','No Response'))


analysis <- analysis %>% 
  mutate(Income=ifelse(Income %in% c("Less than $1,000","$1,000 to $1,999","$2,000 to $2,999"), "Low",
                       ifelse(Income %in% c("$3,000 to $3,999","$4,000 to $4,999"),"Middle",
                              ifelse(Income %in% c("$5,000 or more"),"High", NA))))

analysis$Income <- factor(analysis$Income, levels=c('Low','Middle','High'))


# Summary table
make_table <- function(data, var) {
  data %>%
    count({{ var }}) %>%
    mutate(percent = round(100 * n / sum(n), 1))
}

age_table    <- make_table(analysis, AgeGroup)
eth_table    <- make_table(analysis, single_eth)
edu_table    <- make_table(analysis, Education)
income_table <- make_table(analysis, Income)

## Age
age_t <- analysis %>% 
  dplyr::filter(AgeGroup != "NA") %>%
  dplyr::group_by(AgeGroup) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(age_t)[1] <- "Age"

## Race
race_t <- analysis %>% 
  dplyr::filter(single_eth != "NA") %>%
  dplyr::group_by(single_eth) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(race_t)[1] <- "Race"

## Education
edu_t <- analysis %>% 
  dplyr::filter(Education != "NA") %>%
  dplyr::group_by(Education) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(edu_t)[1] <- "Education"

## Income
income_t <- analysis %>% 
  dplyr::filter(Income != "NA") %>%
  dplyr::group_by(Income) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(income_t)[1] <- "Income"




