library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)

# pvq <- read.csv("pvq.users.data.csv") #N = 5436
# 
# #setwd("Z:/usersurvey/_raw/survey/dupe_fix")
# demo <- read.csv("complete_responses_noipdupes.csv") #N= 5879
# 
# demo <- demo %>%
#   mutate(
#     gender_g = str_trim(Gender_Select)
#   )
# 
# demo <- demo %>% mutate(gender_g = case_when(gender_g == "Female (including transgender women)" ~ "female",
#                                              gender_g == "Male (including transgender men)" ~ "male",
#                                              TRUE ~ NA_character_))
# 
# # Merge two datasets: Female = 3033, Male = 1963
# all <- pvq %>% left_join(demo, by= "ResponseId")
# 
# # Filter by gender & value scores: N = 261, NA = 127 -> 134
# all_ver1 <- all %>% 
#   filter(gender_g == "female",
#          SELF.DIRECTIONscore >=5,
#          POWERscore <= 2)
# 
# # Filter by gender & value scores: N = 1448, NA = 642 -> 806
# all_ver2 <- all %>%
#   filter(gender_g == "female",
#          SELF.DIRECTIONscore >=4,
#          POWERscore <= 3)
# 
# # Create a dataset
# write.csv(all, "merged_male_female.csv", row.names = TRUE)

# Set working directory
setwd("C:/Users/hp/OneDrive - UW-Madison/Desktop/Dissertation_value_demo_data")

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

demographic_table <-  analysis %>% 
  select(c(3,14,13,15,16)) %>% 
  tbl_summary(missing = "no",label = list(single_eth = "Race/Ethnicity",AgeGroup = "Age Group")) %>% 
  modify_header(label ~ "**Variable**")  %>% 
  bold_labels()

gender_table <-  analysis %>% 
  select(c(3)) %>% 
  tbl_summary(missing = "no") %>%
  modify_header(label ~ "**Variable**")  %>%
  bold_labels()

age_table <-  analysis %>% 
  select(c(14)) %>% 
  tbl_summary(missing = "no",label = list(AgeGroup = "Age Group")) %>%
  modify_header(label ~ "**Variable**")  %>%
  bold_labels()

eth_table <-  analysis %>% 
  select(c(13)) %>% 
  tbl_summary(missing = "no",label = list(single_eth = "Race/Ethnicity")) %>%
  modify_header(label ~ "**Variable**")  %>%
  bold_labels()

edu_table <-  analysis %>% 
  select(c(15)) %>% 
  tbl_summary(missing = "no") %>%
  modify_header(label ~ "**Variable**")  %>%
  bold_labels()

income_table <-  analysis %>% 
  select(c(16)) %>% 
  tbl_summary(missing = "no") %>%
  modify_header(label ~ "**Variable**")  %>%
  bold_labels()





