library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)

setwd("Z:/usersurvey/_raw/survey")
pvq <- read.csv("pvq.users.data.csv") #N = 5436

setwd("Z:/usersurvey/_raw/survey/dupe_fix")
demo <- read.csv("complete_responses_noipdupes.csv") #N= 5879

demo <- demo %>%
  mutate(
    gender_g = str_trim(Gender_Select)
  )

demo <- demo %>% mutate(gender_g = case_when(gender_g == "Female (including transgender women)" ~ "female",
                                             gender_g == "Male (including transgender men)" ~ "male",
                                             TRUE ~ NA_character_))

# Merge two datasets: Female = 3033, Male = 1963
all <- pvq %>% left_join(demo, by= "ResponseId")

# Filter by gender & value scores: N = 261, NA = 127 -> 134
all_ver1 <- all %>% 
              filter(gender_g == "female",
                     SELF.DIRECTIONscore >=5,
                     POWERscore <= 2)

# Filter by gender & value scores: N = 1448, NA = 642 -> 806
all_ver2 <- all %>% 
  filter(gender_g == "female",
         SELF.DIRECTIONscore >=4,
         POWERscore <= 3)


