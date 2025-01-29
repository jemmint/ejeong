library(DescTools)
library(tidyverse)
library(agricolae)
library(ggpubr)
library(car)
library(broom)
library(ggplot2)
library(openxlsx)
library(DT)
library(dplyr)
library(kableExtra)

# get_os <- function(){
#   sysinf <- Sys.info()
#   if (!is.null(sysinf)){
#     os <- sysinf['sysname']
#     if (os == 'Darwin')
#       os <- "osx"
#   } else { ## mystery machine
#     os <- .Platform$OS.type
#     if (grepl("^darwin", R.version$os))
#       os <- "osx"
#     if (grepl("linux-gnu", R.version$os))
#       os <- "linux"
#   }
#   tolower(os)
# }
# 
# if(get_os() == "osx") {setwd("/Volumes/cbjackson2/usersurvey/_raw/survey/")} else
# {setwd("Z:/usersurvey/_raw/survey/")} # SHOULD AUTOMATICALLY SETWD
# 
# pvq.users.data <- read.csv("pvq.users.data.csv")
# hv_analysis <- read.csv("values_analysis.csv")
# user_data <- merge(pvq.users.data, hv_analysis, by="ResponseId")
# 
# # Add score ranks (listing value priorities) 
# value_ranks <-  data.frame(t(apply(-user_data[,c(3:12)], 1, rank, ties.method='min')))
# names(value_ranks) <- paste("rank" ,names(value_ranks),sep="-")
# user_data <- cbind(user_data,value_ranks)
# 
# # CONVERT OUTCOMES FOR VALUE RANK ANALYSIS
# user_data <- user_data %>%
#   mutate(Age_g=ifelse(Age<39, "Young", 
#                       ifelse(Age<65, "Middle", "Old"))) # No non responses
# 
# 
# user_data <- user_data %>% 
#   mutate(Income_g=ifelse(Income %in% c("Less than $1,000","$1,000 to $1,999","$2,000 to $2,999"), "Low",
#                          ifelse(Income %in% c("$3,000 to $3,999","$4,000 to $4,999"),"Middle",
#                                 ifelse(Income %in% c("$5,000 or more"),"High", NA)))) # LOOKS LIKE SOME ODDITIES IN EARLIER CODE ON LINE 84
# 
# user_data <- user_data %>% mutate(Ethnicity_g=ifelse(Ethnicity_Select == "White (A person having origins in any of the original peoples of Europe, the Middle East, or North Africa.)","White",
#                                                      ifelse(Ethnicity_Select == "Asian (A person having origins in any of the original peoples of the Far East, Southeast Asia, or the Indian subcontinent)","Asian",
#                                                             ifelse(Ethnicity_Select == "Black (A person having origins in any of the black racial groups of Africa.)","Black", 
#                                                                    ifelse(Ethnicity_Select == "Hispanic/Latino/Spanish origin (A person of Cuban, Mexican, Puerto Rican, South or Central American, or other Spanish culture or origin, regardless of race)","Hispanic", 
#                                                                           ifelse(grepl("Prefer", Ethnicity_Select),"Prefer not to answer", "Multiple Ethnicity/Other"))))))
# 
# user_data <- user_data %>% mutate(Edu_g=ifelse(SchoolHighestEdu %in% c("Early childhood Education","Primary education"), "LessThanHighSchool",
#                                                ifelse(SchoolHighestEdu %in%c("Upper secondary education", "Lower secondary education"),"HighSchoolEducation",
#                                                       ifelse(SchoolHighestEdu %in%c("Post-secondary non-tertiary education", "Short-cycle tertiary education, e.g. vocational program"),"VocEducation",
#                                                              ifelse(SchoolHighestEdu %in%c("Bachelor's or equivalent"),"CollegeEducation",
#                                                                     ifelse(SchoolHighestEdu %in%c("Doctoral or equivalent","Master's or equivalent"),"AdvancedDegree",
#                                                                            ifelse(SchoolHighestEdu %in% c("Prefer not to answer"),"Prefer not to answer",NA)))))))
# 
# 
# #Value ranks for our outcomes
# rank_analysis <- user_data[,c(106:119,49)] # EXTRACT RANKS FOR EASY ANALYSIS 
# value_results <- data.frame() # Holder for loop to store results
# tukey_results <- data.frame() # Holder for loop to store results for tukey
# 
# 
# ### ### ### ### 
# ### AGE ### 
# ### ### ### ### 
# for(i in 1:10)
# {
#   
#   column <- names(rank_analysis[i])
#   #tidy will summarise and return neat format
#   avz <- broom::tidy(aov(rank_analysis[,i] ~ Age_g, data = rank_analysis))
#   avz <- cbind(avz,column)
#   # Add this condition if you only want aov with P < 0.05 printed
#   #if(avz$p.value[1] < 0.05) {
#   #  print(column)
#   #  print(avz)
#   #}
#   value_results <- rbind(value_results,avz)
# }
# 
# for (i in 1:4){
#   column <- names(rank_analysis[i])
#   anova <- aov(rank_analysis[,i] ~ Age_g, data = rank_analysis)
#   tukey <- TukeyHSD(anova)
#   tukey <- tidy(tukey)
#   tukey <- cbind(tukey,column)
#   # only want tukey with P < 0.07 printed
#   # if(tukey[["p adj"]] < 0.07) {
#   # print(column)
#   # print(tukey)
#   tukey_results <- rbind(tukey_results,tukey)
#   }
# 
# 
# ### ### ### ### 
# ### GENDER ### 
# ### ### ### ### 
# for(i in 1:10)
# {
#   
#   column <- names(rank_analysis[i])
#   #tidy will summarise and return neat format
#   avz <- broom::tidy(aov(rank_analysis[,i] ~ Gender_Select, data = rank_analysis))
#   avz <- cbind(avz,column)
#   # Add this condition if you only want aov with P < 0.05 printed
#   #if(avz$p.value[1] < 0.05) {
#   #  print(column)
#   #  print(avz)
#   #}
#   value_results <- rbind(value_results,avz)
# }
# 
# for (i in 1:4){
#   column <- names(rank_analysis[i])
#   anova <- aov(rank_analysis[,i] ~ Gender_Select, data = rank_analysis)
#   tukey <- TukeyHSD(anova)
#   tukey <- tidy(tukey)
#   tukey <- cbind(tukey,column)
#   # only want tukey with P < 0.07 printed
#   # if(tukey[["p adj"]] < 0.07) {
#   # print(column)
#   # print(tukey)
#   tukey_results <- rbind(tukey_results,tukey)
# }
# 
# 
# ### ### ### ### 
# ### INCOME ### 
# ### ### ### ### 
# 
# for(i in 1:10)
# {
#   
#   column <- names(rank_analysis[i])
#   #tidy will summarise and return neat format
#   avz <- broom::tidy(aov(rank_analysis[,i] ~ Income_g, data = rank_analysis))
#   avz <- cbind(avz,column)
#   # Add this condition if you only want aov with P < 0.05 printed
#   #if(avz$p.value[1] < 0.05) {
#   #  print(column)
#   #  print(avz)
#   #}
#   value_results <- rbind(value_results,avz)
# }
# 
# for (i in 1:4){
#   column <- names(rank_analysis[i])
#   anova <- aov(rank_analysis[,i] ~ Income_g, data = rank_analysis)
#   tukey <- TukeyHSD(anova) 
#   tukey <- tidy(tukey) # CONVERT DATA FROM LIST TO DATAFRAME
#   tukey <- cbind(tukey,column) # ADD THE NAME OF THE COLUMN 
#   # only want tukey with P < 0.07 printed
#   # if(tukey[["p adj"]] < 0.07) {
#   # print(column)
#   # print(tukey)
#   tukey_results <- rbind(tukey_results,tukey)
# }
# 
# ### ### ### ### ### ###  
# ### RACE ANALYSIS ### 
# ### ### ### ### ### ###  
# 
# for(i in 1:10)
# {
#   
#   column <- names(rank_analysis[i])
#   #tidy will summarise and return neat format
#   avz <- broom::tidy(aov(rank_analysis[,i] ~ Ethnicity_g, data = rank_analysis))
#   avz <- cbind(avz,column)
#   # Add this condition if you only want aov with P < 0.05 printed
#   #if(avz$p.value[1] < 0.05) {
#   #  print(column)
#   #  print(avz)
#   #}
#   value_results <- rbind(value_results,avz)
# }
# 
# for (i in 1:4){
#   column <- names(rank_analysis[i])
#   anova <- aov(rank_analysis[,i] ~ Ethnicity_g, data = rank_analysis)
#   tukey <- TukeyHSD(anova) 
#   tukey <- tidy(tukey) # CONVERT DATA FROM LIST TO DATAFRAME
#   tukey <- cbind(tukey,column) # ADD THE NAME OF THE COLUMN 
#   # only want tukey with P < 0.07 printed
#   # if(tukey[["p adj"]] < 0.07) {
#   # print(column)
#   # print(tukey)
#   tukey_results <- rbind(tukey_results,tukey)
# }
# 
# 
# ### ### ### ### ### ###  
# ### EDU ANALYSIS ### 
# ### ### ### ### ### ###  
# 
# for(i in 1:10)
# {
#   
#   column <- names(rank_analysis[i])
#   #tidy will summarise and return neat format
#   avz <- broom::tidy(aov(rank_analysis[,i] ~ Edu_g, data = rank_analysis))
#   avz <- cbind(avz,column)
#   # Add this condition if you only want aov with P < 0.05 printed
#   #if(avz$p.value[1] < 0.05) {
#   #  print(column)
#   #  print(avz)
#   #}
#   value_results <- rbind(value_results,avz)
# }
# 
# for (i in 1:4){
#   column <- names(rank_analysis[i])
#   anova <- aov(rank_analysis[,i] ~ Edu_g, data = rank_analysis)
#   tukey <- TukeyHSD(anova) 
#   tukey <- tidy(tukey) # CONVERT DATA FROM LIST TO DATAFRAME
#   tukey <- cbind(tukey,column) # ADD THE NAME OF THE COLUMN 
#   # only want tukey with P < 0.07 printed
#   # if(tukey[["p adj"]] < 0.07) {
#   # print(column)
#   # print(tukey)
#   tukey_results <- rbind(tukey_results,tukey)
# }
# 
# ## REMOVE RESIDUALS 
# value_results <- value_results[which(value_results$term != "Residuals"),]
# 
# #write.csv(value_results,"/Volumes/cbjackson2/usersurvey/_CT2023/value_results.csv")
# #write.csv(tukey_results,"/Volumes/cbjackson2/usersurvey/_CT2023/tukey_results.csv")

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

if(get_os() == "osx") {setwd("/Volumes/cbjackson2/usersurvey/_raw/survey/")} else
{setwd("Z:/usersurvey/_raw/survey/")} # SHOULD AUTOMATICALLY SETWD

#### 
pvq.users.data <- read.csv("pvq.users.data.csv")
hv_analysis <- read.csv("values_analysis.csv")
user_data <- merge(pvq.users.data, hv_analysis, by="ResponseId")

# Add score ranks (listing value priorities) 
value_ranks <-  data.frame(t(apply(-user_data[,c(3:12)], 1, rank, ties.method='min')))
names(value_ranks) <- paste("rank" ,names(value_ranks),sep="-")
user_data <- cbind(user_data,value_ranks)

# CONVERT OUTCOMES FOR VALUE RANK ANALYSIS
user_data <- user_data %>%
  mutate(Age_g=ifelse(Age<39, "Young", 
                      ifelse(Age<65, "Middle", "Old"))) # No non responses

user_data <- user_data %>% 
  mutate(Income_g=ifelse(Income %in% c("Less than $1,000","$1,000 to $1,999","$2,000 to $2,999"), "Low",
                         ifelse(Income %in% c("$3,000 to $3,999","$4,000 to $4,999"),"Middle",
                                ifelse(Income %in% c("$5,000 or more"),"High", NA)))) # LOOKS LIKE SOME ODDITIES IN EARLIER CODE ON LINE 84

user_data <- user_data %>% mutate(Ethnicity_g=ifelse(Ethnicity_Select == "White (A person having origins in any of the original peoples of Europe, the Middle East, or North Africa.)","White",
                                                     ifelse(Ethnicity_Select == "Asian (A person having origins in any of the original peoples of the Far East, Southeast Asia, or the Indian subcontinent)","Asian",
                                                            ifelse(Ethnicity_Select == "Black (A person having origins in any of the black racial groups of Africa.)","Black", 
                                                                   ifelse(Ethnicity_Select == "Hispanic/Latino/Spanish origin (A person of Cuban, Mexican, Puerto Rican, South or Central American, or other Spanish culture or origin, regardless of race)","Hispanic", 
                                                                          ifelse(grepl("Prefer", Ethnicity_Select),"Prefer not to answer", "Multiple Ethnicity/Other"))))))

user_data <- user_data %>% mutate(Edu_g=ifelse(SchoolHighestEdu %in% c("Early childhood Education","Primary education"), "LessThanHighSchool",
                                               ifelse(SchoolHighestEdu %in%c("Upper secondary education", "Lower secondary education"),"HighSchoolEducation",
                                                      ifelse(SchoolHighestEdu %in%c("Post-secondary non-tertiary education", "Short-cycle tertiary education, e.g. vocational program"),"VocEducation",
                                                             ifelse(SchoolHighestEdu %in%c("Bachelor's or equivalent"),"CollegeEducation",
                                                                    ifelse(SchoolHighestEdu %in%c("Doctoral or equivalent","Master's or equivalent"),"AdvancedDegree",
                                                                           ifelse(SchoolHighestEdu %in% c("Prefer not to answer"),"Prefer not to answer",NA)))))))

user_data <- user_data %>%
  mutate(gender_g=ifelse(Gender_Select== "Female (including transgender women)", "Female", 
                         ifelse(Gender_Select== "Male (including transgender men)", "Male",
                                ifelse(Gender_Select== "Prefer to self describe as ____________ (non-binary, gender-fluid, agender, please specify)","Other","Prefer not to answer"))))


#Value ranks for our outcomes
rank_analysis <- user_data[,c(106:120)] # EXTRACT RANKS FOR EASY ANALYSIS 
value_results <- data.frame() # Holder for loop to store results
tukey_results <- data.frame() # Holder for loop to store results for tukey


### ### ### ### 
### AGE ### 
### ### ### ### 
for(i in 1:10)
{
  column <- names(rank_analysis[i])
  avz <- broom::tidy(aov(rank_analysis[,i] ~ Age_g, data = rank_analysis)) # remove people who prefer not to answer
  avz <- cbind(avz,column)
  value_results <- rbind(value_results,avz)
}

for (i in 1:10){
  column <- names(rank_analysis[i])
  anova <- aov(rank_analysis[,i] ~ Age_g, data = rank_analysis)
  tukey <- TukeyHSD(anova)
  tukey <- tidy(tukey)
  tukey <- cbind(tukey,column)
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_results <- rbind(tukey_results,tukey)
}


### ### ### ### 
### GENDER ### 
### ### ### ### 
for(i in 1:10)
{
  
  column <- names(rank_analysis[i])
  #tidy will summarise and return neat format
  avz <- broom::tidy(aov(rank_analysis[,i] ~ gender_g, data = rank_analysis))
  avz <- cbind(avz,column)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(avz$p.value[1] < 0.05) {
  #  print(column)
  #  print(avz)
  #}
  value_results <- rbind(value_results,avz)
}

for (i in 1:10){
  column <- names(rank_analysis[i])
  anova <- aov(rank_analysis[,i] ~ gender_g, data = rank_analysis)
  tukey <- TukeyHSD(anova)
  tukey <- tidy(tukey)
  tukey <- cbind(tukey,column)
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_results <- rbind(tukey_results,tukey)
}


### ### ### ### 
### INCOME ### 
### ### ### ### 

for(i in 1:10)
{
  
  column <- names(rank_analysis[i])
  #tidy will summarise and return neat format
  avz <- broom::tidy(aov(rank_analysis[,i] ~ Income_g, data = rank_analysis))
  avz <- cbind(avz,column)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(avz$p.value[1] < 0.05) {
  #  print(column)
  #  print(avz)
  #}
  value_results <- rbind(value_results,avz)
}

for (i in 1:10){
  column <- names(rank_analysis[i])
  anova <- aov(rank_analysis[,i] ~ Income_g, data = rank_analysis)
  tukey <- TukeyHSD(anova) 
  tukey <- tidy(tukey) # CONVERT DATA FROM LIST TO DATAFRAME
  tukey <- cbind(tukey,column) # ADD THE NAME OF THE COLUMN 
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_results <- rbind(tukey_results,tukey)
}

### ### ### ### ### ###  
### RACE ANALYSIS ### 
### ### ### ### ### ###  

for(i in 1:10)
{
  
  column <- names(rank_analysis[i])
  rank_analysis_race <- rank_analysis[which(rank_analysis$Ethnicity_g != "Prefer not to answer"),]
  #tidy will summarise and return neat format
  avz <- broom::tidy(aov(rank_analysis_race[,i] ~ Ethnicity_g, data = rank_analysis_race))
  avz <- cbind(avz,column)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(avz$p.value[1] < 0.05) {
  #  print(column)
  #  print(avz)
  #}
  value_results <- rbind(value_results,avz)
}

for (i in 1:10){
  column <- names(rank_analysis[i])
  rank_analysis_race <- rank_analysis[which(rank_analysis$Ethnicity_g != "Prefer not to answer"),]
  anova <- aov(rank_analysis_race[,i] ~ Ethnicity_g, data = rank_analysis_race)
  tukey <- TukeyHSD(anova) 
  tukey <- tidy(tukey) # CONVERT DATA FROM LIST TO DATAFRAME
  tukey <- cbind(tukey,column) # ADD THE NAME OF THE COLUMN 
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_results <- rbind(tukey_results,tukey)
}


### ### ### ### ### ###  
### EDU ANALYSIS ### 
### ### ### ### ### ###  

for(i in 1:10)
{
  
  column <- names(rank_analysis[i])
  rank_analysis_edu <- rank_analysis[which(rank_analysis$Ethnicity_g != "Prefer not to answer"),]
  avz <- broom::tidy(aov(rank_analysis_edu[,i] ~ Edu_g, data = rank_analysis_edu))
  avz <- cbind(avz,column)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(avz$p.value[1] < 0.05) {
  #  print(column)
  #  print(avz)
  #}
  value_results <- rbind(value_results,avz)
}

for (i in 1:10){
  column <- names(rank_analysis[i])
  rank_analysis_edu <- rank_analysis[which(rank_analysis$Ethnicity_g != "Prefer not to answer"),]
  anova <- aov(rank_analysis_edu[,i] ~ Edu_g, data = rank_analysis_edu)
  tukey <- TukeyHSD(anova) 
  tukey <- tidy(tukey) # CONVERT DATA FROM LIST TO DATAFRAME
  tukey <- cbind(tukey,column) # ADD THE NAME OF THE COLUMN 
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_results <- rbind(tukey_results,tukey)
}

## REMOVE RESIDUALS 
value_results <- value_results[which(value_results$term != "Residuals"),]

value_results <- value_results %>% 
  mutate_if(is.numeric, round,digits=3)

tukey_results <- tukey_results %>% 
  mutate_if(is.numeric, round,digits=3)

# Summarize values https://stackoverflow.com/questions/21644848/summarizing-multiple-columns-with-dplyr 
age_g <- user_data[,c(3:12,116)] %>% group_by(Age_g) %>% 
  summarise_at(.vars = names(.)[1:10],
               .funs = c(mean="mean")) 
age_g <- data.frame(age_g[,1],t(apply(-age_g[,c(2:11)], 1, rank, ties.method='min')))
names(age_g)[1] <- "value"
age_g$group <- "Age"

income_g <- user_data[,c(3:12,117)] %>% group_by(Income_g) %>% 
  summarise_at(.vars = names(.)[1:10],
               .funs = c(mean="mean")) 
income_g <- data.frame(income_g[,1],t(apply(-income_g[,c(2:11)], 1, rank, ties.method='min')))
names(income_g)[1] <- "value"
income_g$group <- "Income"

eth_g <- user_data[,c(3:12,118)] %>% group_by(Ethnicity_g) %>% 
  summarise_at(.vars = names(.)[1:10],
               .funs = c(mean="mean")) 
eth_g <- data.frame(eth_g[,1],t(apply(-eth_g[,c(2:11)], 1, rank, ties.method='min')))
names(eth_g)[1] <- "value"
eth_g$group <- "Ethnicity"

edu_g <- user_data[,c(3:12,119)] %>% group_by(Edu_g) %>% 
  summarise_at(.vars = names(.)[1:10],
               .funs = c(mean="mean")) 
edu_g <- data.frame(edu_g[,1],t(apply(-edu_g[,c(2:11)], 1, rank, ties.method='min')))
names(edu_g)[1] <- "value"
edu_g$group <- "Education"

gend_g <- user_data[,c(3:12,120)] %>% group_by(gender_g) %>% 
  summarise_at(.vars = names(.)[1:10],
               .funs = c(mean="mean")) 
gend_g <- data.frame(gend_g[,1],t(apply(-gend_g[,c(2:11)], 1, rank, ties.method='min')))
names(gend_g)[1] <- "value"
gend_g$group <- "Gender"

value_summary <- rbind(gend_g,age_g,eth_g,edu_g,income_g)

value_summary <- value_summary[which(!value_summary$value %in% c("Prefer not to answer",NA)),]
value_summary2  <- t(value_summary)
value_summary2 <- data.frame(row.names(value_summary2),value_summary2)
names(value_summary2) <- value_summary2[1,]; 
value_summary2 <- value_summary2[-c(1,12),];row.names(value_summary2) <- 1:dim(value_summary2)[1]
value_summary2$value <- gsub("score_mean","",value_summary2$value); colnames(value_summary2)[1] <- ""

###############################################################################################
###############################################################################################




# Value mean score of all population
colMeans(pvq.users.data[,c(3:12)])
mean_score <- as.data.frame(colMeans(pvq.users.data[,c(3:12)]))
colnames(mean_score) <- c("mean")
sd_score <- as.data.frame(apply(pvq.users.data[,c(3:12)],2,sd))
colnames(sd_score) <- c("sd")


# Value mean score of different demographic groups
## Gender
#options(digits=3)
#hv_gender <- round(hv_gender[,c(2:11)], digits = 2)
#hv_gender <- rename(hv_gender, "gender"="Gender_Select")
unique(user_data$Gender_Select)
hv_gender <- user_data %>% 
  group_by(Gender_Select) %>%
  summarise(ACHIEVEMENT = mean(ACHIEVEMENTscore, na.rm = TRUE), BENEVOLENCE = mean(BENEVOLENCEscore, na.rm = TRUE), CONFORMITY = mean(CONFORMITYscore, na.rm = TRUE), HEDONISM = mean(HEDONISMscore, na.rm = TRUE), POWER = mean(POWERscore, na.rm = TRUE), SECURITY = mean(SECURITYscore, na.rm = TRUE), SELF_DIRECTION = mean(SELF.DIRECTIONscore, na.rm = TRUE), STIMULATION = mean(STIMULATIONscore, na.rm = TRUE), TRADITION = mean(TRADITIONscore, na.rm = TRUE), UNIVERSALISM = mean(UNIVERSALISMscore, na.rm = TRUE))
hv_gender <- as.data.frame(hv_gender)
hv_gender_t <- t(hv_gender)
gender <- user_data %>% group_by(Gender_Select) %>% summarise(gender=n())
## Age
# user_data <- user_data %>%
#   mutate(Age_g=ifelse(Age<40, "Young", 
#                       ifelse(Age<66, "Middle", "Old")))


hv_age <- user_data %>%
  group_by(Age_g) %>%
  summarise(ACHIEVEMENT = mean(ACHIEVEMENTscore, na.rm = TRUE), BENEVOLENCE = mean(BENEVOLENCEscore, na.rm = TRUE), CONFORMITY = mean(CONFORMITYscore, na.rm = TRUE), HEDONISM = mean(HEDONISMscore, na.rm = TRUE), POWER = mean(POWERscore, na.rm = TRUE), SECURITY = mean(SECURITYscore, na.rm = TRUE), SELF_DIRECTION = mean(SELF.DIRECTIONscore, na.rm = TRUE), STIMULATION = mean(STIMULATIONscore, na.rm = TRUE), TRADITION = mean(TRADITIONscore, na.rm = TRUE), UNIVERSALISM = mean(UNIVERSALISMscore, na.rm = TRUE))%>%
  filter(Age_g != "NA")
hv_age_t <- t(hv_age)
age_n <- user_data %>% group_by(Age_g) %>% summarise(age=n()) #number of respondents in each age group
## Income level

hv_income <- user_data %>% filter(Income !="NA", Income !="Prefer not to answer")
low <- c("Less than $1,000","$1,000 to $1,999","$2,000 to $2,999" )
middle <- c("$3,000 to $3,999", "$4,000 to $4,999")
hv_income <- hv_income %>% mutate(Income_g=ifelse(Income %in% low, "Low",
                                                  ifelse(Income %in% middle,"Middle","High")))
                                                         

#unique(user_data$Income)
#hv_income <- hv_income %>% mutate(Income_g=ifelse(Income == c("Less than $1,000","$1,000 to $1,999","$2,000 to $2,999"), "Low",
#                                                  ifelse(Income == c("$3,000 to $3,999","$4,000 to $4,999"),"Middle","High")))

unique(hv_income$Income_g)
hv_income_s <- hv_income %>%
  #filter(Income !="NA", Income !="Prefer not to answer")%>%
  group_by(Income_g) %>% 
  summarise(ACHIEVEMENT = mean(ACHIEVEMENTscore, na.rm = TRUE), BENEVOLENCE = mean(BENEVOLENCEscore, na.rm = TRUE), CONFORMITY = mean(CONFORMITYscore, na.rm = TRUE), HEDONISM = mean(HEDONISMscore, na.rm = TRUE), POWER = mean(POWERscore, na.rm = TRUE), SECURITY = mean(SECURITYscore, na.rm = TRUE), SELF_DIRECTION = mean(SELF.DIRECTIONscore, na.rm = TRUE), STIMULATION = mean(STIMULATIONscore, na.rm = TRUE), TRADITION = mean(TRADITIONscore, na.rm = TRUE), UNIVERSALISM = mean(UNIVERSALISMscore, na.rm = TRUE))
hv_income_t <- t(hv_income_s)
income <- hv_income %>% group_by(Income_g) %>% summarise(income=n())
income_detail <- user_data %>% group_by(Income) %>% summarise(income=n())
## Ethnicity
#user_data %>% group_by(Ethnicity_Select) %>% tally() %>% arrange(desc(n)) 
hv_eth <- user_data %>% mutate(Ethnicity_Select_g=ifelse(Ethnicity_Select == "White (A person having origins in any of the original peoples of Europe, the Middle East, or North Africa.)","White",
                                                     ifelse(Ethnicity_Select == "Asian (A person having origins in any of the original peoples of the Far East, Southeast Asia, or the Indian subcontinent)","Asian",
                                                            ifelse(Ethnicity_Select == "Black (A person having origins in any of the black racial groups of Africa.)","Black", 
                                                                   ifelse(Ethnicity_Select == "Hispanic/Latino/Spanish origin (A person of Cuban, Mexican, Puerto Rican, South or Central American, or other Spanish culture or origin, regardless of race)","Hispanic", 
                                                                          ifelse(grepl("Prefer", Ethnicity_Select),"Prefer not to answer", "Multiple Ethnicity/Other"))))))

hv_eth <- hv_eth %>% filter(Ethnicity_Select_g %in% c("White","Asian","Black","Hispanic","Multiple Ethnicity/Other"))
# hv_eth <- hv_eth %>% mutate(Ethnicity_Select_g=ifelse(Ethnicity_Select == "White (A person having origins in any of the original peoples of Europe, the Middle East, or North Africa.)","White",
#                                                       ifelse(Ethnicity_Select == "Asian (A person having origins in any of the original peoples of the Far East, Southeast Asia, or the Indian subcontinent)","Asian",
#                                                              ifelse(Ethnicity_Select == "Black (A person having origins in any of the black racial groups of Africa.)","Black", "Hispanic"
#                                                              ))))
hv_eth_s <- hv_eth %>%
  filter(Ethnicity_Select_g %in% c("White","Asian","Black","Hispanic") ) %>% 
  group_by(Ethnicity_Select_g) %>%
  summarise(ACHIEVEMENT = mean(ACHIEVEMENTscore, na.rm = TRUE), BENEVOLENCE = mean(BENEVOLENCEscore, na.rm = TRUE), CONFORMITY = mean(CONFORMITYscore, na.rm = TRUE), HEDONISM = mean(HEDONISMscore, na.rm = TRUE), POWER = mean(POWERscore, na.rm = TRUE), SECURITY = mean(SECURITYscore, na.rm = TRUE), SELF_DIRECTION = mean(SELF.DIRECTIONscore, na.rm = TRUE), STIMULATION = mean(STIMULATIONscore, na.rm = TRUE), TRADITION = mean(TRADITIONscore, na.rm = TRUE), UNIVERSALISM = mean(UNIVERSALISMscore, na.rm = TRUE))

hv_eth_t <- t(hv_eth_s)
hv_eth_t <- hv_eth_t[-1,]
ethnicity <- hv_eth %>% group_by(Ethnicity_Select_g) %>% summarise(eth=n())
## Education level
#unique(user_data$SchoolHighestEdu)

hv_edu <- user_data %>% filter(SchoolHighestEdu !="NA")
LessThanHighSchool <- c("Early childhood Education","Primary education")
HighSchoolEducation <- c("Upper secondary education", "Lower secondary education")
VocEducation <- c("Post-secondary non-tertiary education", "Short-cycle tertiary education, e.g. vocational program")
CollegeEducation <- c("Bachelor's or equivalent")
AdvancedDegree <- c("Doctoral or equivalent","Master's or equivalent")

hv_edu <- hv_edu %>% mutate(Edu_g=ifelse(SchoolHighestEdu %in% LessThanHighSchool,"LessThanHighSchool",
                                         ifelse(SchoolHighestEdu %in% HighSchoolEducation,"HighSchoolEducation",
                                                ifelse(SchoolHighestEdu %in% VocEducation, "VocEducation",
                                                ifelse(SchoolHighestEdu %in% CollegeEducation,"CollegeEducation",
                                                       ifelse(SchoolHighestEdu %in% AdvancedDegree,"AdvancedDegree",
                                                              ifelse(SchoolHighestEdu %in% c("Prefer not to answer"),"Prefer not to answer",NA)))))))


hv_edu <- hv_edu %>% filter(Edu_g %in% c("LessThanHighSchool","HighSchoolEducation","VocEducation","CollegeEducation","AdvancedDegree"))

hv_edu_s <- hv_edu %>%
  group_by(Edu_g) %>%
  summarise(ACHIEVEMENT = mean(ACHIEVEMENTscore, na.rm = TRUE), BENEVOLENCE = mean(BENEVOLENCEscore, na.rm = TRUE), CONFORMITY = mean(CONFORMITYscore, na.rm = TRUE), HEDONISM = mean(HEDONISMscore, na.rm = TRUE), POWER = mean(POWERscore, na.rm = TRUE), SECURITY = mean(SECURITYscore, na.rm = TRUE), SELF_DIRECTION = mean(SELF.DIRECTIONscore, na.rm = TRUE), STIMULATION = mean(STIMULATIONscore, na.rm = TRUE), TRADITION = mean(TRADITIONscore, na.rm = TRUE), UNIVERSALISM = mean(UNIVERSALISMscore, na.rm = TRUE))
hv_edu_t <- t(hv_edu_s)
education <- hv_edu %>% group_by(Edu_g) %>% summarise(education=n())

# ANOVA
## Gender
#gender_aov <- user_data %>% filter(Gender_Select %in% c("Female (including transgender women)", "Male (including transgender men)"))
#add non-binary to anova
gender_aov <- user_data %>% filter(Gender_Select %in% c("Female (including transgender women)", "Male (including transgender men)","Prefer to self describe as ____________ (non-binary, gender-fluid, agender, please specify)"))
#unique(gender_aov$Gender_Select)
gender_aov$Gender_Select <- replace(gender_aov$Gender_Select,gender_aov$Gender_Select=="Female (including transgender women)", "Female") 
gender_aov$Gender_Select <- replace(gender_aov$Gender_Select,gender_aov$Gender_Select=="Male (including transgender men)", "Male")
gender_aov$Gender_Select <- replace(gender_aov$Gender_Select,gender_aov$Gender_Select=="Prefer to self describe as ____________ (non-binary, gender-fluid, agender, please specify)", "Other")
aov_gen_hed <- aov(HEDONISMscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_hed)
aov_gen_sel <- aov(SELF.DIRECTIONscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_sel)
aov_gen_sti <- aov(STIMULATIONscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_sti)
aov_gen_sec <- aov(SECURITYscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_sec)
aov_gen_con <- aov(CONFORMITYscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_con)
aov_gen_tra <- aov(TRADITIONscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_tra)
aov_gen_ach <- aov(ACHIEVEMENTscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_ach)
aov_gen_pow <- aov(POWERscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_pow)
aov_gen_uni <- aov(UNIVERSALISMscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_uni)
aov_gen_ben <- aov(BENEVOLENCEscore ~ Gender_Select, data = gender_aov)
summary(aov_gen_ben)

#GENDER: Mean value scores & Demographic var anova, Tukey test

aov_mean <- data.frame()
tukey_mean <- data.frame()
#g <- broom::tidy(aov(gender_aov[,x] ~ Gender_Select, data = gender_aov))
#g <- gh[which(gh$term != "Residuals"),]
#aov_analysis <- user_data[,c(106:119)] 

for(x in 76:85)
{
  
  values <- names(gender_aov[x])
  #tidy will summarise and return neat format
  aov2 <- broom::tidy(aov(gender_aov[,x] ~ Gender_Select, data = gender_aov))
  aov2 <- cbind(aov2,values)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(aov2$p.value[1] < 0.005) {
  #print(column)
  #  print(aov2)
  #}
  aov_mean <- rbind(aov_mean,aov2)
}

#aov_mean <- aov_mean[which(aov_mean$term != "Residuals"),]

for (x in 76:85){
  column <- names(gender_aov[x])
  mean <- aov(gender_aov[,x] ~ Gender_Select, data = gender_aov)
  tukey2 <- TukeyHSD(mean) 
  tukey2 <- tidy(tukey2) # CONVERT DATA FROM LIST TO DATAFRAME
  tukey2 <- cbind(tukey2,column) # ADD THE NAME OF THE COLUMN 
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_mean <- rbind(tukey_mean,tukey2)
}

## Age
age_aov <- user_data %>% filter(Age_g !="NA")
aov_age_hed <- aov(HEDONISMscore ~ Age_g, data = age_aov)
summary(aov_age_hed)
aov_age_sel <- aov(SELF.DIRECTIONscore ~ Age_g, data = age_aov)
summary(aov_age_sel)
aov_age_sti <- aov(STIMULATIONscore ~ Age_g, data = age_aov)
summary(aov_age_sti)
aov_age_sec <- aov(SECURITYscore ~ Age_g, data = age_aov)
summary(aov_age_sec)
aov_age_con <- aov(CONFORMITYscore ~ Age_g, data = age_aov)
summary(aov_age_con)
aov_age_tra <- aov(TRADITIONscore ~ Age_g, data = age_aov)
summary(aov_age_tra)
aov_age_ach <- aov(ACHIEVEMENTscore ~ Age_g, data = age_aov)
summary(aov_age_ach)
aov_age_pow <- aov(POWERscore ~ Age_g, data = age_aov)
summary(aov_age_pow)
aov_age_uni <- aov(UNIVERSALISMscore ~ Age_g, data = age_aov)
summary(aov_age_uni)
aov_age_ben <- aov(BENEVOLENCEscore ~ Age_g, data = age_aov)
summary(aov_age_ben)
unique(age_aov$Age_g)

#AGE: Mean value scores & Demographic var anova, Tukey test

for(x in 76:85)
{
  
  values <- names(age_aov[x])
  #tidy will summarise and return neat format
  aov2 <- broom::tidy(aov(age_aov[,x] ~ Age_g, data = age_aov))
  aov2 <- cbind(aov2,values)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(aov2$p.value[1] < 0.005) {
  #print(column)
  #  print(aov2)
  #}
  aov_mean <- rbind(aov_mean,aov2)
}

#aov_mean <- aov_mean[which(aov_mean$term != "Residuals"),]

for (x in 76:85){
  column <- names(age_aov[x])
  mean <- aov(age_aov[,x] ~ Age_g, data = age_aov)
  tukey2 <- TukeyHSD(mean) 
  tukey2 <- tidy(tukey2) # CONVERT DATA FROM LIST TO DATAFRAME
  tukey2 <- cbind(tukey2,column) # ADD THE NAME OF THE COLUMN 
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_mean <- rbind(tukey_mean,tukey2)
}

## Ethnicity
aov_eth_hed <- aov(HEDONISMscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_hed)
aov_eth_sel <- aov(SELF.DIRECTIONscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_sel)
aov_eth_sti <- aov(STIMULATIONscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_sti)
aov_eth_sec <- aov(SECURITYscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_sec)
aov_eth_con <- aov(CONFORMITYscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_con)
aov_eth_tra <- aov(TRADITIONscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_tra)
aov_eth_ach <- aov(ACHIEVEMENTscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_ach)
aov_eth_pow <- aov(POWERscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_pow)
aov_eth_uni <- aov(UNIVERSALISMscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_uni)
aov_eth_ben <- aov(BENEVOLENCEscore ~ Ethnicity_Select_g, data = hv_eth)
summary(aov_eth_ben)
unique(hv_eth$Ethnicity_Select_g)

#ETHNICITY: Mean value scores & Demographic var anova, Tukey test

for(x in 76:85)
{
  
  values <- names(hv_eth[x])
  #tidy will summarise and return neat format
  aov2 <- broom::tidy(aov(hv_eth[,x] ~ Ethnicity_Select_g, data = hv_eth))
  aov2 <- cbind(aov2,values)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(aov2$p.value[1] < 0.005) {
  #print(column)
  #  print(aov2)
  #}
  aov_mean <- rbind(aov_mean,aov2)
}

#aov_mean <- aov_mean[which(aov_mean$term != "Residuals"),]

for (x in 76:85){
  column <- names(hv_eth[x])
  mean <- aov(hv_eth[,x] ~ Ethnicity_Select_g, data = hv_eth)
  tukey2 <- TukeyHSD(mean) 
  tukey2 <- tidy(tukey2) # CONVERT DATA FROM LIST TO DATAFRAME
  tukey2 <- cbind(tukey2,column) # ADD THE NAME OF THE COLUMN 
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_mean <- rbind(tukey_mean,tukey2)
}

## Education level
aov_edu_hed <- aov(HEDONISMscore ~ Edu_g, data = hv_edu)
summary(aov_edu_hed)
aov_edu_sel <- aov(SELF.DIRECTIONscore ~ Edu_g, data = hv_edu)
summary(aov_edu_sel)
aov_edu_sti <- aov(STIMULATIONscore ~ Edu_g, data = hv_edu)
summary(aov_edu_sti)
aov_edu_sec <- aov(SECURITYscore ~ Edu_g, data = hv_edu)
summary(aov_edu_sec)
aov_edu_con <- aov(CONFORMITYscore ~ Edu_g, data = hv_edu)
summary(aov_edu_con)
aov_edu_tra <- aov(TRADITIONscore ~ Edu_g, data = hv_edu)
summary(aov_edu_tra)
aov_edu_ach <- aov(ACHIEVEMENTscore ~ Edu_g, data = hv_edu)
summary(aov_edu_ach)
aov_edu_pow <- aov(POWERscore ~ Edu_g, data = hv_edu)
summary(aov_edu_pow)
aov_edu_uni <- aov(UNIVERSALISMscore ~ Edu_g, data = hv_edu)
summary(aov_edu_uni)
aov_edu_ben <- aov(BENEVOLENCEscore ~ Edu_g, data = hv_edu)
summary(aov_edu_ben)
unique(hv_edu$Edu_g)

#EDUCATION: Mean value scores & Demographic var anova, Tukey test

for(x in 76:85)
{
  
  values <- names(hv_edu[x])
  #tidy will summarise and return neat format
  aov2 <- broom::tidy(aov(hv_edu[,x] ~ Edu_g, data = hv_edu))
  aov2 <- cbind(aov2,values)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(aov2$p.value[1] < 0.005) {
  #print(column)
  #  print(aov2)
  #}
  aov_mean <- rbind(aov_mean,aov2)
}

#aov_mean <- aov_mean[which(aov_mean$term != "Residuals"),]

for (x in 76:85){
  column <- names(hv_edu[x])
  mean <- aov(hv_edu[,x] ~ Edu_g, data = hv_edu)
  tukey2 <- TukeyHSD(mean) 
  tukey2 <- tidy(tukey2) # CONVERT DATA FROM LIST TO DATAFRAME
  tukey2 <- cbind(tukey2,column) # ADD THE NAME OF THE COLUMN 
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_mean <- rbind(tukey_mean,tukey2)
}

## Income level
income_aov <- hv_income
aov_inc_hed <- aov(HEDONISMscore ~ Income_g, data = income_aov)
summary(aov_inc_hed)
aov_inc_sel <- aov(SELF.DIRECTIONscore ~ Income_g, data = income_aov)
summary(aov_inc_sel)
aov_inc_STI <- aov(STIMULATIONscore ~ Income_g, data = income_aov)
summary(aov_inc_STI)
aov_inc_SEC <- aov(SECURITYscore ~ Income_g, data = income_aov)
summary(aov_inc_SEC)
aov_inc_CON <- aov(CONFORMITYscore ~ Income_g, data = income_aov)
summary(aov_inc_CON)
aov_inc_TRA <- aov(TRADITIONscore ~ Income_g, data = income_aov)
summary(aov_inc_TRA)
aov_inc_ACH <- aov(ACHIEVEMENTscore ~ Income_g, data = income_aov)
summary(aov_inc_ACH)
aov_inc_POW <- aov(POWERscore ~ Income_g, data = income_aov)
summary(aov_inc_POW)
aov_inc_UNI <- aov(UNIVERSALISMscore ~ Income_g, data = income_aov)
summary(aov_inc_UNI)
aov_inc_BEN <- aov(BENEVOLENCEscore ~ Income_g, data = income_aov)
summary(aov_inc_BEN)
unique(hv_income$Income_g)

#INCOME: Mean value scores & Demographic var anova, Tukey test

for(x in 76:85)
{
  
  values <- names(income_aov[x])
  #tidy will summarise and return neat format
  aov2 <- broom::tidy(aov(income_aov[,x] ~ Income_g, data = income_aov))
  aov2 <- cbind(aov2,values)
  # Add this condition if you only want aov with P < 0.05 printed
  #if(aov2$p.value[1] < 0.005) {
  #print(column)
  #  print(aov2)
  #}
  aov_mean <- rbind(aov_mean,aov2)
}

aov_mean <- aov_mean[which(aov_mean$term != "Residuals"),]
aov_rep <- aov_mean %>% filter(p.value<0.005)

for (x in 76:85){
  column <- names(income_aov[x])
  mean <- aov(income_aov[,x] ~ Income_g, data = income_aov)
  tukey2 <- TukeyHSD(mean) 
  tukey2 <- tidy(tukey2) # CONVERT DATA FROM LIST TO DATAFRAME
  tukey2 <- cbind(tukey2,column) # ADD THE NAME OF THE COLUMN 
  # only want tukey with P < 0.07 printed
  # if(tukey[["p adj"]] < 0.07) {
  # print(column)
  # print(tukey)
  tukey_mean <- rbind(tukey_mean,tukey2)
}

tukey_rep <- tukey_mean %>% filter(adj.p.value<0.001)

#write.xlsx(aov_mean, "aov_mean.xlsx", rowNames = FALSE)
#write.xlsx(tukey_mean, "tukey_mean.xlsx", rowNames = FALSE)


# TukeyHSD & Bonferonni
## Gender
TukeyHSD(aov_gen_hed,conf.level=.95)
#PostHocTest(aov_gen_hed, method='bonferroni')
TukeyHSD(aov_gen_sel,conf.level=.95)
#PostHocTest(aov_gen_sel, method='bonferroni')
TukeyHSD(aov_gen_sti,conf.level=.95)
#PostHocTest(aov_gen_sti, method='bonferroni')
TukeyHSD(aov_gen_sec,conf.level=.95)
#PostHocTest(aov_gen_sec, method='bonferroni')
TukeyHSD(aov_gen_con,conf.level=.95)
#PostHocTest(aov_gen_con, method='bonferroni')
TukeyHSD(aov_gen_tra,conf.level=.95)
#PostHocTest(aov_gen_tra, method='bonferroni')
TukeyHSD(aov_gen_ach,conf.level=.95)
#PostHocTest(aov_gen_ach, method='bonferroni')
TukeyHSD(aov_gen_pow,conf.level=.95)
#PostHocTest(aov_gen_pow, method='bonferroni')
TukeyHSD(aov_gen_uni,conf.level=.95)
#PostHocTest(aov_gen_uni, method='bonferroni')
TukeyHSD(aov_gen_ben,conf.level=.95)
#PostHocTest(aov_gen_ben, method='bonferroni')
#Age
TukeyHSD(aov_age_hed,conf.level=.95)
#PostHocTest(aov_age_hed, method='bonferroni', conf.level=.995)

TukeyHSD(aov_age_sel,conf.level=.95)
#PostHocTest(aov_age_sel, method='bonferroni')

TukeyHSD(aov_age_sti,conf.level=.95)
#PostHocTest(aov_age_sti, method='bonferroni')

TukeyHSD(aov_age_sec,conf.level=.95)
#PostHocTest(aov_age_sec, method='bonferroni')

TukeyHSD(aov_age_con,conf.level=.95)
#PostHocTest(aov_age_con, method='bonferroni')

TukeyHSD(aov_age_tra,conf.level=.95)
#PostHocTest(aov_age_tra, method='bonferroni')

TukeyHSD(aov_age_ach,conf.level=.95)
#PostHocTest(aov_age_ach, method='bonferroni')

TukeyHSD(aov_age_pow,conf.level=.95)
#PostHocTest(aov_age_pow, method='bonferroni')

TukeyHSD(aov_age_uni,conf.level=.95)
#PostHocTest(aov_age_uni, method='bonferroni')

TukeyHSD(aov_age_ben,conf.level=.95)
#PostHocTest(aov_age_ben, method='bonferroni')

## Ethnicity
TukeyHSD(aov_eth_hed,conf.level=.95)
#PostHocTest(aov_eth_hed, method='bonferroni')
TukeyHSD(aov_eth_sel,conf.level=.95)
#PostHocTest(aov_eth_sel, method='bonferroni')
TukeyHSD(aov_eth_sti,conf.level=.95)
#PostHocTest(aov_eth_sti, method='bonferroni')
TukeyHSD(aov_eth_sec,conf.level=.95)
#PostHocTest(aov_eth_sec, method='bonferroni')
TukeyHSD(aov_eth_con,conf.level=.95)
#PostHocTest(aov_eth_con, method='bonferroni')
TukeyHSD(aov_eth_tra,conf.level=.95)
#PostHocTest(aov_eth_tra, method='bonferroni')
TukeyHSD(aov_eth_ach,conf.level=.95)
#PostHocTest(aov_eth_ach, method='bonferroni')
TukeyHSD(aov_eth_pow,conf.level=.95)
#PostHocTest(aov_eth_pow, method='bonferroni')
TukeyHSD(aov_eth_uni,conf.level=.95)
#PostHocTest(aov_eth_uni, method='bonferroni')
TukeyHSD(aov_eth_ben,conf.level=.95)
#PostHocTest(aov_eth_ben, method='bonferroni')

#Education level
TukeyHSD(aov_edu_hed,conf.level=.95)
#PostHocTest(aov_edu_hed, method='bonferroni')

TukeyHSD(aov_edu_sel,conf.level=.95)
#PostHocTest(aov_edu_sel, method='bonferroni')

TukeyHSD(aov_edu_sti,conf.level=.95)
#PostHocTest(aov_edu_sti, method='bonferroni')

TukeyHSD(aov_edu_sec,conf.level=.95)
#PostHocTest(aov_edu_sec, method='bonferroni')

TukeyHSD(aov_edu_con,conf.level=.95)
#PostHocTest(aov_edu_con, method='bonferroni')

TukeyHSD(aov_edu_tra,conf.level=.95)
#PostHocTest(aov_edu_tra, method='bonferroni')

TukeyHSD(aov_edu_ach,conf.level=.95)
#PostHocTest(aov_edu_ach, method='bonferroni')

TukeyHSD(aov_edu_pow,conf.level=.95)
#PostHocTest(aov_edu_pow, method='bonferroni')

TukeyHSD(aov_edu_uni,conf.level=.95)
#PostHocTest(aov_edu_uni, method='bonferroni')

TukeyHSD(aov_edu_ben,conf.level=.95)
#PostHocTest(aov_edu_ben, method='bonferroni')

# Income level
TukeyHSD(aov_inc_hed,conf.level=.95)
#PostHocTest(aov_inc_hed, method='bonferroni')

TukeyHSD(aov_inc_sel,conf.level=.95)
#PostHocTest(aov_inc_sel, method='bonferroni')

TukeyHSD(aov_inc_STI,conf.level=.95)
#PostHocTest(aov_inc_STI, method='bonferroni')

TukeyHSD(aov_inc_SEC,conf.level=.95)
#PostHocTest(aov_inc_SEC, method='bonferroni')

TukeyHSD(aov_inc_CON,conf.level=.95)
#PostHocTest(aov_inc_CON, method='bonferroni')

TukeyHSD(aov_inc_TRA,conf.level=.95)
#PostHocTest(aov_inc_TRA, method='bonferroni')

TukeyHSD(aov_inc_ACH,conf.level=.95)
#PostHocTest(aov_inc_ACH, method='bonferroni')

TukeyHSD(aov_inc_POW,conf.level=.95)
#PostHocTest(aov_inc_POW, method='bonferroni')

TukeyHSD(aov_inc_UNI,conf.level=.95)
#PostHocTest(aov_inc_UNI, method='bonferroni')

TukeyHSD(aov_inc_BEN,conf.level=.95)
#PostHocTest(aov_inc_BEN, method='bonferroni')

# ANOVA assumption test 
## Normality test
## QQ plot draws the correlation between a given data and the normal distribution.
## In all plots, points are close to the reference line -> we can assume normality of data
## Gender
ggqqplot(gender_aov, "POWERscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "ACHIEVEMENTscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "HEDONISMscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "STIMULATIONscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "SELF.DIRECTIONscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "UNIVERSALISMscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "BENEVOLENCEscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "TRADITIONscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "CONFORMITYscore", facet.by = "Gender_Select")
ggqqplot(gender_aov, "SECURITYscore", facet.by = "Gender_Select")

## Age
ggqqplot(age_aov, "POWERscore", facet.by = "Age_g")
ggqqplot(age_aov, "ACHIEVEMENTscore", facet.by = "Age_g")
ggqqplot(age_aov, "HEDONISMscore", facet.by = "Age_g")
ggqqplot(age_aov, "STIMULATIONscore", facet.by = "Age_g")
ggqqplot(age_aov, "SELF.DIRECTIONscore", facet.by = "Age_g")
ggqqplot(age_aov, "UNIVERSALISMscore", facet.by = "Age_g")
ggqqplot(age_aov, "BENEVOLENCEscore", facet.by = "Age_g")
ggqqplot(age_aov, "TRADITIONscore", facet.by = "Age_g")
ggqqplot(age_aov, "CONFORMITYscore", facet.by = "Age_g")
ggqqplot(age_aov, "SECURITYscore", facet.by = "Age_g")

## Ethnicity
ggqqplot(hv_eth, "POWERscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "ACHIEVEMENTscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "HEDONISMscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "STIMULATIONscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "SELF.DIRECTIONscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "UNIVERSALISMscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "BENEVOLENCEscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "TRADITIONscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "CONFORMITYscore", facet.by = "Ethnicity_Select_g")
ggqqplot(hv_eth, "SECURITYscore", facet.by = "Ethnicity_Select_g")

## Education level
ggqqplot(hv_edu, "POWERscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "ACHIEVEMENTscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "HEDONISMscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "STIMULATIONscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "SELF.DIRECTIONscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "UNIVERSALISMscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "BENEVOLENCEscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "TRADITIONscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "CONFORMITYscore", facet.by = "Edu_g")
ggqqplot(hv_edu, "SECURITYscore", facet.by = "Edu_g")

## Income level
ggqqplot(income_aov, "POWERscore", facet.by = "Income_g")
ggqqplot(income_aov, "ACHIEVEMENTscore", facet.by = "Income_g")
ggqqplot(income_aov, "HEDONISMscore", facet.by = "Income_g")
ggqqplot(income_aov, "STIMULATIONscore", facet.by = "Income_g")
ggqqplot(income_aov, "SELF.DIRECTIONscore", facet.by = "Income_g")
ggqqplot(income_aov, "UNIVERSALISMscore", facet.by = "Income_g")
ggqqplot(income_aov, "BENEVOLENCEscore", facet.by = "Income_g")
ggqqplot(income_aov, "TRADITIONscore", facet.by = "Income_g")
ggqqplot(income_aov, "CONFORMITYscore", facet.by = "Income_g")
ggqqplot(income_aov, "SECURITYscore", facet.by = "Income_g")

# Homogeneity of variance 
## If p value is larger than 0.05, there's no significant difference between variances across groups-> homogeneity of variance assumption turned out to be fine: the Levene test is not significant.)
## Gender
leveneTest(POWERscore ~ Gender_Select, gender_aov) #***
leveneTest(ACHIEVEMENTscore ~ Gender_Select, gender_aov) #***
leveneTest(HEDONISMscore ~ Gender_Select, gender_aov) #***
leveneTest(STIMULATIONscore ~ Gender_Select, gender_aov) #***
leveneTest(SELF.DIRECTIONscore ~ Gender_Select, gender_aov)
leveneTest(UNIVERSALISMscore ~ Gender_Select, gender_aov) #***
leveneTest(BENEVOLENCEscore ~ Gender_Select, gender_aov) 
leveneTest(TRADITIONscore ~ Gender_Select, gender_aov)
leveneTest(CONFORMITYscore ~ Gender_Select, gender_aov) #*
leveneTest(SECURITYscore ~ Gender_Select, gender_aov) #**

## Age
leveneTest(POWERscore ~ Age_g, age_aov) #**
leveneTest(ACHIEVEMENTscore ~ Age_g, age_aov)
leveneTest(HEDONISMscore ~ Age_g, age_aov) #***
leveneTest(STIMULATIONscore ~ Age_g, age_aov)
leveneTest(SELF.DIRECTIONscore ~ Age_g, age_aov) #***
leveneTest(UNIVERSALISMscore ~ Age_g, age_aov) 
leveneTest(BENEVOLENCEscore ~ Age_g, age_aov)
leveneTest(TRADITIONscore ~ Age_g, age_aov)
leveneTest(CONFORMITYscore ~ Age_g, age_aov)
leveneTest(SECURITYscore ~ Age_g, age_aov)

## Ethnicity
leveneTest(POWERscore ~ Ethnicity_Select_g, hv_eth)
leveneTest(ACHIEVEMENTscore ~ Ethnicity_Select_g, hv_eth) #***
leveneTest(HEDONISMscore ~ Ethnicity_Select_g, hv_eth) #***
leveneTest(STIMULATIONscore ~ Ethnicity_Select_g, hv_eth)
leveneTest(SELF.DIRECTIONscore ~ Ethnicity_Select_g, hv_eth) #**
leveneTest(UNIVERSALISMscore ~ Ethnicity_Select_g, hv_eth) #***
leveneTest(BENEVOLENCEscore ~ Ethnicity_Select_g, hv_eth) #***
leveneTest(TRADITIONscore ~ Ethnicity_Select_g, hv_eth) 
leveneTest(CONFORMITYscore ~ Ethnicity_Select_g, hv_eth) #**
leveneTest(SECURITYscore ~ Ethnicity_Select_g, hv_eth) #***

## Education level
leveneTest(POWERscore ~ Edu_g, hv_edu)
leveneTest(ACHIEVEMENTscore ~ Edu_g, hv_edu)
leveneTest(HEDONISMscore ~ Edu_g, hv_edu)
leveneTest(STIMULATIONscore ~ Edu_g, hv_edu) 
leveneTest(SELF.DIRECTIONscore ~ Edu_g, hv_edu) #**
leveneTest(UNIVERSALISMscore ~ Edu_g, hv_edu) #*
leveneTest(BENEVOLENCEscore ~ Edu_g, hv_edu) #*
leveneTest(TRADITIONscore ~ Edu_g, hv_edu)
leveneTest(CONFORMITYscore ~ Edu_g, hv_edu) #**
leveneTest(SECURITYscore ~ Edu_g, hv_edu) #***

## Income level
leveneTest(POWERscore ~ Income_g, hv_income)
leveneTest(ACHIEVEMENTscore ~ Income_g, hv_income) #*
leveneTest(HEDONISMscore ~ Income_g, hv_income) #**
leveneTest(STIMULATIONscore ~ Income_g, hv_income)
leveneTest(SELF.DIRECTIONscore ~ Income_g, hv_income) #**
leveneTest(UNIVERSALISMscore ~ Income_g, hv_income)
leveneTest(BENEVOLENCEscore ~ Income_g, hv_income)
leveneTest(TRADITIONscore ~ Income_g, hv_income)
leveneTest(CONFORMITYscore ~ Income_g, hv_income)
leveneTest(SECURITYscore ~ Income_g, hv_income) #*

# Residuals versus fits plot: Homogeneity of variances follow-up test
##In the plot above, there is no evident relationships between residuals and fitted values (the mean of each groups), which is good. So, we can assume the homogeneity of variances.
model_1<- lm(POWERscore ~ Gender_Select, gender_aov)
plot(model_1,1)
model_2<- lm(ACHIEVEMENTscore ~ Gender_Select, gender_aov)
plot(model_2,1)
model_3<- lm(HEDONISMscore ~ Gender_Select, gender_aov)
plot(model_3,1)
model_4<- lm(STIMULATIONscore ~ Gender_Select, gender_aov)
plot(model_4,1)
model_5<- lm(UNIVERSALISMscore ~ Gender_Select, gender_aov)
plot(model_5,1)
model_6<- lm(BENEVOLENCEscore ~ Gender_Select, gender_aov)
plot(model_6,1)
model_7<- lm(TRADITIONscore ~ Gender_Select, gender_aov)
plot(model_7,1)
model_8<- lm(CONFORMITYscore ~ Gender_Select, gender_aov)
plot(model_8,1)
model_9<- lm(SECURITYscore ~ Gender_Select, gender_aov)
plot(model_9,1)

model_10<- lm(POWERscore ~ Age_g, age_aov)
plot(model_10,1)
model_11<- lm(HEDONISMscore ~ Age_g, age_aov)
plot(model_11,1)
model_12<- lm(SELF.DIRECTIONscore ~ Age_g, age_aov)
plot(model_12,1)

model_13<- lm(ACHIEVEMENTscore ~ Ethnicity_Select_g, hv_eth)
plot(model_13,1)
model_14<- lm(HEDONISMscore ~ Ethnicity_Select_g, hv_eth)
plot(model_14,1)
model_15<- lm(BENEVOLENCEscore ~ Ethnicity_Select_g, hv_eth)
plot(model_15,1)
model_16<- lm(TRADITIONscore ~ Ethnicity_Select_g, hv_eth)
plot(model_16,1)
model_17<- lm(CONFORMITYscore ~ Ethnicity_Select_g, hv_eth)
plot(model_17,1)
model_18<- lm(SECURITYscore ~ Ethnicity_Select_g, hv_eth)
plot(model_18,1)


model_19<- lm(ACHIEVEMENTscore ~ Ethnicity_Select_g, hv_eth)
plot(model_19,1)
model_20<- lm(BENEVOLENCEscore ~ Ethnicity_Select_g, hv_eth)
plot(model_20,1)
model_21<- lm(SECURITYscore ~ Edu_g, hv_edu)
plot(model_21,1)
model_22<- lm(SELF.DIRECTIONscore ~ Income_g, hv_income)
plot(model_22,1)

model_23<- lm(STIMULATIONscore ~ Edu_g, hv_edu)
plot(model_23,1)

model_24<- lm(SELF.DIRECTIONscore ~ Income_g, hv_income)
plot(model_24,1)
model_25<- lm(TRADITIONscore ~ Income_g, hv_income)
plot(model_25,1)

##Demographics statistics
### Gender
gender_table <- gender_aov %>% 
  dplyr::filter(Gender_Select != "NA") %>%
  dplyr::group_by(Gender_Select) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(gender_table)[1] <- "value"
gender_table$group <- "Gender"

### Age
age_table <- user_data %>% 
  dplyr::filter(Age_g != "NA") %>%
  dplyr::group_by(Age_g) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(age_table)[1] <- "value"
age_table$group <- "Age"

### Edu
edu_table <- hv_edu %>% 
  dplyr::filter(Edu_g != "NA") %>%
  dplyr::group_by(Edu_g) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(edu_table)[1] <- "value"
edu_table$group <- "Education"
                  
### Ethnicity                  
eth_table <- hv_eth %>% 
  dplyr::filter(Ethnicity_Select_g != "NA") %>%
  dplyr::group_by(Ethnicity_Select_g) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(eth_table)[1] <- "value"
eth_table$group <- "Ethnicity"

### Income                 
income_table <- hv_income %>% 
  dplyr::filter(Income_g != "NA") %>%
  dplyr::group_by(Income_g) %>% 
  dplyr::summarize(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency)) %>%
  dplyr::mutate(Percent = round(Frequency / sum(Frequency), digits = 2) * 100)

names(income_table)[1] <- "value"
income_table$group <- "Income"

##Participant demographics
demo_summary <- rbind(gender_table,age_table,eth_table,edu_table,income_table)

#value_summary <- value_summary[which(!value_summary$value %in% c("Prefer not to answer",NA)),]
demo_summary2  <- t(demo_summary)
demo_summary2 <- data.frame(row.names(demo_summary2),demo_summary2)
names(demo_summary2) <- demo_summary2[1,]; 
demo_summary2 <- demo_summary2[-c(1,12),];row.names(demo_summary2) <- 1:dim(demo_summary2)[1]
demo_summary2$value <- gsub("score_mean","",demo_summary2$value); colnames(demo_summary2)[1] <- ""
demo_summary2 <- demo_summary2[-3,]

kbl(demo_summary2,padding = 1,col.names = linebreak(c("","Female","Male", "Other","Middle", "Old", "Young", "White", "Asian", "Multi racial", "Hispanic", "Black", "Adv. Degree", "College", "Voc. Training", "High School", "< High School", "Low", "High", "Middle")), escape = TRUE) %>%
  kable_paper() %>%
  add_header_above(c(" " = 1, "Gender"=3,"Age" = 3, "Race/Ethnicity" = 5,"Education"=5,"Income" = 3)) %>%
  row_spec(0, angle = -50)  %>%
  column_spec (c(1,4,7,12,17), border_right = T) 

