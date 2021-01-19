
# The script we used for cleaning the raw data, before feeding it into the models 
# Code outputs 2 files: one with missing data removed and one with the missing data retained

#####################################################################################
# Data Set Up                                                                       #
#####################################################################################

# Load packages
library(tidyverse)
library(dplyr)
library(readr)

# Import dataset
all_trauma_data<-read_csv("dataset_8.31.18_from_sheet_4_april3.19.csv") # Converted to csv file

# Select relevant columns from original dataset
trauma_data_clean <- all_trauma_data %>% 
                     select(ALIAS_INST_NUM,`trauma center level (0=level1, 1=-level 2)`,
                            `SEX(0=male)`, AGE_IN_YRS, `INJ_TYPE( 0=blunt)`, `day of week`, 
                            PULSE_A, GCS_CALC_A, ISS,`DIS_STATUS(1=died)`, D_DEATH_a, T_DEATH_a, 
                            EDA_TIME_a, RACE, ETHNICITY, `intubat a OR assist ONV`, SYS_BP_A,
                            TRAUMA_NUM,`GROUP dayofweek`, TRALRT1_LO)

# Rename columns
trauma_data_clean_2 <- trauma_data_clean %>% 
                       rename(trauma_center_num=ALIAS_INST_NUM, 
                              trauma_center_level=`trauma center level (0=level1, 1=-level 2)`,
                              sex=`SEX(0=male)`, age=AGE_IN_YRS, injury_type=`INJ_TYPE( 0=blunt)`, 
                              day_of_week=`day of week`, pulse_a = PULSE_A, gcs_calc_a = GCS_CALC_A, 
                              injury_serverity_score = ISS, death = `DIS_STATUS(1=died)`,
                              day_of_death = D_DEATH_a, time_of_death = T_DEATH_a, eda_time = EDA_TIME_a, 
                              race = RACE, ethnicity = ETHNICITY, sys_bp_a = SYS_BP_A,
                              intubat_a_OR_assist_ONV = `intubat a OR assist ONV`,trauma_number=TRAUMA_NUM, 
                              dow_group=`GROUP dayofweek`, tralrt1_LO=TRALRT1_LO)

# Check number of trauma centers
num_trauma_centers <- length(unique(trauma_data_clean_2$trauma_center_num)) # it is 27

#####################################################################################
# Change Numeric Levels to Factors                                                  #
#####################################################################################

trauma_data_clean_2$trauma_center_level[trauma_data_clean_2$trauma_center_level == 0] <- "Level 1"
trauma_data_clean_2$trauma_center_level[trauma_data_clean_2$trauma_center_level == 1] <- "Level 2"

trauma_data_clean_2$death[trauma_data_clean_2$death == 0] <- "Not Dead"
trauma_data_clean_2$death[trauma_data_clean_2$death == 1] <- "Dead"

trauma_data_clean_2$sex[trauma_data_clean_2$sex == 0] <- "Male"
trauma_data_clean_2$sex[trauma_data_clean_2$sex == 1] <- "Female"

#####################################################################################
# Create New Blood Pressure Variable Based on Discussion                            #
#####################################################################################

# Check what class sys_bp_a is
class(trauma_data_clean_2$sys_bp_a) # sys_bp_a is of class numeric

# IF bp_zero = 1 if sys_bp_a=0, and bp_zero=0 otherwise 
trauma_data_clean_2$bp_zero <- ifelse(trauma_data_clean_2$sys_bp_a == 0,1,0)

#####################################################################################
# Create New Pulse Pressure Variable Based on Discussion                            #
#####################################################################################

# Check what class pulse_a is
class(trauma_data_clean_2$pulse_a) # pulse_a is of class character
# Convert pulse_a to integer
trauma_data_clean_2$pulse_a <- as.integer(trauma_data_clean_2$pulse_a) # NAs introduced
# Create two more pulse variables
trauma_data_clean_2$pulse_na <- ifelse(is.na(trauma_data_clean_2$pulse_a),1,0)
trauma_data_clean_2$pulse_zero <- ifelse(trauma_data_clean_2$pulse_a == 0,1,0)

# For Example:
# PULSE_A     PULSE_ZERO    PULSE_NA
# 144         0             0
# 0           1             0
# NA          0             1

#####################################################################################
# Change '<unk>' race & ethnicity to NAs                                            #
#####################################################################################

trauma_data_clean_2$race[trauma_data_clean_2$race == '<unk>'] <- NA
trauma_data_clean_2$ethnicity[trauma_data_clean_2$ethnicity == '<unk>'] <- NA

#####################################################################################
# Adjust Variable Types                                                             #
#####################################################################################

trauma_data_clean_2$trauma_center_level <- as.factor(trauma_data_clean_2$trauma_center_level)
trauma_data_clean_2$sex <- as.factor(trauma_data_clean_2$sex)
trauma_data_clean_2$death <- as.factor(trauma_data_clean_2$death)

# Should be factor for regression model
trauma_data_clean_2$day_of_week <- as.factor(trauma_data_clean_2$day_of_week)

# Day of death was a character type, make it a date
trauma_data_clean_2$day_of_death <- as.Date(trauma_data_clean_2$day_of_death,
                                            format = "%m/%d/%Y")

#####################################################################################
# Create an Updated Variable for Group                                              #
#####################################################################################

# rule: 
# 1: weekday day; 2:weekday night; 3: weekend day; 4: weekend night 
# Monday 07:00 - Saturday 06:59 is a weekday
# Saturday 07:00 - Monday 06:59 Weekend
# Friday night = weekday night; Monday morning = weekend night

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")

trauma_data_clean_2 <- trauma_data_clean_2 %>% mutate(eda_time_rep = eda_time) %>% separate(eda_time_rep, into = c("hour", "minute", "second"))
trauma_data_clean_2$hour <- as.numeric(trauma_data_clean_2$hour)
trauma_data_clean_2 <- trauma_data_clean_2 %>% mutate( updated_group = 
                               ifelse ( (day_of_week %in% c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) & ((hour < 7)), 2, 
                                        ifelse ( (day_of_week %in% c("Sunday", "Monday")) & (((hour < 7))), 4, 
                                                 ifelse ( (day_of_week %in% weekday) & ((hour >= 7) & (hour < 19)), 1,
                                                          ifelse ( (day_of_week %in% weekday) & ((hour >= 19)), 2,
                                                                   ifelse ( (day_of_week %in% weekend) & 
                                                                      ((hour >= 7) & (hour < 19)), 3,4)))))) %>%
                      dplyr::select(-hour, -minute, -second)


#####################################################################################
# Create New Variable: gcs_level                                                    #
#####################################################################################

trauma_data_clean_2 <- trauma_data_clean_2 %>% 
  mutate(gcs_level = ifelse(gcs_calc_a >=3 & gcs_calc_a<=8, "Severe",
                            ifelse(gcs_calc_a >=9 & gcs_calc_a<=12, "Moderate",
                                   ifelse(gcs_calc_a >12, "Minor",NA))))

#####################################################################################
# Remove Missing & Strange Data                                                     #
#####################################################################################

trauma_data_with_missing <- trauma_data_clean_2

# leave in missing GCS
trauma_data_clean_2 <- trauma_data_clean_2 %>% filter(!is.na(injury_serverity_score), !is.na(pulse_a),
                                                      !is.na(sys_bp_a), age < 111) %>% 
                                               dplyr::select(-pulse_na)
#####################################################################################
# Write Output                                                                      #
#####################################################################################
write.csv(trauma_data_clean_2, "UPDATED_2_trauma_data_clean.csv", row.names=FALSE)
write.csv(trauma_data_with_missing, "trauma_data_clean_with_missing.csv", row.names=FALSE)


