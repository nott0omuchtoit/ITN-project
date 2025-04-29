# 1. Data cleaning

# load libraries
library(tidyverse)
library(lubridate)
library(data.table)
library(Hmisc)
library(readxl)
library(janitor)

# Import datasets - households, referrals, CHWs

households <- fread("C:/Users/user/Desktop/Analytic Engineering Project/AE-Project1/households.csv")
referrals <- fread("C:/Users/user/Desktop/Analytic Engineering Project/AE-Project1/referrals.csv")
chws <- fread("C:/Users/user/Desktop/Analytic Engineering Project/AE-Project1/chws.csv")

# check for missing data
describe(households)
describe(referrals) # missing 1204 redemption dates, 1205 redemption facilities
n_distinct(referrals$household_id) # 2025 distinct household ids

# merge datasets by Household ID and CHW ID
names(households) # variable names - household_id, chw_id, region, visit_date, family_size, has_itn 
names(referrals) # variable names - household_id, chw_id, issue_date, referral_code, redemption_date, redemption_facility
names(chws) # chw_id, regions

# left join() households and referrals to only consider households with a unique household ID and visit date

data_1 <- left_join(households, referrals, by = c('household_id','chw_id'))

# converting missing data to NAs
data_1$redemption_date[data_1$redemption_date == ''] <- NA
data_1$redemption_facility[data_1$redemption_facility == ''] <- NA

# changing data types of date variables
class(data_1$redemption_date) # all date variables are dates 
data_1$visit_date <- ymd(data_1$visit_date)
data_1$issue_date <- ymd(data_1$issue_date)
data_1$redemption_date <- ymd(data_1$redemption_date)

describe(data_1) # 821 have a redemption date
                 # 820 have a redemption facilities

# we will use redemption date as a confirmation of redemption

# Create new variables for referral status and redemption status

# We will use referral code as a confirmation of a referral - 2024 have a referral code
data_1 <- data_1 %>%
  mutate(referral_status = case_when(!is.na(referral_code) ~ 'Yes'), 
         redemption_status = case_when(!is.na(redemption_date) ~ 'Yes'))

# Checking if CHW went to their assigned codes
# CHW010 went to a region that was not assigned - Githurai
# Dagoreti is also spelled wrong - Dagoretti
CHW001 <- data_1 %>%
  filter(chw_id == 'CHW001')

table(CHW001$region)

CHW002 <- data_1 %>%
  filter(chw_id == 'CHW002')
table(CHW002$region)

CHW003 <- data_1 %>%
  filter(chw_id == 'CHW003')
table(CHW003$region)

CHW004 <- data_1 %>%
  filter(chw_id == 'CHW004')
table(CHW004$region)

CHW005 <- data_1 %>%
  filter(chw_id == 'CHW005')
table(CHW005$region)

CHW006 <- data_1 %>%
  filter(chw_id == 'CHW006')
table(CHW006$region)

CHW007<- data_1 %>%
  filter(chw_id == 'CHW007')
table(CHW007$region)

CHW008 <- data_1 %>%
  filter(chw_id == 'CHW008')
table(CHW008$region)

CHW009 <- data_1 %>%
  filter(chw_id == 'CHW009')
table(CHW009$region)

CHW010 <- data_1 %>%
  filter(chw_id == 'CHW010')
table(CHW010$region)

data_2 <- data_1 %>%  # 3175/3176
  filter(region != 'Githurai')

data_2$region[data_2$region == 'Dagoreti'] <- 'Dagoretti'

# Checking changes

table(data_2$region, useNA = 'always')



# Changing NAs to No 

data_2$redemption_status[is.na(data_2$redemption_status)] <- 'No'
data_2$referral_status[is.na(data_2$referral_status)] <- 'No'

# Export data 

write.csv(data_2, 'ITN_data.csv', row.names = F)
