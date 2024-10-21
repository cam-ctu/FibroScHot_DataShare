#-------------------------------------------------------------------------------
# FibroScHot baseline data for sharing
#-------------------------------------------------------------------------------

##Import database of eligible individuals downloaded from MACRO on 16th September 2024 (hard locked trial database)
data <- read.csv("data/FibroScHotDataOpenAccessSharing.csv")
#Number of records in data = 4572

##Remove records that exist to record treatment adverse events only. 
#AGE is complete data due to being an eligibility criterion. Participants were eligible if aged 6-14 years of age. Filter database on age.
library(dplyr)
data_1 <- data %>% filter(data$AGE>=6)
#Number of records remaining (one per individual) =735. Corresponds with n=735 eligible recorded in FibroScHot Final Statistics Report.

## Data check ---------------------------------------------------------
###NB on data regarding visit dates
## pCRF and eCRF were designed so that variables labeled VISIT_DT[x] recorded the start date for all activities conducted at that visit e.g. 
      # for baseline this includes ultrasound examination, parasitology and the first treatment
      # for 24-month outcome this includes ultrasound examination, parasitology and the final treatment.

## Baseline examinations in Buhirigi Primary were split over two years (2020 and 2021) due to the COVID-19 pandemic.  
## In order to allow robust tracking of when examinations/sample collections and treatments occurred additional date variables were developed:
      # PARST_ST_DCOLT[x]  - date that stools were collected
## All baseline activities for participants attending Kaiso Primary were conducted in 2021.


# Parasitology checks

###Variable key ##
## PARST_ST_DCOLT[x]
# date stools collected, where:
# - x = stool sample number (starts from 0, x=0 is sample 1)
## PARST_ST_COLCTD[x] = [y]
# tracks stools collected, where:
# - x = stool sample number (starts from 0, x=0 is sample 1)
# - y = 0 is no, 1 is yes
## PARST_KKC[x]_DONE[y] =[z]
# tracks egg counts done, where:
# - x = slide number (starts from 1, x=1 is sample 1)
# - y = stool sample number (starts from _, y=1 is sample 2)
# - z = 0 is no, 1 is yes
## PARST_KKC[X]_CNT[y]
# raw egg count, where:
# - x = slide number (starts from 1, x=1 is slide 1)
# - y = stool sample number (starts from _, y =1 is sample 2)

# Ensure PARST_ST_COLCTD[X] records are present for all individuals with no NA recorded so PARST_ST_COLCTD[x] used to quality check PARST_ST_DCOLT[x].
summary(data_1$PARST_ST_COLCTD)
#14 individuals recorded as NA
#Ensure that for 14 individuals all associated egg count data_1 is NA
all(is.na(c(data_1$PARST_ST_COLCTD[data_1$PARST_KKC1_DONE=="na"], 
            data_1$PARST_ST_COLCTD[data_1$PARST_KKC2_DONE=="na"],
            data_1$PARST_ST_COLCTD[data_1$PARST_KKC1_CNT=="na"],
            data_1$PARST_ST_COLCTD[data_1$PARST_KKC2_CNT=="na"]
            )
            ))
#CODE RETURN = TRUE

summary(data_1$PARST_ST_COLCTD1)
#14 individuals recorded as NA
#Ensure that for 14 individuals all associated egg count data_1 is NA
all(is.na(c(data_1$PARST_ST_COLCTD1[data_1$PARST_KKC1_DONE1=="na"], 
            data_1$PARST_ST_COLCTD1[data_1$PARST_KKC2_DONE1=="na"],
            data_1$PARST_ST_COLCTD1[data_1$PARST_KKC1_CNT1=="na"],
            data_1$PARST_ST_COLCTD1[data_1$PARST_KKC2_CNT1=="na"]
)
))
#CODE RETURN = TRUE

summary(data_1$PARST_ST_COLCTD2)
#57 individuals recorded as NA
#Ensure that for 57 individuals all associated egg count data_1 is NA
all(is.na(c(data_1$PARST_ST_COLCTD2[data_1$PARST_KKC1_DONE2=="na"], 
            data_1$PARST_ST_COLCTD2[data_1$PARST_KKC2_DONE2=="na"],
            data_1$PARST_ST_COLCTD2[data_1$PARST_KKC1_CNT2=="na"],
            data_1$PARST_ST_COLCTD2[data_1$PARST_KKC2_CNT2=="na"]
)
))
#CODE RETURN = TRUE

# Replace data_1$PARST_ST_COLCTD[x]=="na" with ==0
data_1 <- data_1 %>% group_by(SUBJECT_ID_MAIN) %>% mutate_at(vars(PARST_ST_COLCTD, PARST_ST_COLCTD1, PARST_ST_COLCTD2), ~replace(., is.na(.), 0))
summary(data_1$PARST_ST_COLCTD)
summary(data_1$PARST_ST_COLCTD1)
summary(data_1$PARST_ST_COLCTD2)
#No NAs remain for PARST_ST_COLCTD[x]

#Ensure if binary variable indicating stool collected is no, then date of stool collection is NA
all(is.na(
  c(
    data_1$PARST_ST_DCOLT[data_1$PARST_ST_COLCTD == 0],
    data_1$PARST_ST_DCOLT1[data_1$PARST_ST_COLCTD1 == 0],
    data_1$PARST_ST_DCOLT2[data_1$PARST_ST_COLCTD2 == 0]
  )
))
#CODE RETURN = FALSE

# Replace PARST_ST_DT[x] with missing if stool was not collected
data_1 <- data_1  %>% group_by(SUBJECT_ID_MAIN) %>% mutate(PARST_ST_DCOLT = if_else(PARST_ST_COLCTD == 0, NA_character_, PARST_ST_DCOLT))
data_1 <- data_1  %>% group_by(SUBJECT_ID_MAIN) %>% mutate(PARST_ST_DCOLT1 = if_else(PARST_ST_COLCTD1 == 0, NA_character_, PARST_ST_DCOLT1))
data_1 <- data_1  %>% group_by(SUBJECT_ID_MAIN) %>% mutate(PARST_ST_DCOLT2 = if_else(PARST_ST_COLCTD2 == 0, NA_character_, PARST_ST_DCOLT2))

all(is.na(
  c(
    data_1$PARST_ST_DCOLT[data_1$PARST_ST_COLCTD == 0],
    data_1$PARST_ST_DCOLT1[data_1$PARST_ST_COLCTD1 == 0],
    data_1$PARST_ST_DCOLT2[data_1$PARST_ST_COLCTD2 == 0]
  )
))
#CODE RETURN = TRUE


# Ensure if binary variable indicating egg count is done is 0, then egg counts are NA
all(is.na(
  c(
    data_1$PARST_KKC1_CNT[data_1$PARST_KKC1_DONE == 0],
    data_1$PARST_KKC2_CNT[data_1$PARST_KKC2_DONE == 0],
    data_1$PARST_KKC1_CNT1[data_1$PARST_KKC1_DONE1 == 0],
    data_1$PARST_KKC2_CNT1[data_1$PARST_KKC2_DONE1 == 0],
    data_1$PARST_KKC1_CNT2[data_1$PARST_KKC1_DONE2 == 0],
    data_1$PARST_KKC2_CNT2[data_1$PARST_KKC2_DONE2 == 0]
  )
))
#CODE RETURN = TRUE

#Check where 1st stool egg count is recorded for each individual
table(data_1$PARST_ST_COLCTD, data_1$PARST_ST_COLCTD1, data_1$PARST_ST_COLCTD2)
#660 individuals 1st stool collected is recorded as PARST_ST_COLCTD
#39 individuals 1st stool collected is recorded as PARST_ST_COLCTD1
#1 individual 1st stool collected is recorded as PARST_ST_COLCTD2
#35 individuals, egg count data is missing. Corresponds with n=35 missing baseline egg count recorded in FibroScHot Final Statistics Report 

#Year of collection checks. 

#Convert date to format DD/MM/YYYY
library(lubridate)
data_1$PARST_ST_DCOLT <- as.Date(data_1$PARST_ST_DCOLT, format = "%d/%m/%Y")
data_1$PARST_ST_DCOLT1 <- as.Date(data_1$PARST_ST_DCOLT1, format = "%d/%m/%Y")
data_1$PARST_ST_DCOLT2 <- as.Date(data_1$PARST_ST_DCOLT2, format ="%d/%m/%Y")

# Extract year from dates for Buhirigi to account for split baseline
data_1$year_st1[data_1$SCHL_AT_ENROL==1]<- year(data_1$PARST_ST_DCOLT[data_1$SCHL_AT_ENROL==1])
data_1$year_st2[data_1$SCHL_AT_ENROL==1] <- year(data_1$PARST_ST_DCOLT1[data_1$SCHL_AT_ENROL==1])
data_1$year_st3[data_1$SCHL_AT_ENROL==1] <- year(data_1$PARST_ST_DCOLT2[data_1$SCHL_AT_ENROL==1])

#Check for anomalies in year of collection Buhirigi
table(data_1$year_st1[data_1$SCHL_AT_ENROL==1], data_1$year_st2[data_1$SCHL_AT_ENROL==1])
table(data_1$year_st1[data_1$SCHL_AT_ENROL==1], data_1$year_st3[data_1$SCHL_AT_ENROL==1])
table(data_1$year_st2[data_1$SCHL_AT_ENROL==1], data_1$year_st3[data_1$SCHL_AT_ENROL==1])
#None found

#Kaiso examinations all took place in 2021, code year variables as 2021 for Kaiso Participants.

data_1$year_st1[data_1$SCHL_AT_ENROL==2]<- 2021
data_1$year_st2[data_1$SCHL_AT_ENROL==2]<- 2021
data_1$year_st3[data_1$SCHL_AT_ENROL==2]<- 2021


## Data conversions ---------------------------------------------------------

## Create variable for year of examination
data_1$year <- data_1$year_st1
data_1 <- data_1 %>% group_by(SUBJECT_ID_MAIN) %>%
  mutate(year= if_else(is.na(year), year_st2, year), 
         year= if_else(is.na(year), year_st3, year))

#Recode length of residence, as categories 1 and 2 removed via eligibility criterion "residency >= 2-years"
data_1$LEN_RES_HOIMA[data_1$LEN_RES_HOIMA==3]<- 1
data_1$LEN_RES_HOIMA[data_1$LEN_RES_HOIMA==4]<-2

#Create age group variable for 6-8 years, 9-11 years, 12-16 years: 
data_1$AgeGp<-0
data_1$AgeGp[data_1$AGE>8]<- 1
data_1$AgeGp[data_1$AGE>11]<-2


#calculation of infection intensities from 1 stool

data_1 <-data_1 %>%
  group_by(SUBJECT_ID_MAIN) %>%
  mutate(egg_count1st = sum(
    PARST_KKC1_CNT,
    PARST_KKC2_CNT,
    na.rm=TRUE),
    nsamples1st = sum(PARST_KKC1_DONE, PARST_KKC2_DONE, 
                         na.rm=TRUE),
    epg_1st = (egg_count1st / nsamples1st) * 24,

  egg_count1st1 = sum(
    PARST_KKC1_CNT1,
    PARST_KKC2_CNT1,
    na.rm=TRUE),
    nsamples1st1 = sum(PARST_KKC1_DONE1, PARST_KKC2_DONE1, 
                      na.rm=TRUE),
    epg_1st1 = (egg_count1st1 / nsamples1st1) * 24,

  egg_count1st2 = sum(
    PARST_KKC1_CNT2,
    PARST_KKC2_CNT2,
    na.rm=TRUE),
    nsamples1st2 = sum(PARST_KKC1_DONE2, PARST_KKC2_DONE2, 
                       na.rm=TRUE),
    epg_1st2 = (egg_count1st2 / nsamples1st2) * 24)

data_1 <- data_1 %>% group_by(SUBJECT_ID_MAIN) %>% 
  mutate(epg_1st = if_else(is.na(epg_1st), epg_1st1, epg_1st),
         epg_1st = if_else(is.na(epg_1st), epg_1st2, epg_1st))



## Build and download of database for sharing___________________________________

#Select data for sharing
data_2 <- data_1 %>% select (SUBJECT_ID_MAIN, AgeGp, SEX, SCHL_AT_ENROL, LEN_RES_HOIMA, year, epg_1st)

#Filter out participants with no egg count
data_2<- data_2 %>% filter_at(vars(epg_1st), any_vars(!is.na(.)))
#700 individuals with egg count data at baseline.

#convert NaN to NA throughout
data_2 <- data_2 %>% mutate_all(~ifelse(is.nan(.), NA, .))

#Rename columns
data_2 <- setNames(data_2, c("Study_ID", "Age_Group", "Sex", "School", "Residency", "Year_exam", "Egg_Count"))

#Remove participants for whom permission to share data is not held
data_share <- data_2[!(data_2$Study_ID %in% c("S1-246", "S1-267", "S1-334", "S1-346", "S1-468", "S2-553")),]
View(data_share)

#Download database of 7 variables for 694 eligible individuals 
write.csv(data_share, "fibroschot_baseline_share.csv", row.names= FALSE)




