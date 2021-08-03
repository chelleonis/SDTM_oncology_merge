library(haven)
library(tidyverse)
library(dplyr)
library(magrittr)
library(stringr)

#looking for vital signs and pharmacometric information

#file import hidden
#along the lines of read_sas("filename.sas7bdat")

vs_reduce <- vs %>% select(c(USUBJID, VISIT, VSTEST, VSORRES, VSTPT)) %>%
  filter((VSTEST == "Diastolic Blood Pressure" | VSTEST == "Weight" |
            VSTEST == "Systolic Blood Pressure" | VSTEST == "Temperature")) %>% 
  group_by(USUBJID, VISIT, VSTPT, VSTEST) %>%
  mutate(row = row_number()) %>%
  spread(VSTEST, VSORRES) %>%
  rename(TPT = VSTPT) 

vs_reduce_2 <- vs %>% select(c(USUBJID, VISIT, VSTEST, VSORRES, VSTPT)) %>%
  filter((VSTEST == "Diastolic Blood Pressure" | VSTEST == "Weight" |
            VSTEST == "Systolic Blood Pressure" | VSTEST == "Temperature")) %>%
  group_by(USUBJID, VISIT, VSTPT) %>%


weight_test <- vs %>% select(c(USUBJID, VSTEST, VSORRES)) %>%
  filter(VSTEST == "Weight")

ex_mini <- ex %>% select(c(USUBJID, VISIT, EXDOSE, EPOCH))
pc_mini <- pc %>% select(c(USUBJID, VISIT, PCTPT, PCORRES)) %>%
  rename(TPT = PCTPT) 

subj_id <- read.csv("subj_id.csv", fileEncoding="UTF-8-BOM") %>%
  rename(USUBJID = ID) %>%
  mutate(USUBJID = regmatches(USUBJID, regexpr("\\d{6}\\-\\d{3}", USUBJID))) 

PTP_id <- read.csv("PTP_id.csv", fileEncoding="UTF-8-BOM") %>%
  rename(TPT = PTP) %>%
  select(-c("X","X.1")) %>%
  mutate(TPT = sapply(TPT,str_trim,side="right"))

test <- full_join(vs_reduce,ex_mini, by = c("USUBJID","VISIT"))

test2 <- full_join(test,pc_mini, by = c("USUBJID","VISIT","TPT")) %>%
  mutate(USUBJID = regmatches(USUBJID, regexpr("\\d{6}\\-\\d{3}", USUBJID))) %>%
  left_join(subj_id, by = "USUBJID") %>%
  left_join(PTP_id, by = "TPT") %>%
  group_by(USUBJID) %>%
  fill(EPOCH) %>%
  fill(Dose) %>%
  fill(Weight) %>%
  fill(Weight, .direction = "up") %>%
  mutate(part_short = substr(EPOCH,5,6)) %>%
  mutate(part_short = ifelse(part_short == "of", " C", part_short)) %>%
  mutate(PCORRES = ifelse(PCORRES == "BLQ<0.500", 0, PCORRES)) %>%
  mutate(VISIT = ifelse(VISIT == "Screening", "Cycle 1 Day 0", VISIT)) %>%
  filter(!grepl("Rollover",VISIT)) %>%
  filter(!grepl("End of Treatment",VISIT)) %>%
  mutate(cycle_num = 
           as.numeric(str_extract((str_extract(VISIT, "Cycle\\ \\d+")),"\\d+"))) %>%
  mutate(day_num = 
           as.numeric(str_extract((str_extract(VISIT, "Day\\ \\d+")),"\\d+"))) %>%
  mutate(nominal_time_day = ifelse(part_short == " C", 
         21*(cycle_num-1) + day_num-1 + PTP_numday/24, 
         14*pmin(cycle_num-1,1) + 21*pmax(0,cycle_num-2) + day_num-1 + PTP_numday/24)) %>%
  rename(Nominal_Dose = Dose)

#need to deal with end of treatment (currently ommitting) ~200-300 entries

avg_weights <- test2 %>% group_by(USUBJID) %>% 
  summarize(mean_wt = mean(as.numeric(Weight), na.rm = TRUE))

#rollover cycle omitted ~6000 entries
final_dataset <- test2 %>% group_by(USUBJID) %>%
  select(c("USUBJID","part_short","VISIT","TPT","nominal_time_day",
           "Nominal_Dose","PCORRES", "EXDOSE","Weight",
           "Diastolic Blood Pressure", "Systolic Blood Pressure",
           "Temperature")) %>%
  mutate(Weight = as.numeric(Weight)) %>%
  mutate(Nominal_Dose = ifelse(is.na(Nominal_Dose) && !is.na(EXDOSE),
         EXDOSE/Weight , Nominal_Dose)) %>%
  fill(Nominal_Dose)

partA_only <- final_dataset %>%
  filter(part_short == " A")

#export files as csv (export location hidden)
#along the lines of write.csv(final_dataset, filepath)

#dose regime ug/kg = dose/body weight / 5
  
#blood pressure, diastolic & systolic
#temperature (in VSTEST)

#columns needed: ID, Dose time, Dose conc., Dose Information/group (a,b,c),  