library(ggplot2)
library(dplyr)
library(MESS) #AUC data
library("readxl")
library(magrittr)
library(stringr)
library(haven)
library(tidyverse)

#Plot blood pressure (diastolic and systolic) vs time and by dose (done)
#Plot temperature vs time by dose (done)
#Plot nemvaleukin concentration vs time by dose, 
#     matching the same time scale as for blood pressure and body temperature
#Plot blood pressure vs Cmax (using different color for different dose level) to see if there is any correlation 
#Plot temperature vs Cmax (using different color for different dose level) to see if there is any correlation
#Plot blood pressure vs AUC (using different color for different dose level) to see if there is any correlation
#Plot temperature vs AUC (using different color for different dose level) to see if there is any correlation

#load data:

#fixed rounding errors by dose categories:

dose_lvls <- c(0.1,0.3,1,3,6,8,10)

nem_og <-read.csv("data_file.csv",fileEncoding="UTF-8-BOM")

nem <- read.csv("data_file.csv",fileEncoding="UTF-8-BOM") %>% rowwise() %>%
  mutate(Nominal_Dose = dose_lvls[which.min(abs(Nominal_Dose-dose_lvls))]) %>%
  ungroup() %>%
  mutate(nominal_time_day = ifelse(nominal_time_day == -1.0, 0.0, nominal_time_day)) %>%
  mutate(PCORRES = ifelse(PCORRES == "Quantity Not Sufficient", 0.0, PCORRES))  %>%
  mutate(conc_mean = ifelse(PCORRES == "NaN", "NA",PCORRES)) %>%
  mutate(nominal_time_day = ifelse(nominal_time_day == -1.0, 0.0, nominal_time_day))

nem_means <- nem %>% mutate(nominal_time_day = round(nominal_time_day, digits = 1)) %>%
  group_by(nominal_time_day, Nominal_Dose) %>%
  summarise(dia_mean = mean(Diastolic.Blood.Pressure, na.rm = TRUE),
            sys_mean = mean(Systolic.Blood.Pressure, na.rm = TRUE),
            temp_mean = mean(Temperature, na.rm = TRUE),
            conc_mean = mean(PCORRES, na.rm = TRUE)
            )



#vital signs by time/dose

bpdia <-  ggplot(data = nem, aes(x = nominal_time_day, y = Diastolic.Blood.Pressure)) + 
  geom_jitter(data = nem[!is.na(nem$Diastolic.Blood.Pressure),],
            aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
  scale_shape_manual(values = 0:6) +
  theme(legend.position="bottom")
bpdia

bpdia_avg <- ggplot(data = nem_means, aes(x = nominal_time_day,
                                          y= dia_mean)) +
  geom_line(aes(color = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6)
bpdia_avg

bpsys <-  ggplot(data = nem, aes(x = nominal_time_day, y = Systolic.Blood.Pressure)) + 
  geom_jitter(data = nem[!is.na(nem$Systolic.Blood.Pressure),],
              aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6)
bpsys

bpsys_avg <- ggplot(data = nem_means, aes(x = nominal_time_day,
                                          y= sys_mean)) +
  geom_line(aes(color = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
bpsys_avg

temp_plot <-  ggplot(data = nem, aes(x = nominal_time_day, y = Temperature)) + 
  geom_jitter(data = nem[!is.na(nem$Temperature),],
              aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
  scale_shape_manual(values = 0:6) +
  theme(legend.position="bottom")
temp_plot

temp_avg <- ggplot(data = nem_means, aes(x = nominal_time_day,
                                          y= temp_mean)) +
  geom_line(aes(color = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
temp_avg

#conc test

nem_conc <-  ggplot(data = nem, aes(x = nominal_time_day, y = PCORRES)) + 
  geom_point(aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6)
nem_conc

#data=subset(iris, !is.na(Sepal.Length))

nem_conc_avg <- ggplot(data = subset(nem_means, !is.na(conc_mean)), aes(x = nominal_time_day,
                                         y= conc_mean)) +
  geom_line(aes(color = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
nem_conc_avg

ggsave('bpsys.png', plot = bpsys,width = 8.5,height = 5)
ggsave('bpsys_avg.png', plot = bpsys_avg,width = 8.5,height = 5)
ggsave('bpdia.png', plot = bpdia,width = 8.5,height = 5)
ggsave('bpdia_avg.png', plot = bpdia_avg,width = 8.5,height = 5)
ggsave('temp_plot.png', plot = temp_plot,width = 8.5,height = 5)
ggsave('temp_avg.png', plot = temp_avg,width = 8.5,height = 5)
ggsave('nem_conc.png', plot = nem_conc,width = 8.5,height = 5)
ggsave('nem_conc_avg.png', plot = nem_conc_avg,width = 8.5,height = 5)

#AUC stuff

nem_auc <- nem %>% filter(!is.na(PCORRES) == TRUE) %>% 
  #select(c('USUBJID','part_short','Nominal_Dose','nominal_time_day','PCORRES')) %>%
  group_by(USUBJID) %>%
  arrange(nominal_time_day, .by_group = TRUE) %>%
  rename(ntd = nominal_time_day) %>%
  mutate(ntd_prev = lag(ntd, default = 0)) %>%
  mutate(PCORRES_prev = lag(PCORRES, default = 0)) %>%
  mutate(p_AUC = (ntd-ntd_prev)*((PCORRES+PCORRES_prev)/2)) %>%
  mutate(cum_AUC = cumsum(p_AUC))

#limit AUC x-axis to 40k  

auc_plot_time <-  ggplot(data = nem_auc, aes(x = ntd, y = cum_AUC)) + 
  geom_point(aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose)), size = 2) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6)
auc_plot_time

auc_plot_dia <- ggplot(data = nem_auc, aes(x = cum_AUC, y = Diastolic.Blood.Pressure)) + 
  geom_point(aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose)), size = 2) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6) +
  xlim(0,40000)
auc_plot_dia

auc_plot_sys <- ggplot(data = nem_auc, aes(x = cum_AUC, y = Systolic.Blood.Pressure)) + 
  geom_point(aes(color = as.factor(Nominal_Dose), shape = as.factor(Nominal_Dose)), size = 2) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6) +
  xlim(0,40000)
auc_plot_sys

auc_plot_temp <- ggplot(data = nem_auc, aes(x = cum_AUC, y = Temperature)) + 
  geom_point(aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose)), size = 2) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6) +
  xlim(0,40000)
auc_plot_temp

cmax_data <- read_excel("cmax_plots.xls")

cmax_data$Z_Dose <- factor(cmax_data$Z_Dose)


cmax_plot_dia <- ggplot(data = cmax_data, aes(x = Cmax, y = Diastolic_Blood_Pressure,
                                          color = Z_Dose,shape = Z_Dose)) + 
  geom_point(size = 2.5) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6)
cmax_plot_dia

cmax_plot_sys <- ggplot(data = cmax_data, aes(x = Cmax, y = Systolic_Blood_Pressure,
                                              color = Z_Dose,shape = Z_Dose)) + 
  geom_point(size = 2.5) +
  scale_shape_manual(values = 0:6) +
  theme(legend.position="bottom")
cmax_plot_sys

cmax_plot_temp <- ggplot(data = cmax_data, aes(x = Cmax, y = Temperature,
                                               color = Z_Dose,shape = Z_Dose)) + 
  geom_point(size = 2.5) +
  scale_shape_manual(values = 0:6) +
  theme(legend.position="bottom")
cmax_plot_temp

ggsave('auc_plot_time.png', plot = auc_plot_time,width = 8.5,height = 5)
ggsave('auc_plot_dia.png', plot = auc_plot_dia,width = 8.5,height = 5)
ggsave('auc_plot_sys.png', plot = auc_plot_sys,width = 8.5,height = 5)
ggsave('auc_plot_temp.png', plot = auc_plot_temp,width = 8.5,height = 5)
ggsave('cmax_plot_dia.png', plot = cmax_plot_dia,width = 8.5,height = 5)
ggsave('cmax_plot_sys.png', plot = cmax_plot_sys,width = 8.5,height = 5)
ggsave('cmax_plot_temp.png', plot = cmax_plot_temp,width = 8.5,height = 5)

dat_test <- read.csv("C:/Users/li_wen-i/Documents/ALKS4230_VSdata_ptAonly.csv") %>%
  mutate(nominal_time_hour = nominal_time_day*24) %>%
  filter(nominal_time_hour <= 24) %>%
  rowwise() %>%
  mutate(Nominal_Dose = dose_lvls[which.min(abs(Nominal_Dose-dose_lvls))]) %>%
  ungroup() %>%
  mutate(nominal_time_day = ifelse(nominal_time_day == -1.0, 0.0, nominal_time_day)) %>%
  #mutate(PCORRES = ifelse(PCORRES == "Quantity Not Sufficient", 0.0, PCORRES)) %>%
  mutate(conc_mean = ifelse(PCORRES == "NaN", "NA",PCORRES)) %>%
  filter(!is.na(Diastolic.Blood.Pressure) == TRUE) %>%
  relocate(nominal_time_hour, .after = nominal_time_day) %>%
  filter(!is.na(PCORRES) == TRUE) %>%
  group_by(USUBJID)
  #fill(PCORRES, .direction = "updown")

write.csv(dat_test, "C:/Users/li_wen-i/Documents/ALKS4230_ptA_doseID_AE_fixed.csv")

write.csv(dat, "C:/Users/li_wen-i/Documents/ALKS4230_ptA_doseID_AE.csv")

write.csv(dat, "C:/Users/li_wen-i/Documents/ALKS4230_ptA_doseID_AE_filter.csv")

ez_clap <- read.csv("C:/Users/li_wen-i/Documents/ALKS4230_VSdata_ptAonly.csv")

dat_test_shrink <- read.csv("C:/Users/li_wen-i/Documents/ALKS4230_VSdata_ptAonly.csv") %>%
  mutate(nominal_time_hour = nominal_time_day*24) %>%
  filter(nominal_time_hour <= 24) %>%
  rowwise() %>%
  mutate(Nominal_Dose = dose_lvls[which.min(abs(Nominal_Dose-dose_lvls))]) %>%
  ungroup() %>%
  mutate(nominal_time_day = ifelse(nominal_time_day == -1.0, 0.0, nominal_time_day)) %>%
  #mutate(PCORRES = ifelse(PCORRES == "Quantity Not Sufficient", 0.0, PCORRES)) %>%
  mutate(conc_mean = ifelse(PCORRES == "NaN", "NA",PCORRES)) %>%
  #filter(!is.na(Diastolic.Blood.Pressure) == TRUE) %>%
  relocate(nominal_time_hour, .after = nominal_time_day) %>%
  filter(!is.na(PCORRES) == TRUE) %>%
  group_by(USUBJID) %>%
  rename(ID = USUBJID) %>%
  select(c(ID,nominal_time_hour,PCORRES)) %>%
  mutate(nominal_time_hour = round(nominal_time_hour, digits = 4))
  
#fill(PCORRES, .direction = "updown")

delete <- read.csv("C:/Users/li_wen-i/Documents/9 P T, Sp, Dp_AE.csv") %>%
  group_by(ID,T.hour,Nominal_Dose) %>%
  summarise(DP = mean(Diastolic.Blood.Pressure), 
            SP = mean(Systolic.Blood.Pressure),Temp = mean(Temperature)) %>%
  mutate(T.hour = round(T.hour, digits = 4))

merge_final <- inner_join(dat_test_shrink, delete, 
                         by = c('ID' = 'ID', 'nominal_time_hour' = 'T.hour')) %>%
  filter(!PCORRES == "" & !PCORRES == 0)

write.csv(merge_final,"C:/Users/li_wen-i/Documents/ALKS4230_9new.csv")

#6,8,10
#plot 1: x, time y, conc
#plot 2-4: x, conc y, dp,sp,temp

merge_final_6 <- merge_final %>% filter(Nominal_Dose == 6) %>%
  mutate(PCORRES = as.numeric(PCORRES))

merge_final_8 <- merge_final %>% filter(Nominal_Dose == 8) %>%
  mutate(PCORRES = as.numeric(PCORRES))

merge_final_10 <- merge_final %>% filter(Nominal_Dose == 10) %>%
  mutate(PCORRES = as.numeric(PCORRES))

nem_tester <- read.csv("C:/Users/li_wen-i/Documents/ALKS4230_VSdata_ptAonly.csv") %>%
  rowwise() %>%
  mutate(Nominal_Dose_test = dose_lvls[which.min(abs(Nominal_Dose-dose_lvls))]) %>%
  ungroup() %>%
  mutate(nominal_time_day = ifelse(nominal_time_day == -1.0, 0.0, nominal_time_day)) %>%
  #mutate(PCORRES = ifelse(PCORRES == "Quantity Not Sufficient", 0.0, PCORRES))  %>%
  #mutate(conc_mean = ifelse(PCORRES == "NaN", "NA",PCORRES)) %>%
  mutate(nominal_time_day = ifelse(nominal_time_day == -1.0, 0.0, nominal_time_day)) %>%
  mutate(nominal_time_hour = nominal_time_day*24) %>%
  filter(nominal_time_hour <= 24) %>%
  mutate(PCORRES = na_if(PCORRES, "")) %>%
  mutate(PCORRES = as.numeric(PCORRES)) %>%
  relocate(Nominal_Dose_test, .after = Nominal_Dose) %>%
  relocate(nominal_time_hour, .after = nominal_time_day) %>%
  select(c(USUBJID, nominal_time_hour, Nominal_Dose_test, PCORRES, 
           Diastolic.Blood.Pressure, Systolic.Blood.Pressure, Temperature))

test2_88 <- read.csv("C:/Users/li_wen-i/Documents/ALKS4230_VSdata_ptAonly.csv") %>%
  filter(Nominal_Dose == 8) %>%
  filter(nominal_time_day*24 <= 24)

nem_6og <- nem_tester %>% filter(Nominal_Dose_test == 6) 

nem_6 <- nem_tester %>% filter(Nominal_Dose_test == 6) %>%
  group_by(USUBJID, Nominal_Dose_test,nominal_time_hour) %>%
  summarise(DP = mean(Diastolic.Blood.Pressure, na.rm = TRUE), 
            SP = mean(Systolic.Blood.Pressure, na.rm = TRUE),
            Temp = mean(Temperature, na.rm = TRUE),
            PCORRES = mean(PCORRES,na.rm = TRUE))
            
nem_8 <- nem_tester %>% filter(Nominal_Dose_test == 8) %>%
  group_by(USUBJID, Nominal_Dose_test,nominal_time_hour) %>%
  summarise(DP = mean(Diastolic.Blood.Pressure, na.rm = TRUE), 
            SP = mean(Systolic.Blood.Pressure, na.rm = TRUE),
            Temp = mean(Temperature, na.rm = TRUE),
            PCORRES = mean(PCORRES,na.rm = TRUE))

nem_10 <- nem_tester %>% filter(Nominal_Dose_test == 10) %>%
  group_by(USUBJID, Nominal_Dose_test,nominal_time_hour) %>%
  summarise(DP = mean(Diastolic.Blood.Pressure, na.rm = TRUE), 
            SP = mean(Systolic.Blood.Pressure, na.rm = TRUE),
            Temp = mean(Temperature, na.rm = TRUE),
            PCORRES = mean(PCORRES,na.rm = TRUE))

#dfr[!is.na(dfr$y),]

asdf_6 <- ggplot(data = nem_6[!is.na(nem_6$PCORRES),], 
                 aes(x = nominal_time_hour,y= PCORRES)) +
  geom_point(aes(color = USUBJID)) +
  geom_path(aes(color = USUBJID)) +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5)) +
  labs(title = "Concentration vs Time (6ug)",  x = "Time (hours)")
asdf_6

asdf_8 <- ggplot(data = nem_8[!is.na(nem_8$PCORRES),], aes(x = nominal_time_hour,
                                                           y= PCORRES)) +
  geom_point(aes(color = USUBJID)) +
  geom_path(aes(color = USUBJID)) +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5)) +
  labs(title = "Concentration vs Time (8ug)", x = "Time (hours)")
asdf_8

asdf_10 <- ggplot(data = nem_10[!is.na(nem_10$PCORRES),], aes(x = nominal_time_hour,
                                                           y= PCORRES)) +
  geom_point(aes(color = USUBJID)) +
  geom_path(aes(color = USUBJID)) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  labs(title = "Concentration vs Time (10ug)", x = "Time (hours)")
asdf_10

plotterthing <- function(dataset, y_axis){
  ggobj <- ggplot(data = dataset[!is.na(dataset$PCORRES),], aes_string(x = "PCORRES",
                                                     y= y_axis)) +
    geom_point(aes_string(color = "USUBJID")) +
    geom_line(aes_string(color = "USUBJID")) +
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
  return(ggobj)
}

pc_dp_6 <- plotterthing(nem_6,"DP") +
  labs(title = "Concentration vs Diastolic Pressure (6ug)")

pc_sp_6 <- plotterthing(nem_6,"SP") +
  labs(title = "Concentration vs Systolic Pressure (6ug)")

pc_temp_6 <- plotterthing(nem_6,"Temp") + 
  labs(title = "Concentration vs Temperature (6ug)")

pc_dp_8 <- plotterthing(nem_8,"DP") +
  labs(title = "Concentration vs Diastolic Pressure (8ug)")

pc_sp_8 <- plotterthing(nem_8,"SP") +
  labs(title = "Concentration vs Systolic Pressure (8ug)")

pc_temp_8 <- plotterthing(nem_8,"Temp") +
  labs(title = "Concentration vs Temperature (8ug)")

pc_dp_10 <- plotterthing(nem_10,"DP") +
  labs(title = "Concentration vs Diastolic Pressure (10ug)")

pc_sp_10 <- plotterthing(nem_10,"SP") +
  labs(title = "Concentration vs Systolic Pressure (10ug)")

pc_temp_10 <- plotterthing(nem_10,"Temp") +
  labs(title = "Concentration vs Temperature (10ug)")

ggsave('bydose4230_timeconc_6.png',plot = asdf_6)
ggsave('bydose4230_timeconc_8.png',plot = asdf_8)
ggsave('bydose4230_timeconc_10.png',plot = asdf_10)

ggsave('bydose4230_concDp_6.png',plot = pc_dp_6)
ggsave('bydose4230_concSp_6.png',plot = pc_sp_6)
ggsave('bydose4230_concTemp_6.png',plot = pc_temp_6)

ggsave('bydose4230_concDp_8.png',plot = pc_dp_8)
ggsave('bydose4230_concSp_8.png',plot = pc_sp_8)
ggsave('bydose4230_concTemp_8.png',plot = pc_temp_8)

ggsave('bydose4230_concDp_10.png',plot = pc_dp_10)
ggsave('bydose4230_concSp_10.png',plot = pc_sp_10)
ggsave('bydose4230_concTemp_10.png',plot = pc_temp_10)



