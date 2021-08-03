library(ggplot2)
library(dplyr)
library(MESS)
library("readxl")

#Plot blood pressure (diastolic and systolic) vs time and by dose (done)
#Plot temperature vs time by dose (done)
#Plot nemvaleukin concentration vs time by dose, 
#     matching the same time scale as for blood pressure and body temperature
#Plot blood pressure vs Cmax (using different color for different dose level) to see if there is any correlation 
#Plot temperature vs Cmax (using different color for different dose level) to see if there is any correlation
#Plot blood pressure vs AUC (using different color for different dose level) to see if there is any correlation
#Plot temperature vs AUC (using different color for different dose level) to see if there is any correlation

#load data:

nem <- read.csv("data_file.csv",fileEncoding="UTF-8-BOM") %>%
  mutate(Nominal_Dose = round(Nominal_Dose)) %>%
  mutate(nominal_time_day = ifelse(nominal_time_day == -1.0, 0.0, nominal_time_day)) %>%
  mutate(PCORRES = ifelse(PCORRES == "Quantity Not Sufficient", 0.0, PCORRES))  %>%
  mutate(conc_mean = ifelse(PCORRES == "NaN", "NA",PCORRES))

nem_means <- nem %>% mutate(nominal_time_day = round(nominal_time_day)) %>%
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
  theme(legend.position="bottom")
bpdia

bpdia_avg <- ggplot(data = nem_means, aes(x = nominal_time_day,
                                          y= dia_mean)) +
  geom_line(aes(color = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
bpdia_avg

bpsys <-  ggplot(data = nem, aes(x = nominal_time_day, y = Systolic.Blood.Pressure)) + 
  geom_jitter(data = nem[!is.na(nem$Systolic.Blood.Pressure),],
              aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
bpsys

bpsys_avg <- ggplot(data = nem_means, aes(x = nominal_time_day,
                                          y= sys_mean)) +
  geom_line(aes(color = as.factor(Nominal_Dose), linetype = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
bpsys_avg

temp_plot <-  ggplot(data = nem, aes(x = nominal_time_day, y = Temperature)) + 
  geom_jitter(data = nem[!is.na(nem$Temperature),],
              aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
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
  theme(legend.position="bottom")
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


auc_plot_time <-  ggplot(data = nem_auc, aes(x = ntd, y = cum_AUC)) + 
  geom_point(aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
auc_plot_time

auc_plot_dia <- ggplot(data = nem_auc, aes(x = cum_AUC, y = Diastolic.Blood.Pressure)) + 
  geom_point(aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
auc_plot_dia

auc_plot_sys <- ggplot(data = nem_auc, aes(x = cum_AUC, y = Systolic.Blood.Pressure)) + 
  geom_point(aes(color = as.factor(Nominal_Dose), shape = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
auc_plot_sys

auc_plot_temp <- ggplot(data = nem_auc, aes(x = cum_AUC, y = Temperature)) + 
  geom_point(aes(color = as.factor(Nominal_Dose),shape = as.factor(Nominal_Dose))) +
  theme(legend.position="bottom")
auc_plot_temp

cmax_data <- read_excel("cmax_plots.xls")

cmax_data$Z_Dose <- factor(cmax_data$Z_Dose)


cmax_plot_dia <- ggplot(data = cmax_data, aes(x = Cmax, y = Diastolic_Blood_Pressure,
                                          color = Z_Dose,shape = Z_Dose)) + 
  geom_point() +
  theme(legend.position="bottom") +
  scale_shape_manual(values = 0:6)
cmax_plot_dia

cmax_plot_sys <- ggplot(data = cmax_data, aes(x = Cmax, y = Systolic_Blood_Pressure,
                                              color = Z_Dose,shape = Z_Dose)) + 
  geom_point() +
  scale_shape_manual(values = 0:6) +
  theme(legend.position="bottom")
cmax_plot_sys

cmax_plot_temp <- ggplot(data = cmax_data, aes(x = Cmax, y = Temperature,
                                               color = Z_Dose,shape = Z_Dose)) + 
  geom_point() +
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





