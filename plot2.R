
library("readxl")
library(tidyverse)
library(colorspace)

ae_dat <- read_excel("ae_info.xls") %>%
  select(c(TEST,dose,ID,T_hour,Value)) %>%
  pivot_wider(names_from = TEST,values_from = Value) %>%
  select(-c(dose))

ae_og <- read_excel("ae_info.xls") %>%
  select(c(TEST,dose,ID,T_hour,Value)) %>%
  filter(dose == 10)

conc_dat <- read_excel("conc_info.xls") %>%
  mutate(CONC = ifelse(CONC == "BLQ<0.500", 0, CONC)) %>%
  rename(ID = `ID...2`) %>%
  rename(dose = `dose...1`) %>%
  rename(T_hour = `T_hour...3`) %>%
  mutate(CONC = as.numeric(CONC)) %>%
  select(c(ID,dose,T_hour,CONC))

conc_dat6 <- conc_dat %>% filter(dose == 6)
conc_dat8 <- conc_dat %>% filter(dose == 8)
conc_dat10 <- conc_dat %>% filter(dose == 10)

conctime_6_new <- ggplot(data = conc_dat6[!is.na(conc_dat6$CONC),], 
                 aes(x = T_hour,y= CONC)) +
  geom_point(aes(color = ID)) +
  geom_line(aes(color = ID)) +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5)) +
  labs(title = "Concentration vs Time (6ug)",  x = "Time (hours)") +
  ylim(0,250)
conctime_6_new

conctime_8_new <- ggplot(data = conc_dat8[!is.na(conc_dat8$CONC),], 
                         aes(x = T_hour,y= CONC)) +
  geom_point(aes(color = ID)) +
  geom_line(aes(color = ID)) +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5)) +
  labs(title = "Concentration vs Time (8ug)",  x = "Time (hours)") +
  ylim(0,250)
conctime_8_new

conctime_10_new <- ggplot(data = conc_dat10[!is.na(conc_dat10$CONC),], 
                         aes(x = T_hour,y= CONC)) +
  geom_point(aes(color = ID)) +
  geom_line(aes(color = ID)) +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5)) +
  labs(title = "Concentration vs Time (10ug)",  x = "Time (hours)") +
  ylim(0,250)
conctime_10_new

conc_merge <- left_join(conc_dat, ae_dat, 
                        by = c('ID' = 'ID', 'T_hour' = 'T_hour')) %>%
  rename(DP = `Diastolic Blood Pressure`) %>%
  rename(SP = `Systolic Blood Pressure`)

conc_merge6 <- conc_merge %>% filter(dose == 6)
conc_merge8 <- conc_merge %>% filter(dose == 8)
conc_merge10 <- conc_merge %>% filter(dose == 10)

plotterthing <- function(dataset, y_axis){
  dataset1 <- filter(dataset,!is.na(dataset[y_axis]))
  
  ggobj <- ggplot(data = dataset1[!is.na(dataset1$CONC),], aes_string(x = "CONC",
                                                                       y= y_axis)) +
    geom_point(aes_string(color = "ID")) +
    geom_line(aes_string(color = "ID")) +
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5)) +
    xlim(0,250)
  return(ggobj)
}

pc_dp_6 <- plotterthing(conc_merge6,"DP") +
  labs(title = "Concentration vs Diastolic Pressure (6ug)")

pc_sp_6 <- plotterthing(conc_merge6,"SP") +
  labs(title = "Concentration vs Systolic Pressure (6ug)")

pc_temp_6 <- plotterthing(conc_merge6,"Temperature") + 
  labs(title = "Concentration vs Temperature (6ug)")

pc_dp_8 <- plotterthing(conc_merge8,"DP") +
  labs(title = "Concentration vs Diastolic Pressure (8ug)")

pc_sp_8 <- plotterthing(conc_merge8,"SP") +
  labs(title = "Concentration vs Systolic Pressure (8ug)")

pc_temp_8 <- plotterthing(conc_merge8,"Temperature") +
  labs(title = "Concentration vs Temperature (8ug)")

pc_dp_10 <- plotterthing(conc_merge10,"DP") +
  labs(title = "Concentration vs Diastolic Pressure (10ug)")

pc_sp_10 <- plotterthing(conc_merge10,"SP") +
  labs(title = "Concentration vs Systolic Pressure (10ug)")

pc_temp_10 <- plotterthing(conc_merge10,"Temperature") +
  labs(title = "Concentration vs Temperature (10ug)")

ggsave('new_bydose4230_timeconc_6.png',plot = conctime_6_new)
ggsave('new_bydose4230_timeconc_8.png',plot = conctime_8_new)
ggsave('new_bydose4230_timeconc_10.png',plot = conctime_10_new)

ggsave('new_bydose4230_concDp_6.png',plot = pc_dp_6)
ggsave('new_bydose4230_concSp_6.png',plot = pc_sp_6)
ggsave('new_bydose4230_concTemp_6.png',plot = pc_temp_6)

ggsave('new_bydose4230_concDp_8.png',plot = pc_dp_8)
ggsave('new_bydose4230_concSp_8.png',plot = pc_sp_8)
ggsave('new_bydose4230_concTemp_8.png',plot = pc_temp_8)

ggsave('new_bydose4230_concDp_10.png',plot = pc_dp_10)
ggsave('new_bydose4230_concSp_10.png',plot = pc_sp_10)
ggsave('new_bydose4230_concTemp_10.png',plot = pc_temp_10)

#zzzzzzzzzzzzzz
sysc6 <- read_excel("6sp.xls") %>% 
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(SP = `ValueSP`) %>%
  group_by(ID,T_hour, CONC) %>%
  summarise(SP = mean(SP, na.rm = TRUE))

diac6 <- read_excel("6dp.xls") %>% 
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(DP = `Value DP`) %>%
  group_by(ID,T_hour, CONC) %>%
  summarise(DP = mean(DP, na.rm = TRUE))

temp6 <- read_excel("6temp.xls") %>% 
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(Temperature = `Value Temp`) %>%
  group_by(ID,T_hour, CONC) %>%
  summarise(Temperature = mean(Temperature, na.rm = TRUE))

sysc8 <- read_excel("8sp.xls") %>% 
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(SP = `Value SP`) %>%
  group_by(ID,T_hour, CONC) %>%
  summarise(SP = mean(SP, na.rm = TRUE)) 

diac8 <- read_excel("8dp.xls") %>% 
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(DP = `Value DP`) %>%
  group_by(ID,T_hour, CONC) %>%
  summarise(DP = mean(DP, na.rm = TRUE)) 

temp8 <- read_excel("8temp.xls") %>% 
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(Temperature = `Value Temp`) %>%
  group_by(ID,T_hour, CONC) %>%
  summarise(Temperature = mean(Temperature, na.rm = TRUE))

diac10 <- read_excel("10dp.xls") %>% 
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(DP = `Result`) %>%
  group_by(ID,T_hour, CONC) %>%
  summarise(DP = mean(DP, na.rm = TRUE)) 

temp10 <- read_excel("10temp.xls") %>% 
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(Temperature = `Value Temp`) %>%
  group_by(ID,T_hour, CONC) %>%
  summarise(Temperature = mean(Temperature, na.rm = TRUE))

big_color_palette <- unique(c(unique(diac6$ID),unique(sysc6$ID),unique(temp6$ID),
                              unique(diac8$ID),unique(sysc8$ID),unique(temp8$ID),
                              unique(diac10$ID),unique(temp10$ID)))

colorid_main <- rainbow_hcl(11)
colorid <- diverge_hcl(11)

#this shouldn't be done manually btw
Dose <- c(6,6,6,6,6,6,6,8,8,8,10)

iris$colors <- factor(iris$Species,
                      levels=c("virginica", "versicolor", "setosa"),
                      labels=rainbow_hcl(3))

colors <- data.frame(big_color_palette, colorid_main, Dose) %>%
  rename(ID = big_color_palette)

col = setNames(c("green","red","blue"), levels(iris$Species))

test <- setNames(colorid_main, levels(colors$ID))

pt1 = full_join(sysc6,diac6, by = c("ID" = "ID", "T_hour" = "T_hour",
                                              "CONC" = "CONC")) %>%
  full_join(temp6, by = c("ID" = "ID", "T_hour" = "T_hour",
                                "CONC" = "CONC"))
pt2 <- full_join(sysc8, diac8, by = c("ID" = "ID", "T_hour" = "T_hour",
                          "CONC" = "CONC")) %>%
  full_join(temp8, by = c("ID" = "ID", "T_hour" = "T_hour",
                          "CONC" = "CONC"))
pt3 <- full_join(temp10, diac10, by = c("ID" = "ID", "T_hour" = "T_hour",
                          "CONC" = "CONC"))
  
bigbig <- rbind(pt1,pt2,pt3) %>%
  inner_join(colors, by = "ID")

#aes_string can make this into 1 function, but i was gigatilted at that point
plotterthingDP <- function(dataset, dose,coloridn){
  sub <- subset(dataset, Dose == dose & !is.na(CONC) & !is.na(DP))
  ggobj <- ggplot(data = sub, 
                  aes(x = CONC, y= DP, color = ID)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = coloridn, drop = TRUE,limits = force) +
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
  return(ggobj)
}

plotterthingSP <- function(dataset, dose, coloridn){
  sub <- subset(dataset, Dose == dose & !is.na(CONC) & !is.na(SP))
  ggobj <- ggplot(data = sub, 
                  aes(x = CONC, y= SP, color = ID)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = coloridn, drop = TRUE, limits = force) +
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
  return(ggobj)
}

plotterthingTemp <- function(dataset, dose, coloridn){
  sub <- subset(dataset, Dose == dose & !is.na(CONC) & !is.na(Temperature))
  ggobj <- ggplot(data = sub, 
                  aes(x = CONC, y= Temperature, color = ID)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = coloridn, drop = TRUE, limits = force) +
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
  return(ggobj)
}

pc_dp_6again <- plotterthingDP(bigbig,6, colorfac) +
  labs(title = "Concentration vs Diastolic Pressure (6ug)")
pc_dp_6again

pc_sp_6again <- plotterthingSP(bigbig,6, colorfac) +
  labs(title = "Concentration vs Systolic Pressure (6ug)")
pc_sp_6again

pc_temp_6again <- plotterthingTemp(bigbig,6, colorfac) +
  labs(title = "Concentration vs Temperature (6ug)")
pc_temp_6again

pc_dp_8again <- plotterthingDP(bigbig,8,colorfac) +
  labs(title = "Concentration vs Diastolic Pressure (8ug)")
pc_dp_8again

pc_sp_8again <- plotterthingSP(bigbig,8,colorfac) +
  labs(title = "Concentration vs Systolic Pressure (8ug)")
pc_sp_8again

pc_temp_8again <- plotterthingTemp(bigbig,8,colorfac) +
  labs(title = "Concentration vs Temperature (8ug)")
pc_temp_8again

pc_dp_10again <- plotterthingDP(bigbig,10,colorfac) +
  labs(title = "Concentration vs Diastolic Pressure (10ug)")
pc_dp_10again

pc_temp_10again <- plotterthingTemp(bigbig,10,test) +
  labs(title = "Concentration vs Temperature (10ug)")
pc_temp_10again

#
dt10 <- read_excel("dosetime.xlsx", sheet = 1) %>%
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc))) %>%
  rename(T_hour = `T hour`) %>%
  filter(!is.na(CONC)) %>%
  inner_join(all_colors, by = "ID") %>%
  group_by(ID,T_hour) %>%
  summarise(CONC = mean(CONC, na.rm = TRUE)) 

dt8 <- read_excel("dosetime.xlsx", sheet = 2) %>%
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc)))%>%
  rename(T_hour = `T hour`) %>%
  filter(!is.na(CONC)) %>%
  inner_join(all_colors, by = "ID") %>%
  group_by(ID,T_hour) %>%
  summarise(CONC = mean(CONC, na.rm = TRUE)) 

dt6 <- read_excel("dosetime.xlsx", sheet = 3) %>%
  mutate(CONC = as.numeric(ifelse(Conc == "BLQ<0.500", 0, Conc)))%>%
  rename(T_hour = `T hour`)%>%
  filter(!is.na(CONC)) %>%
  inner_join(all_colors, by = "ID") %>%
  group_by(ID,T_hour) %>%
  summarise(CONC = mean(CONC, na.rm = TRUE)) 

#this should go up top so i don't get confused
big_color_palette2 <- unique(c(unique(diac6$ID),unique(sysc6$ID),unique(temp6$ID),
                              unique(diac8$ID),unique(sysc8$ID),unique(temp8$ID),
                              unique(diac10$ID),unique(temp10$ID),unique(dt10$ID),
                              unique(dt8$ID),unique(dt6$ID)))

#why didn't i do this all along wtf
all_colors <- data.frame(big_color_palette2, colorid_final) %>%
  rename(ID = big_color_palette2) %>%
  mutate(Dose = case_when(ID %in% unique(sysc6$ID) | ID %in% unique(dt6$ID) | ID %in% unique(diac6$ID) | ID %in% unique(dt6$ID) ~ 6,
         ID %in% unique(sysc8$ID) | ID %in% unique(diac8$ID) | ID %in% unique(dt8$ID) | ID %in% unique(dt8$ID) ~ 8,
         ID %in% unique(diac10$ID) | ID %in% unique(temp10$ID) | ID %in% unique(dt10$ID) ~ 10))

col = setNames(c("green","red","blue"), levels(iris$Species))
colorid_final <- rainbow_hcl(length(big_color_palette2))
colorfac <- setNames(colorid_final, levels(all_colors$ID))

plotterthingTime <- function(dataset, coloridn){
  ggobj <- ggplot(data = dataset, 
                  aes(x = T_hour, y= CONC, color = ID)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = coloridn, drop = TRUE, limits = force) +
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
  return(ggobj)
}

pc_time_10ag <- plotterthingTime(dt10,colorfac) +
  labs(title = "Time vs Concentration (10ug)")
pc_time_10ag

pc_time_8ag <- plotterthingTime(dt8,colorfac)+
  labs(title = "Time vs Concentration (8ug)")
pc_time_8ag

pc_time_6ag <- plotterthingTime(dt6,colorfac)+
  labs(title = "Time vs Concentration (6ug)")
pc_time_6ag



ggsave('newer_timeconc_6.png',plot = pc_time_6ag, height = 4, width = 6)
ggsave('newer_timeconc_8.png',plot = pc_time_8ag, height = 4, width = 6)
ggsave('newer_timeconc_10.png',plot = pc_time_10ag, height = 4, width = 6)

ggsave('newer_concDp_6.png',plot = pc_dp_6again, height = 4, width = 6)
ggsave('newer_concSp_6.png',plot = pc_sp_6again, height = 4, width = 6)
ggsave('newer_concTemp_6.png',plot = pc_temp_6again, height = 4, width = 6)

ggsave('newer_concDp_8.png',plot = pc_dp_8again, height = 4, width = 6)
ggsave('newer_concSp_8.png',plot = pc_sp_8again, height = 4, width = 6)
ggsave('newer_concTemp_8.png',plot = pc_temp_8again, height = 4, width = 6)

ggsave('newer_concDp_10.png',plot = pc_dp_10again, height = 4, width = 6)
ggsave('newer_concTemp_10.png',plot = pc_temp_10again, height = 4, width = 6)
