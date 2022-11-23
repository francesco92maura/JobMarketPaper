# This script generate the data for:
# Table 1, Table 2, Table 3 and Table 4
# Figure 1, Figure 2, Figure 3

library(foreign)
library(dplyr)
library(haven)

# Lenovo
setwd("G:\\Il mio Drive\\PhD Padova\\3rd year\\ELSA data\\stata13_se")
# Surface
# setwd("C:\\Users\\39334\\Google Drive\\PhD Padova\\3rd year\\ELSA data\\stata13_se")
# Set the working directory
# setwd("your path here")

############ Sample Statistics #############
stat_df <- read_dta("wave_8_couple.dta")

# Table 1 - all sample statistics
table1 <- data.frame(val = unclass(stat_df %>% group_by(participation) %>%
           summarise(n = n(), respondent_m = sum(deci==1)/n(),
           h_income = mean(hh_income), med_h_income = median(hh_income), sd_h_income = sd(hh_income),
           finw = mean(net_finw_hh)/1000, med_finw = median(net_finw_hh)/1000, sd_finw = sd(net_finw_hh)/1000,
           gfinw = mean(gross_finw_hh)/1000, med_gfinw = median(gross_finw_hh)/1000, sd_gfinw = sd(gross_finw_hh)/1000,
           alpha = mean(stock_share_0), gamma_male = mean(gamma_m), gamma_fin_resp = mean(gamma_dec))))
t(table1)

# Table 2 - male and female partners
table2 <- data.frame(val = unclass(stat_df %>% group_by(participation) %>%
           summarise(n = n(),
           age_m = mean(indager_m), age_f = mean(indager_f), 
           low_edu_m = sum(edu_m==1)/n(), mid_edu_m = sum(edu_m==2)/n(), h_edu_m = sum(edu_m==3)/n(),
           low_edu_f = sum(edu_f==1)/n(), mid_edu_f = sum(edu_f==2)/n(), h_edu_f = sum(edu_f==3)/n(),
           general_risk_m = mean(scgrisk_1_m), general_risk_f = mean(scgrisk_1_f),
           financial_risk_m = mean(scfrisk_1_m), financial_risk_f = mean(scfrisk_1_f))))
t(table2)

# Table 3 - financial and non-financial respondent partners
table3 <- data.frame(val = unclass(stat_df %>% group_by(participation) %>%
           summarise(n = n(),
           age_dec = mean(indager_dec), age_ndec = mean(indager_ndec), 
           low_edu_dec = sum(edu_dec==1)/n(), mid_edu_dec = sum(edu_dec==2)/n(),
           h_edu_dec = sum(edu_dec==3)/n(), low_edu_ndec = sum(edu_ndec==1)/n(),
           mid_edu_ndec = sum(edu_ndec==2)/n(), h_edu_ndec = sum(edu_ndec==3)/n(),
           general_risk_dec = mean(scgrisk_dec), general_risk_ndec = mean(scgrisk_ndec),
           financial_risk_dec = mean(scfrisk_dec), financial_risk_ndec = mean(scfrisk_ndec))))
t(table3)

# Table 4 - household wealth allocation
table4 <- data.frame(val = unclass(stat_df %>% group_by(participation, quart_income) %>%
           summarise(n = n(), male_age = mean(indager_m),
           tot_wealth = mean(net_totw_hh)/1000,
           house_wealth = mean((net_totw_hh-nettotnhw_bu_s_m)/net_totw_hh, na.rm = T),
           safe_asset = mean(savings_bu_s_m/net_totw_hh, na.rm = T),
           risky_asset = mean(invests_bu_s_m/net_totw_hh, na.rm = T),
           physical_wealth = mean(netpw_bu_s_m/net_totw_hh, na.rm = T), debt = mean(debt_bu_s_m, na.rm = T),
           mortgage = mean(mgdebt_bu_s_m, na.rm = T))))
t(table4)
table4

############ Figures #############
library(ggplot2)
m <- stat_df %>% dplyr::select(indsex_m, income_m) %>%
  mutate(gender=ifelse(indsex_m==7,"male","female"), income = income_m) %>%
  dplyr::select(gender, income)
f <- stat_df %>% dplyr::select(indsex_f, income_f)%>%
  mutate(gender=ifelse(indsex_f==8,"female","male"), income = income_f) %>%
  dplyr::select(gender, income)
a <- rbind(m,f)
# Figure 1
a %>% ggplot(aes(y=income, x = gender)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits=c(0, 1000))
ggsave("figure1.png", width = 1500,
       height = 750, units = "px", dpi = 300)
# Figure 2
a %>% ggplot(aes(x=income)) + 
  geom_histogram(binwidth = 40, color="black") +
  scale_x_continuous(limits=c(0, 1250)) +
  facet_grid(.~gender)
ggsave("figure2.png", width = 1500,
       height = 750, units = "px", dpi = 300)
# Figure 3
a %>% ggplot(aes(x=income, fill = gender)) + 
  geom_density(alpha = 0.2, color="black") +
  scale_x_continuous(limits=c(0, 1250)) +
  theme(legend.position = "bottom")
ggsave("figure3.png", width = 1500,
       height = 750, units = "px", dpi = 300)

