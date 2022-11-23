library(foreign)
library(dplyr)
library(haven)

# Lenovo
setwd("G:\\Il mio Drive\\PhD Padova\\3rd year\\ELSA data\\stata13_se")
# Surface
# setwd("C:\\Users\\39334\\Google Drive\\PhD Padova\\3rd year\\ELSA data\\stata13_se")
# Set the working directory
# setwd("your path here")

############### Sample Selection step by step ###############
dt1 <- read.dta("wave_8_elsa_data_eul_v2.dta")
dt2 <- read_dta("wave_8_elsa_financial_dvs_eul_v1.dta")
dt3 <- read.dta("wave_8_elsa_ifs_dvs_eul_v1.dta")
dt4 <- read_dta("wave8_Hindex.dta")

dt4 <- dt4 %>% dplyr::select(idauniq, H_index) %>% mutate(idauniq = as.integer(idauniq))
dt2 <- dt2 %>% mutate(idauniq = as.integer(dt2$idauniq))

df <- inner_join(dt1, dt2, by = "idauniq")
df <- inner_join(df, dt3, by = "idauniq")
df <- inner_join(df, dt4, by = "idauniq")

df <- df %>% mutate(scgrisk_1 = ifelse(scgrisk=="Avoid taking risks",0,scgrisk),
                    scgrisk_1 = ifelse(scgrisk=="Fully prepared to take risks",10,scgrisk),
                    scgrisk_1 = ifelse(scgrisk=="Self-completion instrument not completed",-1,scgrisk),
                    scgrisk_1 = ifelse(scgrisk=="No valid answer",-1,scgrisk),
                    scgrisk_1 = ifelse(scgrisk=="Do not know",-1,scgrisk),
                    scgpati_1 = ifelse(scgpati=="Very impatient",0,scgpati),
                    scgpati_1 = ifelse(scgpati=="Very Patient",10,scgpati),
                    scgpati_1 = ifelse(scgpati=="Self-completion instrument not completed",-1,scgpati),
                    scgpati_1 = ifelse(scgpati=="No valid answer",-1,scgpati),
                    scgpati_1 = ifelse(scgpati=="Do not know",-1,scgpati), 
                    scfrisk_1 = ifelse(scfrisk=="Avoid taking risks",0,scfrisk),
                    scfrisk_1 = ifelse(scfrisk=="Fully prepared to take risks",10,scfrisk),
                    scfrisk_1 = ifelse(scfrisk=="Self-completion instrument not completed",-1,scfrisk),
                    scfrisk_1 = ifelse(scfrisk=="No valid answer",-1,scfrisk),
                    scfrisk_1 = ifelse(scfrisk=="Do not know",-1,scfrisk), 
                    scfpati_1 = ifelse(scfpati=="Very impatient",0,scfpati),
                    scfpati_1 = ifelse(scfpati=="Very Patient",10,scfpati),
                    scfpati_1 = ifelse(scfpati=="Self-completion instrument not completed",-1,scfpati),
                    scfpati_1 = ifelse(scfpati=="No valid answer",-1,scfpati),
                    scfpati_1 = ifelse(scfpati=="Do not know",-1,scfpati))

df <- df %>% mutate(scgrisk_1 = scgrisk_1-6, scgpati_1 = scgpati_1-6,
                    scfrisk_1 = scfrisk_1-6, scfpati_1 = scfpati_1-6)
df_copia <- df
df <- df %>% dplyr::select(idauniq, idahhw8.x, futype.x, perid, iapid, fuid.x, coupid.x, hhresp,
                           indager, indobyr.x, indsex, edend, edqual, hhtot, everwork, wpactive, ecpos, hoh,
                           cfsva, cfsvb, cfsvc, cfsvd, cfsve,
                           savings_bu_s, invests_bu_s, invests_bu_t, invests_bu_f, grossfw_bu_s, netfw_bu_s, netpw_bu_s,
                           grosstotnhw_bu_s, nettotnhw_bu_s, nettotw_bu_s, mgdebt_bu_s, debt_bu_s,
                           numhhldrsinhh, nameonprop, spnameonprop, hopaym,
                           empinc_r_s, empinc_p_s, empinc_bu_s, seinc_r_s, seinc_p_s, seinc_bu_s,
                           ppinc_r_s, ppinc_p_s, ppinc_bu_s, spinc_r_s, spinc_p_s, spinc_bu_s, 
                           beninc_bu_s, beninc_r_s, beninc_p_s, dhiasep, exrslf,
                           scgrisk_1, scgpati_1, scfrisk_1, scfpati_1, H_index)

# couple with joint finances
df1 <- df %>% filter(futype.x == "Couple with joint finances")
df_m1 <- df1 %>% filter(indsex == "Male")
df_f1 <- df1 %>% filter(indsex == "Female")
couple <- inner_join(df_m1, df_f1, by = "idahhw8.x")
check_joint <- anti_join(df_m1, df_f1, by = "idahhw8.x")
couple <- couple %>% mutate(check_age = (indager.x<0)+(indager.y<0)) %>% filter(check_age == 0)
couple <- couple %>% mutate(check_grisk_m = (scgrisk_1.x>0 & scgrisk_1.x < 12), check_grisk_f = (scgrisk_1.y>0 & scgrisk_1.y < 12),
                            check_frisk_m = (scfrisk_1.x>0 & scfrisk_1.x < 12), check_frisk_f = (scfrisk_1.y>0 & scfrisk_1.y < 12),
                            check_gp_m = (scgpati_1.x>0 & scgpati_1.x < 12), check_gp_f = (scgpati_1.y>0 & scgpati_1.y < 12),
                            check_fp_m = (scfpati_1.x>0 & scfpati_1.x < 12), check_fp_f = (scfpati_1.y>0 & scfpati_1.y < 12),
                            check_tot = check_grisk_m+check_grisk_f+check_frisk_m+check_frisk_f+check_gp_m+check_gp_f+check_fp_m+check_fp_f) %>%
  filter(check_tot == 8)
write.dta(couple, "wave_8_empirical_analysis.dta")