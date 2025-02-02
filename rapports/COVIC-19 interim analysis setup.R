library(knitr)
library(tidyverse)
library(readxl)
library(labelled)
library(gtsummary)
library(BSDA)
library(gsDesign)
library(ggplot2)

# Import data
t1_inc <- read_csv2("~/DATA/COVIC-19/SOURCES/S1_191904/T1_INC_ANSI.csv", locale = locale(encoding = "ISO-8859-1") )
t1_rando <- read_csv2("~/DATA/COVIC-19/SOURCES/S1_191904/T1_4_RANDO_ANSI.csv", locale = locale(encoding = "ISO-8859-1"))
t2_d1 <- read_csv2("~/DATA/COVIC-19/SOURCES/S1_191904/T2_D1_ANSI.csv", locale = locale(encoding = "ISO-8859-1"))
t5_d28 <- read_csv2("~/DATA/COVIC-19/SOURCES/S1_191904/T5_D28_ANSI.csv", locale = locale(encoding = "ISO-8859-1"))
t9_cmtab <- read_csv2("~/DATA/COVIC-19/SOURCES/S1_191904/T9_CMTAB_ANSI.csv", locale = locale(encoding = "ISO-8859-1"))

# Merging the data
covic19.data <- t1_inc %>% merge(t1_rando, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t2_d1, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t5_d28, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y"))

# Removing patients who withdrew consent
covic19.data <- covic19.data %>% filter(SUBJECT_REF != "DE-04-019")


# Dealing with missing data
covic19.data <- covic19.data %>%
  mutate(across(where(is.character), ~na_if(.,"ND"))) %>%
  mutate(COVID19_VAC = na_if(COVID19_VAC, 2)) %>%
  mutate(D1_PCRASSTYP1 = replace(D1_PCRASSTYP1, D1_PCRASSTYP1 %in% c("ND","11"), NA)) %>%
  mutate(D1_IGAASS = replace(D1_IGAASS, D1_IGAASS %in% c("ND","2"), NA)) %>%
  mutate(D1_IGGASS = replace(D1_IGGASS, D1_IGGASS %in% c("ND","2"), NA)) 


# Dates
dates <- c("COVIDAT", "PCRDAT", "COVID19_PCR_DATIM", "COVID19_1STVAC_DAT", "COVID19_2NDVAC_DAT", "COVID19_3RDVAC_DAT", "COVID19_4RDVAC_DAT")
datetimes <- c("INCDAT", "RANDODAT", "PLASMA1_STDAT")
covic19.data <- covic19.data %>% mutate(across(all_of(dates), \(x) as.Date(x, format = "%d/%m/%Y") )) %>%
  mutate(across(all_of(datetimes), \(x) as.Date(x, format = "%d%b%Y:%H:%M") ))

# Sort by inclusion date and select first 102 inclusions
# covic19.data <- covic19.data %>% arrange(INCDAT) %>% remove_rownames() %>% rownames_to_column(var = "id") %>% mutate(id = as.numeric(id)) %>% filter(id <= 102)

# Recode Omicron types
covic19.data <- covic19.data %>%
  mutate(OMICRONTYP = case_when(OMICRONTYP_C1 == 1 ~ "BA.1 lineage",
                                OMICRONTYP_C2 == 1 ~ "BA.2 lineage",
                                OMICRONTYP_C3 == 1 ~ "BA.3 lineage",
                                OMICRONTYP_C4 == 1 ~ "BA.4 lineage",
                                OMICRONTYP_C5 == 1 ~ "BA.5 lineage",
                                OMICRONTYP_C6 == 1 ~ "XBB / XBB.1",
                                OMICRONTYP_C7 == 1 ~ "BQ1 / BQ1.1", 
                                OMICRONTYP_C8 == 1 ~ "BF.7 / BF.14",
                                OMICRONTYP_C10 == 1 ~ "CH1.1"))

# Recode concomittant medications DCI
## Table des DCI médicamenteuses

# COVID19_CMDCI_recode <- t9_cmtab %>% select(SUBJECT_REF, RANDOMIZATION_R1, COVID19_CMDCI) %>%
#   count(COVID19_CMDCI) %>%
#   write_csv2("~/DATA/COVIC-19/SORTIES/COVID19_CMDCI_recoding.csv")

## Import recoded medications
CM_recoding <- read_csv2("~/DATA/COVIC-19/SOURCES/2023 07 20 COVID19_CM_recoding_md.csv")

t9_cmtab <- left_join(t9_cmtab, CM_recoding, by = c("COVID19_CMCLASS", "COVID19_CMDCI"))


# Variable creation
covic19.data <- covic19.data %>% mutate(days_cov_rando = RANDODAT - COVIDAT) %>% # Time from symptom onset to randomisation
  mutate(days_first_pcr_rando = RANDODAT - COVID19_PCR_DATIM) %>% # Time from first PCR to randomisation
  mutate(days_last_pcr_rando = RANDODAT - PCRDAT) %>% # Time from last PCR to randomisation
  mutate(days_rando_plasma = PLASMA1_STDAT - RANDODAT) %>% # Time from randomisation to plasma
  mutate(vac_1dose = if_else(!is.na(COVID19_1STVAC_TYP), "Yes", NA)) %>%
  mutate(vac_2dose = if_else(!is.na(COVID19_2NDVAC_TYP), "Yes", NA)) %>%
  mutate(vac_3dose = if_else(!is.na(COVID19_3RDVAC_TYP), "Yes", NA)) %>%
  mutate(vac_4dose = if_else(!is.na(COVID19_4RDVAC_TYP), "Yes", NA)) %>%
  mutate(nb_vacdose = case_when(!is.na(COVID19_4RDVAC_TYP) ~ 4,
                                !is.na(COVID19_3RDVAC_TYP) ~ 3,
                                !is.na(COVID19_2NDVAC_TYP) ~ 2,
                                !is.na(COVID19_1STVAC_TYP) ~ 1,
                                COVID19_VAC == 0 ~ 0)) %>%
  mutate(lastvacdose_dat = pmax(COVID19_1STVAC_DAT, COVID19_2NDVAC_DAT, COVID19_3RDVAC_DAT, COVID19_4RDVAC_DAT, na.rm = TRUE)) %>% # Date of last vaccine dose 
  mutate(days_lastvacdose_rando = RANDODAT - lastvacdose_dat) %>% # Time from last vaccine dose to randomisation
  mutate(D1_PCRASSCT_V = D1_PCRASSCT_V / 10) %>% # Decimal correction
  mutate(D28_PCRCT_V = D28_PCRCT_V / 10) # Decimal correction

# Variable labels
covic19.data <- covic19.data %>% set_variable_labels(AGE_V = "Age",
                                                     SEX = "Sex at birth",
                                                     BLOODGRP = "Blood Group",
                                                     days_cov_rando = "Time from symptom onset to randomisation (days)",
                                                     days_first_pcr_rando = "Time from first PCR to randomisation (days)",
                                                     days_last_pcr_rando = "Time from last PCR to randomisation (days)",
                                                     days_rando_plasma = "Time from randomisation to plasma infusion (days)",
                                                     IN07 = "High risk",
                                                     # acquired immune deficiencies
                                                     IN07A1 = "Lymphoid malignancy",
                                                     IN07A2 = "Myeloid malignancy",
                                                     IN07A3 = "Solid tumor",
                                                     IN07A4 = "Allogenic HSCT",
                                                     IN07A5 = "Organ transplantation",
                                                     IN07A6 = "Anti CD19/20 MoAb / MMF",
                                                     IN07A7 = "Anti CD19/20 CAR-T cell",
                                                     IN07A8 = "ATG / alemtuzumab",
                                                     IN07A9 = "AIDS",
                                                     # primary immune deficiencies
                                                     IN07B1 = "B cell deficiency",
                                                     IN07B2 = "T cell deficiency",
                                                     IN07B3 = "Combined deficiency",
                                                     # No seroconversion
                                                     IN08C1 = "No detectable seroconversion",
                                                     COVID19_VAC = "COVID19 Vaccination",
                                                     nb_vacdose = "Nb doses",
                                                     D1_PCRASS = "D1 SARS-CoV-2 PCR",
                                                     D1_PCRASSRES = "D1 SARS-CoV-2 PCR Result",
                                                     D1_PCRASSCT_V = "D1 Ct value",
                                                     D1_PCRASSTYP1 = "Variant",
                                                     OMICRONTYP = "Omicron variants",
                                                     D1_IGAASS = "D1 SARS-CoV-2 IgA",
                                                     D1_IGAASSRATIO_V = "IgA ELISA OD ratio",
                                                     D1_IGGASS = "D1 SARS-CoV-2 IgG",
                                                     D1_IGGASSRATIO_V = "IgG ELISA OD ratio",
                                                     days_lastvacdose_rando = "Time from last vaccine dose to randomisation (days",
                                                     PLASMA_NB = "No. plasma units received",
                                                     D28_PCRASS = "D28 SARS-CoV-2 PCR",
                                                     D28_PCRRES = "D28 SARC-CoV-2 PCR Result",
                                                     D28_PCRCT_V = "D28 Ct value"
) %>%
  set_value_labels(SEX = c(Male = 1, Female = 2),
                   BLOODGRP = c(A = 1, B = 2, AB = 3, O = 4),
                   COVID19_VAC = c(Yes = 1, No = 0),
                   D1_PCRASSRES = c(Positive = 1, Negative = 2),
                   D28_PCRASS = c(Yes = "1", No = "0"),
                   D28_PCRRES = c(Positive = "1", Negative = "2"),
                   D1_PCRASSTYP1 = c("Alpha B.1.1.7" = "1",
                                     "Beta B.1.351" = "2",
                                     "Gamma P.1" = "3",
                                     "Delta B.1.617.2" = "4",
                                     "Omicron B.1.1.529" = "5",
                                     "Iota B.1.526" = "6",
                                     "Kappa B.1.617.1" = "7",
                                     "Lambda C.37" = "8",
                                     "Mu B.1.621" = "9",
                                     "Variant not listed" = "10"),
                   D1_IGAASS = c(Yes = "1", No = "0"),
                   D1_IGGASS = c(Yes = "1", No = "0")
                   ) %>%
  to_factor()

glimpse(covic19.data)
