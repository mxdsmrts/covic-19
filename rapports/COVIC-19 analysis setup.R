
library(tidyverse)
library(readxl)
library(labelled)

#### Import data ####
t1_inc <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T1_INC_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t1_rando <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T1_4_RANDO_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t2_d1 <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T2_D1_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t3_d3 <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T3_D3_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t4_d14 <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T4_D14_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t5_d28 <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T5_D28_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t6_d90 <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T6_D90_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t7_d180 <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T7_D180_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t8_hp <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T8_HP_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t9_cmtab <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T9_CMTAB_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t10_eos <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T10_EOS_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t11_ae <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T11_AE_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t12_epro <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/T12_EPRO_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")
t21_pastcovidinf <- read.delim("~/DATA/COVIC-19/SOURCES/final02/data/eCRF/TBL_21_PASTCOVIDINF_ANSI.csv", fileEncoding = "Latin1", sep = ";", dec = ".")

adjudicated_ei <- read_xlsx("~/DATA/COVIC-19/SOURCES/final02/data/covic-19_adjudication.xlsx") |> 
  mutate(AENUMGLOBAL = paste(SUBJECT_REF,`Event identification`, sep = "-"))

# Merging the data
covic19.data <- t1_inc %>% merge(t1_rando, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t2_d1, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t3_d3, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t4_d14, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t5_d28, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t6_d90, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t7_d180, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) %>%
  merge(t10_eos, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) |>
  merge(t12_epro, by = "SUBJECT_REF", suffixes = c("",".y")) %>% select(!ends_with(".y")) 


# Merging AE and adjudication data

adjudicated_ei <- adjudicated_ei |> 
  full_join(t11_ae, by = "AENUMGLOBAL", suffix = c("",".y")) |> 
  select(!ends_with(".y")) |> 
  select(SUBJECT_REF, AEINCDAT, AENUMGLOBAL, `Associated SAE`, AESTDAT, AESHOSPSTDAT,
         AESHOSPENDAT, adj_n1, adj_n2, adj_n3, Relatedness) |> 
    mutate(AESTDAT = as.Date(AESTDAT, format = "%d/%m/%Y"))

# Removing patients who withdrew consent ####
covic19.data <- covic19.data %>% filter(EOS_NCOMPLETED != 3 | is.na(EOS_NCOMPLETED)) |>
  filter(!SUBJECT_REF %in% c("DE-04-019","DE-04-037"))
t11_ae <- t11_ae |> filter(!SUBJECT_REF %in% c("DE-04-019","DE-04-037")) 

# Selection safety population
safety_pop <- covic19.data |> 
  filter(!SUBJECT_REF %in% c("DE-04-019","DE-04-037"))  |> 
  select(SUBJECT_REF, RANDOMIZATION_R1)

# test <- covic19.data |> select(SUBJECT_REF, RANDOMIZATION_R1, EOS_COMPLETED, EOS_NCOMPLETED, PLASMA_NB, 
#                               PLASMA1_RATE, PLASMA2_RATE, PLASMA1_DISC, 
#                                PLASMA1_DISC_TEMP, PLASMA1_DISC_PERM, PLASMA2_DISC, PLASMA2_DISC_TEMP, PLASMA2_DISC_PERM, PLASMA1_TEMP,
#                                PLASMA1_BP, PLASMA1_ALL, PLASMA1_ALL_TYP, PLASMA1_AE_YN, PLASMA2_TEMP, PLASMA2_BP, PLASMA2_ALL, PLASMA2_ALL_TYP, 
#                                PLASMA2_AE_YN)


# Removing patients enrolled in error
covic19.data <- covic19.data |> filter(SUBJECT_REF != "DE-14-001")

# Modified ITT population
patients <- covic19.data |> select(SUBJECT_REF, RANDOMIZATION_R1)


# Dealing with missing data ####
covic19.data <- covic19.data |> 
  mutate(across(where(is.character), ~na_if(.,"ND"))) |> 
  mutate(COVID19_VAC = na_if(COVID19_VAC, 2)) |>
  mutate(across(c(D1_FEV:D1_BLEED, D1_OTH), ~na_if(., 2))) |> 
  mutate(D1_PCRASSTYP1 = replace(D1_PCRASSTYP1, D1_PCRASSTYP1 %in% c("ND","11",""), NA)) |> 
  mutate(COVID19_1STVAC_TYP = replace(COVID19_1STVAC_TYP, COVID19_1STVAC_TYP %in% c("ND","","6"), NA)) |> 
  mutate(COVID19_2NDVAC_TYP = replace(COVID19_2NDVAC_TYP, COVID19_2NDVAC_TYP %in% c("ND","","6"), NA)) |> 
  mutate(COVID19_3RDVAC_TYP = replace(COVID19_3RDVAC_TYP, COVID19_3RDVAC_TYP %in% c("ND","","6"), NA)) |> 
  mutate(COVID19_4RDVAC_TYP = replace(COVID19_4RDVAC_TYP, COVID19_4RDVAC_TYP %in% c("ND","","6"), NA)) |> 
  
  mutate(D1_IGAASS = replace(D1_IGAASS, D1_IGAASS %in% c("","ND","2"), NA),
         D1_IGGASS = replace(D1_IGGASS, D1_IGGASS %in% c("","ND","2"), NA),
         D14_IGAASS = replace(D14_IGAASS, D14_IGAASS %in% c("","ND","2"), NA),
         D14_IGGASS = replace(D14_IGGASS, D14_IGGASS %in% c("","ND","2"), NA)
         ) |> 
  mutate() |> 
  mutate(D1_STATE = as.numeric(D1_STATE)) |> 
  mutate(D3_STATE = as.numeric(D3_STATE)) |> 
  mutate(D14_STATE = as.numeric(D14_STATE)) |> 
  mutate(D28_STATE = as.numeric(D28_STATE))

test <- covic19.data |> select(COVID19_1STVAC_TYP, COVID19_2NDVAC_TYP, COVID19_3RDVAC_TYP, COVID19_4RDVAC_TYP)

# Dates
dates <- c("COVIDAT", "PCRDAT", "COVID19_PCR_DATIM", "COVID19_1STVAC_DAT", "COVID19_2NDVAC_DAT", "COVID19_3RDVAC_DAT", 
           "COVID19_4RDVAC_DAT", "EOSDAT")
datetimes <- c("INCDAT", "RANDODAT", "PLASMA1_STDAT", "PLASMA1_ENDDAT", "PLASMA2_STDAT", "PLASMA2_ENDDAT")
covic19.data <- covic19.data %>% mutate(across(all_of(dates), \(x) as.Date(x, format = "%d/%m/%Y") )) %>%
  mutate(across(all_of(datetimes), \(x) strptime(x, format = "%d%b%Y:%H:%M") ))

t8_hp <- t8_hp |> mutate(HP_ICUSTDAT = as.Date(HP_ICUSTDAT, format = "%d/%m/%Y"))
t9_cmtab <- t9_cmtab |> mutate(COVID19_CMSTDAT = as.Date(COVID19_CMSTDAT, format = "%d/%m/%Y"))

# Numeric
numeric <- c("D1_PHOS_V", "D1_CRP_V", "D1_DDIMER_V", "D1_IL6_V", "D1_WBC_V", "D1_LYMCE_V", "D1_NEUT_V", "D1_LNR_V", "D1_HGB_V",
             
             "D1_IGGASSRATIO_V",
             
             "PLASMA1_RATE", "PLASMA2_RATE", "PLASMA1_TEMP", "PLASMA1_BP","PLASMA2_TEMP", "PLASMA2_BP",
  
  "PCFS_D1", "PCFS_D28", "PCFS_D180",
             "C19_YRS26_D1", "C19_YRS33_D1", "C19_YRS43_D1", "C19_YRS55_D1", "C19_YRS69_D1", "C19_YRS76_D1", "C19_YRS82_D1", "C19_YRS92_D1", "C19_YRS105_D1",
             "C19_YRS113_D1", "C19_YRS122_D1", "C19_YRS132_D1", "C19_YRS142_D1", "C19_YRS152_D1",
             "C19_YRS162_D1",
             
             "C19_YRS25_D1", "C19_YRS32_D1", "C19_YRS42_D1", "C19_YRS54_D1",
             "C19_YRS68_D1", "C19_YRS75_D1", "C19_YRS81_D1", "C19_YRS91_D1", "C19_YRS104_D1",
             "C19_YRS112_D1", "C19_YRS121_D1", "C19_YRS131_D1", "C19_YRS141_D1", "C19_YRS151_D1",
             "C19_YRS221_D1", "C19_YRS223_D1", "C19_YRS224_D1", "C19_YRS225_D1", "C19_YRS226_D1", "C19_YRS227_D1",
             
             "C19_YRS25_D28", "C19_YRS32_D28", "C19_YRS42_D28", "C19_YRS54_D28",
             "C19_YRS68_D28", "C19_YRS75_D28", "C19_YRS81_D28", "C19_YRS91_D28", "C19_YRS104_D28",
             "C19_YRS112_D28", "C19_YRS121_D28", "C19_YRS131_D28", "C19_YRS141_D28", "C19_YRS151_D28",
             "C19_YRS221_D28", "C19_YRS223_D28", "C19_YRS224_D28", "C19_YRS225_D28", "C19_YRS226_D28", "C19_YRS227_D28",
             
             "C19_YRS25_D180", "C19_YRS32_D180", "C19_YRS42_D180", "C19_YRS54_D180",
             "C19_YRS68_D180", "C19_YRS75_D180", "C19_YRS81_D180", "C19_YRS91_D180", "C19_YRS104_D180",
             "C19_YRS112_D180", "C19_YRS121_D180", "C19_YRS131_D180", "C19_YRS141_D180", "C19_YRS151_D180",
             "C19_YRS221_D180", "C19_YRS223_D180", "C19_YRS224_D180", "C19_YRS225_D180", "C19_YRS226_D180", "C19_YRS227_D180",
             
             "COVID19_1STVAC_TYP","COVID19_2NDVAC_TYP","COVID19_3RDVAC_TYP","COVID19_4RDVAC_TYP")
covic19.data <- covic19.data %>% mutate(across(all_of(numeric), \(x) as.numeric(x) )) 

# Factor
covic19.data <- covic19.data |> mutate(RANDOMIZATION_R1 = relevel(as.factor(RANDOMIZATION_R1), ref = "standard")) 

# OMS scale labels
who_scale_labels <- c("0 Uninfected; no viral RNA detected" = 1,
               "1 Asymptomatic; viral RNA detected" = 2,
               "2 Symptomatic; independent" = 3,
               "3 Symptomatic; assistance needed" = 4,
               "4 Hospitalised; no oxygen therapy*" = 5,
               "5 Hospitalised; oxygen by mask or nasal prongs" = 6,
               "6 Hospitalised; oxygen by NIV or high flow" = 7,
               "7 Intubation and mechanical ventilation, pO2/FiO2 ≥150 or SpO2/FiO2 ≥ 200" = 8,
               "8 Mechanical ventilation pO2/FIO2 <150 (SpO2/FiO2 <200) or vasopressors" = 9,
               "9 Mechanical ventilation pO2/FiO2 <150 and vasopressors, dialysis, or ECMO" = 10,
               "10 Dead" = 11)


#### Recoding #####

# Recode Omicron types
covic19.data <- covic19.data |> 
  mutate(OMICRONTYP = case_when(OMICRONTYP_C1 == 1 ~ "BA.1 lineage",
                                OMICRONTYP_C2 == 1 ~ "BA.2 lineage",
                                OMICRONTYP_C3 == 1 ~ "BA.3 lineage",
                                OMICRONTYP_C4 == 1 ~ "BA.4 lineage",
                                OMICRONTYP_C5 == 1 ~ "BA.5 lineage",
                                OMICRONTYP_C6 == 1 ~ "XBB / XBB.1",
                                OMICRONTYP_C7 == 1 ~ "BQ1 / BQ1.1", 
                                OMICRONTYP_C8 == 1 ~ "BF.7 / BF.14",
                                OMICRONTYP_C10 == 1 ~ "CH1.1"))


## Integrate sequencing data from center FR02 #####
covic19.data <- covic19.data |> 
  mutate(D1_PCRASSTYP1 = case_when(
    SUBJECT_REF == "FR-02-003" ~ "10",
    SUBJECT_REF == "FR-02-004" ~ "10",
    SUBJECT_REF == "FR-02-001" ~ "10",
    SUBJECT_REF == "FR-02-002" ~ "10",
    SUBJECT_REF == "FR-02-005" ~ "5",
    SUBJECT_REF == "FR-02-006" ~ "10",
    SUBJECT_REF == "FR-02-007" ~ "10",
    .default = D1_PCRASSTYP1)
    ) |> 
  mutate(D1_PCRASSTXT = case_when(
    SUBJECT_REF == "FR-02-003" ~ "OMICRON EG.5.1",
    SUBJECT_REF == "FR-02-004" ~ "OMICRON EG.5.1",
    SUBJECT_REF == "FR-02-001" ~ "OMICRON EG.1",
    SUBJECT_REF == "FR-02-002" ~ "OMICRON FW.2",
    SUBJECT_REF == "FR-02-005" ~ "OMICRON XBB.2.3.11",
    SUBJECT_REF == "FR-02-007" ~ "OMICRON JN.1",
    .default = D1_PCRASSTXT)
  ) |> 
  mutate(OMICRONTYP = case_when(
    SUBJECT_REF == "FR-02-005" ~ "XBB / XBB.1",
    .default = OMICRONTYP)
  )

# test <- covic19.data |> select(SUBJECT_REF, D1_PCRASSTYP1, OMICRONTYP, D1_PCRASSTXT)

# Recode concomittant medications DCI
## Table des DCI médicamenteuses

# COVID19_CMDCI_recode <- t9_cmtab %>% select(SUBJECT_REF, RANDOMIZATION_R1, COVID19_CMDCI) %>%
#   count(COVID19_CMDCI) %>%
#   write_csv2("~/DATA/COVIC-19/SORTIES/COVID19_CMDCI_recoding.csv")

## Import recoded medications
# CM_recoding <- read_csv2("~/DATA/COVIC-19/SOURCES/2023 07 20 COVID19_CM_recoding_md.csv")
CM_recoding <- read_xlsx("~/DATA/COVIC-19/SOURCES/covic_freetxt_coding_2024-08-16.xlsx", sheet = "CM - distinct class, dci, ind")
# CM_recoding <- read_xlsx("~/DATA/COVIC-19/SOURCES/final02/data/covic-19_trt_20240710.xlsx")

patients_rando <- covic19.data |> select(SUBJECT_REF, INCDAT)
CM_recoding <- CM_recoding |> select(COVID19_CMCLASS, COVID19_CMDCI, `Medication class`, Medication) |> 
  unique()

# Medication relative to CCP
patients_ccpdat <- covic19.data |> select(SUBJECT_REF, PLASMA1_STDAT)

t9_cmtab2 <- left_join(t9_cmtab, CM_recoding, by = c("COVID19_CMCLASS", "COVID19_CMDCI"), relationship = "many-to-many") |> 
  left_join(patients_ccpdat, by = "SUBJECT_REF") |> 
  mutate(delai_trt_ccp = as.Date(COVID19_CMSTDAT) - as.Date(PLASMA1_STDAT)) |> 
  mutate(classification_trt = case_when(delai_trt_ccp <= -7 ~ "0 Earlier",
                                        delai_trt_ccp > -7 & delai_trt_ccp < 0 ~ "1 Week before CCP",
                                        delai_trt_ccp >= 0 & delai_trt_ccp < 7 ~ "2 First week",
                                        delai_trt_ccp >= 7 ~ "3 Following weeks"))

# Medication relative to enrolment
t9_cmtab <- left_join(t9_cmtab, CM_recoding, by = c("COVID19_CMCLASS", "COVID19_CMDCI"), relationship = "many-to-many") |> 
  left_join(patients_rando, by = "SUBJECT_REF") |> 
  mutate(delai_trt = as.Date(COVID19_CMSTDAT) - as.Date(INCDAT)) |> 
  mutate(classification_trt = case_when(delai_trt <= -7 ~ "0 Earlier",
                                        delai_trt > -7 & delai_trt < 0 ~ "1 Week before enrollment",
                                        delai_trt >= 0 & delai_trt < 7 ~ "2 First week",
                                        delai_trt >= 7 ~ "3 Following weeks"))




## Import recorded adverse events
ae_recoded <- read_csv2("~/DATA/COVIC-19/SOURCES/covic-19_ae_recoded_20240722.csv") |> select(AENUMGLOBAL, AETERM_CTCAE_recoded)
t11_ae_recoded <- t11_ae |> left_join(ae_recoded, by = "AENUMGLOBAL")


#### Variable creation ####
covic19.data <- covic19.data |>
  mutate(days_vac1_rando = as.Date(RANDODAT) - COVID19_1STVAC_DAT) |> # Time from first vaccination dose to randomisation
  mutate(days_vac2_rando = as.Date(RANDODAT) - COVID19_2NDVAC_DAT) |> # Time from 2nd vaccination dose to randomisation
  mutate(days_vac3_rando = as.Date(RANDODAT) - COVID19_3RDVAC_DAT) |> # Time from 3rd vaccination dose to randomisation
  mutate(days_vac4_rando = as.Date(RANDODAT) - COVID19_4RDVAC_DAT) |> # Time from 4th vaccination dose to randomisation
  mutate(vac_1dose = if_else(!is.na(COVID19_1STVAC_TYP), "Yes", NA)) %>%
  mutate(vac_2dose = if_else(!is.na(COVID19_2NDVAC_TYP), "Yes", NA)) %>%
  mutate(vac_3dose = if_else(!is.na(COVID19_3RDVAC_TYP), "Yes", NA)) %>%
  mutate(vac_4dose = if_else(!is.na(COVID19_4RDVAC_TYP), "Yes", NA)) %>%
  mutate(nb_vacdose = case_when(!is.na(COVID19_4RDVAC_TYP) ~ 4,
                                !is.na(COVID19_3RDVAC_TYP) ~ 3,
                                !is.na(COVID19_2NDVAC_TYP) ~ 2,
                                !is.na(COVID19_1STVAC_TYP) ~ 1,
                                COVID19_VAC == 0 ~ 0)) %>%
  mutate(lastvac_dat = pmax(COVID19_1STVAC_DAT, COVID19_2NDVAC_DAT, COVID19_3RDVAC_DAT, COVID19_4RDVAC_DAT), na.rm = TRUE) |> # Date of last vaccine dose 
  mutate(days_lastvac_rando = as.Date(RANDODAT) - lastvac_dat) |>  # Time from last vaccine dose to randomisation
  rowwise() |> 
  mutate(homologous_vac = case_when(
    n_distinct(c_across(c("COVID19_1STVAC_TYP", "COVID19_2NDVAC_TYP", "COVID19_3RDVAC_TYP", "COVID19_4RDVAC_TYP")), na.rm = TRUE) == 0 ~ NA,
    n_distinct(c_across(c("COVID19_1STVAC_TYP", "COVID19_2NDVAC_TYP", "COVID19_3RDVAC_TYP", "COVID19_4RDVAC_TYP")), na.rm = TRUE) == 1 ~ "Yes",
    n_distinct(c_across(c("COVID19_1STVAC_TYP", "COVID19_2NDVAC_TYP", "COVID19_3RDVAC_TYP", "COVID19_4RDVAC_TYP")), na.rm = TRUE) >= 2 ~ "No")
    ) |> 
  ungroup() |> 
  mutate(days_cov_rando = as.Date(RANDODAT) - COVIDAT) %>% # Time from symptom onset to randomisation
  mutate(days_first_pcr_rando = as.Date(RANDODAT) - COVID19_PCR_DATIM) %>% # Time from first PCR to randomisation
  mutate(days_last_pcr_rando = as.Date(RANDODAT) - PCRDAT) %>% # Time from last PCR to randomisation
  mutate(days_rando_plasma = as.Date(PLASMA1_STDAT) - as.Date(RANDODAT)) %>% # Time from randomisation to plasma
  mutate(days_cov_plasma = as.Date(PLASMA1_STDAT) - COVIDAT) %>% # Time from symptom onset to plasma
  mutate(plasma1_duration = PLASMA1_ENDDAT - PLASMA1_STDAT) |>  # Plasma 1 infusion duration
  mutate(plasma2_duration = PLASMA2_ENDDAT - PLASMA2_STDAT) |> # Plasma 2 infusion duration
  mutate(plasma_duration = plasma1_duration + plasma2_duration) |> # Plasma 1+2 infusion duration

  mutate(status = case_when(EOS_NCOMPLETED == 1 ~ 1, .default = 0)) %>%
  mutate(days_rando_followup = EOSDAT - as.Date(RANDODAT))

#### Adding primary endpoint ####

adjudicated_events <- adjudicated_ei |> filter(Relatedness == "Related")
covic19.data <- covic19.data |> left_join(adjudicated_events, by = "SUBJECT_REF") |> 
  mutate(days_rando_sae = AESTDAT - as.Date(RANDODAT))

covic19.data <- covic19.data |>
  mutate(
    event = case_when(
      D3_OUTCOM == 2 ~ 1,
      D14_OUTCOM == 2 ~ 1,
      D28_OUTCOM == 2 ~ 1,
      EOS_NCOMPLETED == 1 & days_rando_followup <= 28 ~ 1,
      Relatedness == "Related" & days_rando_sae <= 28 ~ 1,
      .default = 0
    )
  )

test <- covic19.data |> select(SUBJECT_REF, RANDOMIZATION_R1, days_rando_sae, Relatedness, event)

#### Adding subgroup analyses variables ####

covic19.data <- covic19.data |> 
  mutate(age_median = cut(AGE_V,
                          breaks = quantile(covic19.data$AGE_V, probs = seq(0, 1, by = 0.50)),
                          include.lowest=TRUE)) |> 
  mutate(variant_omicron = case_when(D1_PCRASSTYP1 %in% c(5,10) ~ 1,
                                     D1_PCRASSTYP1 %in% c(1:4,6:9) ~ 2)) 

# medication classes
pat_med_class <- t9_cmtab |> select(SUBJECT_REF, `Medication class`) |>
  filter(!is.na(`Medication class`)) |>
  unique() |>
  dplyr::count(SUBJECT_REF, `Medication class`) |>
  pivot_wider(id_cols = SUBJECT_REF, names_from = `Medication class`, values_from = n, values_fill = 0) |>
  merge(patients, by = "SUBJECT_REF", all.y = TRUE) |>
  mutate(across(everything(), ~replace_na(., 0)))

covic19.data <- covic19.data |> left_join(pat_med_class, by = "SUBJECT_REF", suffix = c("",".y")) |> select(!ends_with(".y")) 

temp <- covic19.data |> select(SUBJECT_REF, RANDOMIZATION_R1, AGE, AGE_V, age_median, SEX, D1_PCRASSTYP1, variant_omicron, COVID19_VAC,
                               `COVID-19 monoclonal antibodies`,
                               EOS_NCOMPLETED, AESTDAT, days_rando_sae, Relatedness, event)
# View(temp)


####  Adding secondary endpoints ####
## Event at day 14
covic19.data <- covic19.data |>
  mutate(
    event14 = case_when(
      D3_OUTCOM == 2 ~ 1,
      D14_OUTCOM == 2 ~ 1,
      D28_OUTCOM == 2 ~ 1,
      EOS_NCOMPLETED == 1 &
        days_rando_followup <= 14 ~ 1,
      Relatedness == "Related" &
        days_rando_sae <= 14 ~ 1,
      .default = 0
    )
  )


## OMS5+ for COVID at day 14
covic19.data <- covic19.data |>
  mutate(
    oms5_14 = case_when(
      D3_OUTCOM == 2 ~ 1,
      D14_OUTCOM == 2 ~ 1,
      D28_OUTCOM == 2 ~ 1,
      EOS_NCOMPLETED == 1 & days_rando_followup <= 14 ~ 1,
      Relatedness == "Related" & D3_STATE >= 6 & days_rando_sae <= 14 ~ 1,
      Relatedness == "Related" & D14_STATE >= 6 & days_rando_sae <= 14 ~ 1,
      .default = 0
    )
  )

## OMS5+ for COVID at day 28
covic19.data <- covic19.data |>
  mutate(
    oms5_28 = case_when(
      D3_OUTCOM == 2 ~ 1,
      D14_OUTCOM == 2 ~ 1,
      D28_OUTCOM == 2 ~ 1,
      EOS_NCOMPLETED == 1 & days_rando_followup <= 14 ~ 1,
      Relatedness == "Related" & D3_STATE >= 6 & days_rando_sae <= 28 ~ 1,
      Relatedness == "Related" & D14_STATE >= 6 & days_rando_sae <= 28 ~ 1,
      .default = 0
    )
  )

## All cause mortality at day 28
covic19.data <- covic19.data %>% mutate(death28 = case_when(D3_OUTCOM == 2 ~ 1,
                                                                      D3_STATE == 11 ~ 1,
                                                                      D14_OUTCOM == 2 ~ 1,
                                                                      D14_STATE == 11 ~ 1,
                                                                      D28_OUTCOM == 2 ~ 1,
                                                                      D28_STATE == 11 ~ 1,
                                                                      EOS_NCOMPLETED == 1 & days_rando_followup <= 28 ~ 1,
                                                                      .default = 0))
## All cause mortality at day 90
covic19.data <- covic19.data %>% mutate(death90 = case_when(D3_OUTCOM == 2 ~ 1,
                                                                      D3_STATE == 11 ~ 1,
                                                                      D14_OUTCOM == 2 ~ 1,
                                                                      D14_STATE == 11 ~ 1,
                                                                      D28_OUTCOM == 2 ~ 1,
                                                                      D28_STATE == 11 ~ 1,
                                                                      D90_VS == 2 ~ 1,
                                                                      EOS_NCOMPLETED == 1 & days_rando_followup <= 90 ~ 1,
                                                                      .default = 0))
## All cause mortality at day 180
covic19.data <- covic19.data %>% mutate(death180 = case_when(D3_OUTCOM == 2 ~ 1,
                                                                      D3_STATE == 11 ~ 1,
                                                                      D14_OUTCOM == 2 ~ 1,
                                                                      D14_STATE == 11 ~ 1,
                                                                      D28_OUTCOM == 2 ~ 1,
                                                                      D28_STATE == 11 ~ 1,
                                                                      D90_VS == 2 ~ 1,
                                                                      D180_SV == 2 ~ 1,
                                                                      EOS_NCOMPLETED == 1 & days_rando_followup <= 180 ~ 1,
                                                                      .default = 0))

## Oxygen at day 14
covic19.data <- covic19.data %>% mutate(o2_14 = case_when(D3_STATE >= 6 ~ 1,
                                                                       D14_STATE >= 6 ~ 1,
                                                                       .default = 0))
## Oxygen at day 28
covic19.data <- covic19.data %>% mutate(o2_28 = case_when(D3_STATE >= 6 ~ 1,
                                                                       D14_STATE >= 6 ~ 1,
                                                                       D28_STATE >= 6 ~ 1,
                                                                       .default = 0))

## NIV at day 14
covic19.data <- covic19.data %>% mutate(niv_14 = case_when(D3_STATE >= 7 ~ 1,
                                                                       D14_STATE >= 7 ~ 1,
                                                                       .default = 0))
## NIV at day 28
covic19.data <- covic19.data %>% mutate(niv_28 = case_when(D3_STATE >= 7 ~ 1,
                                                                       D14_STATE >= 7 ~ 1,
                                                                       D28_STATE >= 7 ~ 1,
                                                                       .default = 0))

## Mechanical ventilation at day 14
covic19.data <- covic19.data %>% mutate(mv_14 = case_when(D3_STATE >= 8 ~ 1,
                                                                       D14_STATE >= 8 ~ 1,
                                                                       .default = 0))
## Mechanical ventilation at day 28
covic19.data <- covic19.data %>% mutate(mv_28 = case_when(D3_STATE >= 8 ~ 1,
                                                                       D14_STATE >= 8 ~ 1,
                                                                       D28_STATE >= 8 ~ 1,
                                                                       .default = 0))

# View(covic19.data[c("SUBJECT_REF","RANDOMIZATION_R1","ANTIVIRAL", "D3_OUTCOM", "D3_STATE", "D14_OUTCOM", "D14_STATE", "D28_OUTCOM", "D28_PCRCT", "D28_PCRCT_V",
#                   "D90_VS", "D180_SV",
#                  "EOS_NCOMPLETED", "EOSDAT", "days_rando_sae", "Relatedness", "days_rando_followup", "event", "event14","oms5_14", "oms5_28", "death28", "death90", "death180" )])


## Change in OMS Score day 14
covic19.data <- covic19.data %>% mutate(oms_change_14 = D14_STATE - D1_STATE)

## Change in OMS Score day 28
covic19.data <- covic19.data %>% mutate(oms_change_28 = D28_STATE - D1_STATE)

# View(covic19.data[c("SUBJECT_REF","RANDOMIZATION_R1", "D1_STATE", "D3_STATE", "D14_STATE", "D28_STATE", "oms_change_14", "oms_change_28")])
# View(covic19.data[c("SUBJECT_REF","RANDOMIZATION_R1", "D1_STATE", "D3_STATE", "D14_STATE", "D28_STATE")])

## Length of hospital stay censored at D28
### not calculable

## ITU admission day 14 and day 28
d_icu <- t8_hp |> 
  filter(HP_ICU == 1) |> 
  select(SUBJECT_REF, HP_ICU, HP_ICUSTDAT) |> 
  arrange(HP_ICUSTDAT) |> 
  distinct(HP_ICU, .keep_all = TRUE)

covic19.data <- covic19.data |> 
  merge(d_icu, by = "SUBJECT_REF", all.x = TRUE) |>
  mutate(days_rando_icu = HP_ICUSTDAT - as.Date(RANDODAT)) |>
  mutate(icu_14 = case_when(HP_ICU == 1 & days_rando_icu <= 14 ~ 1, .default = 0)) |>
  mutate(icu_28 = case_when(HP_ICU == 1 & days_rando_icu <= 28 ~ 1, .default = 0))

# View(t8_hp[c("SUBJECT_REF","RANDOMIZATION_R1","MODULE_T8_HP", "ID86S44V39", "HP_STATE", "HP_ICU", "HP_ICUSTDAT", "HP_ICUENDAT", "HP_ENDAT", "HP_OUTCOMEDAT")])
# View(covic19.data[c("SUBJECT_REF","RANDOMIZATION_R1", "HP_ICU", "HP_ICUSTDAT", "days_rando_icu")])


## Long COVID measured by Long Covid Functional Scale
covic19.data <- covic19.data |> 
  mutate(diff_pcfs_d1_d28 = PCFS_D28 - PCFS_D1) |>
  mutate(diff_pcfs_d1_d180 = PCFS_D180 - PCFS_D1) |>
  mutate(pcfs2_d28 = if_else(PCFS_D28 >= 2, 1, 0)) |>
  mutate(pcfs2_d180 = if_else(PCFS_D180 >= 2, 1, 0))

# View(covic19.data[c("SUBJECT_REF", "RANDOMIZATION_R1", "PCFS_D1", "PCFS_D28", "PCFS_D180")])

## Long COVID measured by C19-YRS ####
covic19.data <- covic19.data |> rowwise() |>
  mutate(avg_breathless_pre = mean(c_across(c("C19_YRS1A2_D1_V", "C19_YRS1B2_D1_V", "C19_YRS1C2_D1_V")), na.rm = TRUE)) |>
  mutate(c19yrs_symptom_pre = sum(c_across(c("avg_breathless_pre", "C19_YRS26_D1", "C19_YRS33_D1", "C19_YRS43_D1", "C19_YRS55_D1",
                                             "C19_YRS69_D1", "C19_YRS76_D1", "C19_YRS82_D1", "C19_YRS92_D1", "C19_YRS105_D1")))) |>
  mutate(c19yrs_function_pre = sum(c_across(c("C19_YRS113_D1", "C19_YRS122_D1", "C19_YRS132_D1", "C19_YRS142_D1", "C19_YRS152_D1")))) |>
  mutate(c19yrs_overall_pre = as.numeric(C19_YRS162_D1)) |>
  
  mutate(avg_breathless_d1 = mean(c_across(c("C19_YRS1A1_D1_V", "C19_YRS1B1_D1_V", "C19_YRS1C1_D1_V")), na.rm = TRUE)) |>
  mutate(c19yrs_symptom_d1 = sum(c_across(c("avg_breathless_d1", "C19_YRS25_D1", "C19_YRS32_D1", "C19_YRS42_D1", "C19_YRS54_D1",
                                             "C19_YRS68_D1", "C19_YRS75_D1", "C19_YRS81_D1", "C19_YRS91_D1", "C19_YRS104_D1")))) |>
  mutate(c19yrs_function_d1 = sum(c_across(c("C19_YRS112_D1", "C19_YRS121_D1", "C19_YRS131_D1", "C19_YRS141_D1", "C19_YRS151_D1")))) |>
  mutate(c19yrs_additional_d1 = sum(c_across(C19_YRS221_D1:C19_YRS227_D1))) |>
  mutate(c19yrs_overall_d1 = as.numeric(C19_YRS161_D1)) |>

  mutate(avg_breathless_d28 = mean(c_across(c("C19_YRS1A1_D28_V", "C19_YRS1B1_D28_V", "C19_YRS1C1_D28_V")), na.rm = TRUE)) |>
  mutate(c19yrs_symptom_d28 = sum(c_across(c("avg_breathless_d28", "C19_YRS25_D28", "C19_YRS32_D28", "C19_YRS42_D28", "C19_YRS54_D28",
                                            "C19_YRS68_D28", "C19_YRS75_D28", "C19_YRS81_D28", "C19_YRS91_D28", "C19_YRS104_D28")))) |>
  mutate(c19yrs_function_d28 = sum(c_across(c("C19_YRS112_D28", "C19_YRS121_D28", "C19_YRS131_D28", "C19_YRS141_D28", "C19_YRS151_D28")))) |>
  mutate(c19yrs_additional_d28 = sum(c_across(C19_YRS221_D28:C19_YRS227_D28))) |>
  mutate(c19yrs_overall_d28 = as.numeric(C19_YRS161_D28)) |>
  
  mutate(avg_breathless_d180 = mean(c_across(c("C19_YRS1A1_D180_V", "C19_YRS1B1_D180_V", "C19_YRS1C1_D180_V")), na.rm = TRUE)) |>
  mutate(c19yrs_symptom_d180 = sum(c_across(c("avg_breathless_d180", "C19_YRS25_D180", "C19_YRS32_D180", "C19_YRS42_D180", "C19_YRS54_D180",
                                             "C19_YRS68_D180", "C19_YRS75_D180", "C19_YRS81_D180", "C19_YRS91_D180", "C19_YRS104_D180")))) |>
  mutate(c19yrs_function_d180 = sum(c_across(c("C19_YRS112_D180", "C19_YRS121_D180", "C19_YRS131_D180", "C19_YRS141_D180", "C19_YRS151_D180")))) |>
  mutate(c19yrs_additional_d180 = sum(c_across(C19_YRS221_D180:C19_YRS227_D180))) |>
  ungroup() |> 
  
  mutate(c19yrs_overall_d180 = as.numeric(C19_YRS161_D180)) |>
  
  mutate(diff_c19yrs_symptom_pre_d28 = c19yrs_symptom_d28 - c19yrs_symptom_pre) |>
  mutate(diff_c19yrs_function_pre_d28 = c19yrs_function_d28 - c19yrs_function_pre) |>
  mutate(diff_c19yrs_overall_pre_d28 = c19yrs_overall_d28 - c19yrs_overall_pre) |>
  
  mutate(diff_c19yrs_symptom_pre_d180 = c19yrs_symptom_d180 - c19yrs_symptom_pre) |>
  mutate(diff_c19yrs_function_pre_d180 = c19yrs_function_d180 - c19yrs_function_pre) |>
  mutate(diff_c19yrs_overall_pre_d180 = c19yrs_overall_d180 - c19yrs_overall_pre)
  
c19yrs_data <- covic19.data |> select(avg_breathless_pre:c19yrs_overall_d180)

## HR-QoL EQ-5D-5L
covic19.data <- covic19.data |> 
  mutate(diff_eq5dvas_d1_d28 = EQ5D5L_SC_D28_V - EQ5D5L_SC_D1_V) |>
  mutate(diff_eq5dvas_d1_d180 = EQ5D5L_SC_D1_V - EQ5D5L_D180_SC_V)

### Factors ####
covic19.data2 <- covic19.data |> 
  mutate(across(c(IN07:IN08C1, MH:MH_OTHER), ~ factor(., levels = c(0, 1)))) # Binary 0/1 variables

#### Variable labels ####
my_labels <- function (x) {
  x |> set_variable_labels(
    AGE_V = "Age",
    SEX = "Sex at birth",
    BLOODGRP = "Blood Group",
    COUNTRY = "Country of enrollment",
    days_cov_rando = "Time from symptom onset to randomisation (days)",
    days_first_pcr_rando = "Time from first PCR to randomisation (days)",
    days_last_pcr_rando = "Time from last PCR to randomisation (days)",
    days_rando_followup = "Time from randomisation to last followup (days)",
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
    MH = "Medical condition (any)",
    MH_BMI30 = "Obesity (BMI>30)",
    MH_CCD = "Chronic cardiac disease",
    MH_HTA = "Hypertension",
    MH_CP = "Chronic pulmonary disease (not asthma)",
    MH_ASTH = "Asthma",
    MH_CKC = "Chronic kidney disease (stage 1-4)",
    MH_CLD = "Chronic liver disease",
    MH_CND = "Chronic neurological disease",
    MH_PSO = "Rheumatoid disease, lupus or psoriasis",
    MH_CD = "Cerebrovascullar disease",
    MH_HIV = "HIV",
    MH_DIAB = "Diabetes",
    MH_TOBACCO = "Current smoker",
    MH_TUBER = "Tuberculosis",
    MH_SPLEEN = "Asplenia or spleen disease",
    MH_MNEO = "Malignant neoplasm",
    MH_OTHER = "Other",
    COVID19_VAC = "COVID-19 Vaccination",
    nb_vacdose = "Nb doses",
    COVID19_1STVAC_TYP = "Manufacturer of 1st dose",
    COVID19_2NDVAC_TYP = "Manufacturer of 2nd dose",
    COVID19_3RDVAC_TYP = "Manufacturer of 3rd dose",
    COVID19_4RDVAC_TYP = "Manufacturer of 4th dose",
    days_vac1_rando = "Time from 1st dose to randomisation (days)",
    days_vac2_rando = "Time from 2nd dose to randomisation (days)",
    days_vac3_rando = "Time from 3rd dose to randomisation (days)",
    days_vac4_rando = "Time from 4th dose to randomisation (days)",
    days_lastvac_rando = "Time from last vaccine dose to randomisation (days)",
    homologous_vac = "Homologous scheme (among vaccinated)",
    D1_FEV = "Fever",
    D1_COU = "Cough",
    D1_CSP = "Cough with sputum",
    D1_CHAE = "Cough with haemoptysis",
    D1_STH = "Sore throat",
    D1_SMELL = "Loss of smell",
    D1_TASTE = "Loss of taste",
    D1_RHINO = "Rhinorrhoea",
    D1_WHEE = "Wheezing",
    D1_CHPAIN = "Chest pain",
    D1_MYAL = "Myalgia",
    D1_ARTH = "Arthralgia",
    D1_FAT = "Fatigue/Malaise/Dizziness",
    D1_DYSP = "Dyspnea",
    D1_WALK = "Inability to walk",
    D1_CHEST = "Lower chest wall indrawing",
    D1_HEAD = "Headache",
    D1_ALTCONS = "Altered consciousness/Confusion",
    D1_SEIZ = "Seizure",
    D1_ABPAIN = "Abdominal pain",
    D1_VOMIT = "Vomiting/Nausea",
    D1_DIAR = "Diarrhoea",
    D1_CONJ = "Conjunctivitis",
    D1_SKI = "Skin rash",
    D1_SKIUL = "Skin ulcers",
    D1_LPD = "Lymphadenopathy",
    D1_BLEED = "Haemorrhage",
    D1_OTH = "Other",
    D1_COVIDINF90 = "Past Covid-19 infections, > 90 days before study enrollment",
    D1_COVID90D_NB = "How many",
    D1_STATE = "OMS 10-point scale",
    WEIGHT_V = "Weight (kg)",
    HEIGHT_V = "Height (cm)",
    BMI_V = "BMI (kg/m2)",
    TEMP_V = "Body temperature (highest, °C)",
    HR_V = "Heart rate (highest, bpm)",
    SYSBP_V = "Systolic blood pressure (lowest, mmHg)",
    DIABP_V = "Diastolic blood pressure (lowest, mmHg)",
    RR_V = "Respiratory rate (highest, b/min)",
    SPO2_V = "SpO2 in room air (lowest, %)",
    D1_CREAT_V = "Serum creatinine (mg/L)",
    D1_ALT_V = "ALT/SGPT (IU/L)",
    D1_AST_V = "AST/SGOT (IU/L)",
    D1_CA_V = "Calcium (mg/dL)",
    D1_PHOS_V = "Phosphate (mg/dL)",
    D1_GLUC_V = "Glucose (mg/dL)",
    D1_UREA_V = "Urea (mg/dL)",
    D1_CRP_V = "CRP (mg/L)",
    D1_LDH_V = "LDH (IU/L)",
    D1_DDIMER_V = "D-Dimers (µg/L)",
    D1_FERRITIN_V = "Ferritin (µg/L)",
    D1_NA_V = "Sodium (mmol/L)",
    D1_K_V = "Potassium (mmol/L)",
    D1_CL_V = "Chloride (mmol/L)",
    D1_HCO3_V = "Bicarbonate (mmol/L)",
    D1_IL6_V = "IL-6 (pg/mL)",
    D1_WBC_V = "WBC count (/mm3)",
    D1_LYMCE_V = "Lymphocytes (/mm3)",
    D1_LYMCEPC_V = "Lymphocytes (%)",
    D1_NEUT_V = "Neutrophils (/mm3)",
    D1_NEUTPC_V = "Neutrophils (%)",
    D1_LNR_V = "Lymphocytes to neutrophils ratio",
    D1_HGB_V = "Hemoglobin (g/dL)",
    D1_PLAT_V = "Platelets (/mm3)",
    D1_APTT_V = "aPTT (s)",
    D1_FIBRINOGEN_V = "Fibrinogen (g/L)",
    D1_INR_V = "Prothrombin (%)",
    D1_PCRASS = "D1 SARS-CoV-2 PCR",
    D1_PCRASSRES = "D1 SARS-CoV-2 PCR Result",
    D1_PCRASSCT_V = "D1 Ct value",
    D1_PCRASSTYP1 = "Variant",
    OMICRONTYP = "Omicron variants",
    D1_PCRASSTYP2 = "VOIs",
    D1_PCRASSTXT = "Variant not listed",
    D1_IGAASS = "D1 SARS-CoV-2 IgA",
    D1_IGAASSRATIO_V = "IgA ELISA OD ratio",
    D1_IGGASS = "D1 SARS-CoV-2 IgG",
    D1_IGGASSRATIO_V = "IgG ELISA OD ratio",
    D14_CREAT_V = "Serum creatinine (mg/L)",
    D14_ALT_V = "ALT/SGPT (IU/L)",
    D14_AST_V = "AST/SGOT (IU/L)",
    D14_CA_V = "Calcium (mg/dL)",
    D14_PHOS_V = "Phosphate (mg/dL)",
    D14_GLUC_V = "Glucose (mg/dL)",
    D14_UREA_V = "Urea (mg/dL)",
    D14_CRP_V = "CRP (mg/L)",
    D14_LDH_V = "LDH (IU/L)",
    D14_DDIMER_V = "D-Dimers (µg/L)",
    D14_FERRITIN_V = "Ferritin (µg/L)",
    D14_NA_V = "Sodium (mmol/L)",
    D14_K_V = "Potassium (mmol/L)",
    D14_CL_V = "Chloride (mmol/L)",
    D14_HCO3_V = "Bicarbonate (mmol/L)",
    D14_IL6_V = "IL-6 (pg/mL)",
    D14_WBC_V = "WBC count (/mm3)",
    D14_LYMCE_V = "Lymphocytes (/mm3)",
    D14_LYMCEPC_V = "Lymphocytes (%)",
    D14_NEUT_V = "Neutrophils (/mm3)",
    D14_NEUTPC_V = "Neutrophils (%)",
    D14_LNR_V = "Lymphocytes to neutrophils ratio",
    D14_HGB_V = "Hemoglobin (g/dL)",
    D14_PLAT_V = "Platelets (/mm3)",
    D14_APTT_V = "aPTT (s)",
    D14_FIBRINOGEN_V = "Fibrinogen (g/L)",
    D14_INR_V = "Prothrombin (%)",
    D14_PCRASS = "D14 SARS-CoV-2 PCR",
    OMICRONTYP = "Omicron variants",
    D14_IGAASS = "D14 SARS-CoV-2 IgA",
    D14_IGAASSRATIO_V = "IgA ELISA OD ratio",
    D14_IGGASS = "D14 SARS-CoV-2 IgG",
    D14_IGGASSRATIO_V = "IgG ELISA OD ratio",
    PLASMA_NB = "No. plasma units received",
    D28_PCRASS = "D28 SARS-CoV-2 PCR",
    D28_PCRRES = "D28 SARC-CoV-2 PCR Result",
    D28_PCRCT_V = "D28 Ct value",
    days_rando_plasma = "Time from randomisation to plasma infusion (days)",
    days_cov_plasma = "Time from symptom onset to plasma infusion (days)",
    plasma1_duration = "Plasma 1 duration (min)",
    PLASMA1_RATE = "Plasma 1 rate of infusion (mL/min)",
    plasma2_duration = "Plasma 2 duration (min)",
    PLASMA2_RATE = "Plasma 2 rate of infusion (mL/min)",
    plasma_duration = "Total plasma infusion duration (min)",
    PLASMA1_DISC = "Plasma 1 discontinuation",
    PLASMA1_DISC_TEMP = "Temporary",
    PLASMA1_DISC_PERM = "Permanent",
    PLASMA2_DISC = "Plasma 2 discontinuation",
    PLASMA2_DISC_TEMP = "Temporary",
    PLASMA2_DISC_PERM = "Permanent",
    PLASMA1_TEMP = "Plasma 1 Body temp.",
    PLASMA1_BP = "Plasma 1 BP",
    PLASMA1_ALL = "Plasma 1 Allergy",
    PLASMA1_ALL_TYP = "specify",
    PLASMA1_AE_YN = "Plasma 1 AE",
    PLASMA2_TEMP = "Plasma 2 Body temp.",
    PLASMA2_BP = "Plasma 2 BP",
    PLASMA2_ALL = "Plasma 2 Allergy",
    PLASMA2_ALL_TYP = "specify",
    PLASMA2_AE_YN = "Plasma 2 AE",
    event = "Primary endpoint",
    event14 = "COVID related hospitalisation or death (day 14)",
    oms5_14 = "Hospitalisation for COVID w/ O2 or death (day 14)",
    oms5_28 = "Hospitalisation for COVID w/ O2 or death (day 28)",
    death28 = "All cause mortality (day 28)",
    death90 = "All cause mortality (day 90)",
    death180 = "All cause mortality (day 180)",
    o2_14 = "Oxygen required (day 14)",
    o2_28 = "Oxygen required (day 28)",
    niv_14 = "Non-invase ventilation (day 14)",
    niv_28 = "Non-invasive ventilation (day 28)",
    mv_14 = "Mechanical ventilation (day 14)",
    mv_28 = "Mechanical ventilation (day 28)",
    oms_change_14 = "Diff. WHO scale (day 14)",
    oms_change_28 = "Diff. WHO scale (day 28)",
    icu_14 = "ICU admission (day 14)",
    icu_28 = "ICU admission (day 28)",
    PCFS_D1 = "PCFS (Day 1)",
    PCFS_D28 = "PCFS (Day 28)",
    PCFS_D180 = "PCFS (Day 180)",
    pcfs2_d28 = "PCFS ≥2 (day 28)",
    pcfs2_d180 = "PCFS ≥2 (day 180)",
    diff_pcfs_d1_d28 = "Diff. PCFS (day 28)",
    diff_pcfs_d1_d180 = "Diff. PCFS (day 180)",
    diff_eq5dvas_d1_d28 = "Diff. EQ5D VAS (day 28)",
    diff_eq5dvas_d1_d180 = "Diff. EQ5D VAS (day 180)",
    EQ5D5L1_D1 = "EQ5D Mobility (Day 1)",
    EQ5D5L2_D1 = "EQ5D Self-care (Day 1)",
    EQ5D5L3_D1 = "EQ5D Usual activities (Day 1)",
    EQ5D5L4_D1 = "EQ5D Pain/discomfort (Day 1)",
    EQ5D5L5_D28 = "EQ5D Anxiety/depression (Day 28)",
    EQ5D5L1_D28 = "EQ5D Mobility (Day 28)",
    EQ5D5L2_D28 = "EQ5D Self-care (Day 28)",
    EQ5D5L3_D28 = "EQ5D Usual activities (Day 28)",
    EQ5D5L4_D28 = "EQ5D Pain/discomfort (Day 28)",
    EQ5D5L5_D28 = "EQ5D Anxiety/depression (Day 28)",
    EQ5D5L1_D1 = "EQ5D Mobility (Day 180)",
    EQ5D5L2_D1 = "EQ5D Self-care (Day 180)",
    EQ5D5L3_D1 = "EQ5D Usual activities (Day 180)",
    EQ5D5L4_D1 = "EQ5D Pain/discomfort (Day 180)",
    EQ5D5L5_D1 = "EQ5D Anxiety/depression (Day 180)",
    EQ5D5L_SC_D1_V = "EQ5D VAS (Day 1)",
    EQ5D5L_SC_D28_V = "EQ5D VAS (Day 28)",
    EQ5D5L_D180_SC_V = "EQ5D VAS (Day 180)",
    c19yrs_symptom_pre = "C19-YRS Symptom Pre-COVID-19",
    c19yrs_function_pre = "C19-YRS Functional Pre-COVID-19",
    c19yrs_overall_pre = "C19-YRS Overall Pre-COVID-19",
    c19yrs_symptom_d1 = "C19-YRS Symptom Day 1",
    c19yrs_function_d1 = "C19-YRS Functional Day 1",
    c19yrs_additional_d1 = "C19-YRS Additional Day 1",
    c19yrs_overall_d1 = "C19-YRS Overall Day 1",
    c19yrs_symptom_d28 = "C19-YRS Symptom Day 28",
    c19yrs_function_d28 = "C19-YRS Functional Day 28",
    c19yrs_additional_d28 = "C19-YRS Additional Day 28",
    c19yrs_overall_d28 = "C19-YRS Overall Day 28",
    c19yrs_symptom_d180 = "C19-YRS Symptom Day 180",
    c19yrs_function_d180 = "C19-YRS Functional Day 180",
    c19yrs_additional_d180 = "C19-YRS Additional Day 180",
    c19yrs_overall_d180 = "C19-YRS Overall Day 180",
    diff_c19yrs_symptom_pre_d28 = "Diff. C19-YRS Symptom (Day28)",
    diff_c19yrs_function_pre_d28 = "Diff. C19-YRS Functional (Day 28)",
    diff_c19yrs_overall_pre_d28 = "Diff. C19-YRS Overall (Day 28)",
    diff_c19yrs_symptom_pre_d180 = "Diff. C19-YRS Symptom (Day 180)",
    diff_c19yrs_function_pre_d180 = "Diff. C19-YRS Functional (Day 180)",
    diff_c19yrs_overall_pre_d180 = "Diff. C19-YRS Overall (Day 180)"
  ) |> 
  set_value_labels(SEX = c(Male = 1, Female = 2),
                   BLOODGRP = c(A = 1, B = 2, AB = 3, O = 4),
                   COVID19_VAC = c(Yes = 1, No = 0),
                   COVID19_1STVAC_TYP = c("Pfizer/BioNTech" = 1,
                                          "AstraZeneca" = 2,
                                          "Moderna" = 3,
                                          "Novavax" = 4,
                                          "Janssen" = 5,
                                          # "Unknown" = 6,
                                          "Other" = 7),
                   COVID19_2NDVAC_TYP = c("Pfizer/BioNTech" = 1,
                                          "AstraZeneca" = 2,
                                          "Moderna" = 3,
                                          "Novavax" = 4,
                                          "Janssen" = 5,
                                          # "Unknown" = 6,
                                          "Other" = 7),
                   COVID19_3RDVAC_TYP = c("Pfizer/BioNTech" = 1,
                                          "AstraZeneca" = 2,
                                          "Moderna" = 3,
                                          "Novavax" = 4,
                                          "Janssen" = 5,
                                          # "Unknown" = 6,
                                          "Other" = 7),
                   COVID19_4RDVAC_TYP = c("Pfizer/BioNTech" = 1,
                                          "AstraZeneca" = 2,
                                          "Moderna" = 3,
                                          "Novavax" = 4,
                                          "Janssen" = 5,
                                          # "Unknown" = 6,
                                          "Other" = 7),
                   D1_STATE = who_scale_labels,
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
                   D1_IGGASS = c(Yes = "1", No = "0"),
                   D14_IGAASS = c(Yes = "1", No = "0"),
                   D14_IGGASS = c(Yes = "1", No = "0"),
                   `COVID-19 antivirals` = c(Yes = 1, No = 0),
                   `COVID-19 monoclonal antibodies` = c(Yes = 1, No = 0),
                   variant_omicron = c(Omicron = 1, Other = 2)
                   ) |> 
  to_factor()
}

covic19.data <- my_labels(covic19.data)
covic19.data2 <- my_labels(covic19.data2)
