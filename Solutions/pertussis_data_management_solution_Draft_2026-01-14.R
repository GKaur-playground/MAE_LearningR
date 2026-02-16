# Data management in R (tidyverse) – Pertussis exercise
# Script solution (facilitators)

# 0) Setup ---------------------------------------------------------------

library(tidyverse)
library(haven)
library(lubridate)
library(janitor)

# Optional log (uncomment if desired)
# sink("pertussis_log.txt", split = TRUE)

# 1.1) Import data -------------------------------------------------------

pertussis_raw <- read_dta("pertussis.dta") %>% clean_names()

glimpse(pertussis_raw)
summary(pertussis_raw)

# 1.4) Duplicates --------------------------------------------------------

pertussis_raw %>%
  count(id) %>%
  filter(n > 1) %>%
  arrange(desc(n))

pertussis_nodup <- pertussis_raw %>%
  distinct(id, .keep_all = TRUE)

# 1.5) Dates -------------------------------------------------------------

pertussis_dates <- pertussis_nodup %>%
  mutate(
    dob = dmy(birthdate),
    don = dmy(notificationdate),
    doo = dmy(onsetdate)
  )

glimpse(pertussis_dates)

# 1.6) Days from onset to notification ----------------------------------

pertussis_days <- pertussis_dates %>%
  mutate(days_notif = as.numeric(don - doo))

summary(pertussis_days$days_notif)

pertussis_days <- pertussis_days %>%
  filter(!is.na(days_notif), days_notif >= 0)

# 1.7) Month and week -----------------------------------------------------

pertussis_time <- pertussis_days %>%
  mutate(
    monthonset = month(doo),
    weekonset  = isoweek(doo)
  ) %>%
  filter(!is.na(doo))

pertussis_time %>% count(monthonset, sort = TRUE)
pertussis_time %>% count(weekonset, sort = TRUE)

# 1.8) Age ---------------------------------------------------------------

pertussis_age <- pertussis_time %>%
  mutate(
    age_onset = (as.numeric(doo - dob) / 365.25),
    age_onset = round(age_onset, 0)
  ) %>%
  filter(is.na(age_onset) | age_onset >= 0) %>%
  rename(age = age_onset)

summary(pertussis_age$age)

# 1.9) Age groups --------------------------------------------------------

pertussis_agegrp <- pertussis_age %>%
  mutate(
    under_18 = case_when(
      age < 18 ~ "Under 18 years",
      age >= 18 ~ "18 or more",
      TRUE ~ NA_character_
    )
  )

pertussis_agegrp %>% count(under_18, sort = TRUE)

breaks <- c(0,10,20,30,40,50,60,70,80,110)
labels <- c("0-9 years","10-19 years","20-29 years","30-39 years","40-49 years",
            "50-59 years","60-69 years","70-79 years","80+ years")

pertussis_agegrp <- pertussis_agegrp %>%
  mutate(agegp10 = cut(age, breaks = breaks, right = FALSE, include.lowest = TRUE, labels = labels))

pertussis_agegrp %>% count(agegp10, sort = TRUE)

# 1.10) Sex --------------------------------------------------------------

# Keep coding consistent with the Stata student prompt: female=1, male=0
pertussis_sex <- pertussis_agegrp %>%
  mutate(
    sex_num = case_when(
      str_to_lower(sex) == "female" ~ 1,
      str_to_lower(sex) == "male"   ~ 0,
      TRUE ~ NA_real_
    )
  )

pertussis_sex %>% count(sex, sex_num)

# 1.11) Missing values (9 -> NA) ----------------------------------------

exposure_vars <- c("mother","father","sibling","spouse","otherhousehold","workplace")

pertussis_missing <- pertussis_sex %>%
  mutate(across(all_of(exposure_vars), ~na_if(., 9)))

pertussis_missing %>%
  summarise(across(all_of(exposure_vars), ~sum(is.na(.))))

# 1.12) Residence --------------------------------------------------------

pertussis_res <- pertussis_missing %>%
  mutate(
    residence = case_when(
      postcode < 2000 ~ "Metro",
      postcode >= 2000 ~ "Rural",
      TRUE ~ NA_character_
    )
  )

pertussis_res %>% count(residence, sort = TRUE)

# 1.13) Lab confirmation -------------------------------------------------

# Recommended coding: PCR=1, serology=0
pertussis_lab <- pertussis_res %>%
  mutate(
    lab = str_sub(howconfirmed, 1, 1),
    lab_num = case_when(
      lab == "P" ~ 1,
      lab == "S" ~ 0,
      TRUE ~ NA_real_
    )
  )

pertussis_lab %>% count(lab, lab_num)

# 1.14) Save -------------------------------------------------------------

write_csv(pertussis_lab, "pertussis_clean.csv")
# Optional:
# write_dta(pertussis_lab, "pertussis_clean.dta")

# Stop logging (if used)
# sink()
