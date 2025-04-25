
# - generational imprinting -------------------------------------------------- #
# - CCES time-series | cleaner ----------------------------------------------- #

### note: this script
###       (a) collects & cleans the CCES 2016-2022 time-series data.

rm(list = ls()) # clean-up

pacman::p_load(
  haven, 
  tidyverse)
`%nin%` = Negate(`%in%`)

# ---------------------------------------------------------------------------- #
# PART 1: CCES 2016-2022 TIME SERIES ----------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- data
c2016 <- haven::read_dta(
  "./data/source_files/cces_2016.dta")
c2020 <- haven::read_dta(
  "./data/source_files/cces_2020.dta")
c2022 <- haven::read_dta(
  "./data/source_files/cces_2022.dta")

# --- organize 2016
c2016 <- c2016 |>
  haven::zap_labels() |>
  ## year + age
  mutate(year = 2016) |>
  mutate(age = 2016 - birthyr) |>
  ## filter for non-Hispanic Whites
  ## | 69.2% of the CCES 2016 data
  filter(race == 1 & hispanic == 2) |>
  ## safe with the police
  mutate(
    police_safe = case_when(
      CC16_307 == 1 ~ 0,
      CC16_307 == 2 ~ 1/3,
      CC16_307 == 3 ~ 2/3,
      CC16_307 == 4 ~ 1)
  ) |>
  ## party identification
  mutate(
    pid = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 %in% c(3, 4, 5) ~ "Independent",
      pid3 == 2 ~ "Republican"
    )
  ) |>
  ## select variables
  select(
    year,
    age,
    birthyr,
    gender,
    pid,
    educ,
    county = countyfips,
    wt = commonweight,
    police_safe
  ) |> 
  ## drop the variables
  drop_na()

# --- organize 2020
c2020 <- c2020 |>
  haven::zap_labels() |> 
  ## year + age
  mutate(year = 2020) |>
  mutate(age = 2020 - birthyr) |>
  ## filter for non-Hispanic Whites
  ## | 70.7% of the CCES 2020 data
  filter(race == 1 & hispanic == 2) |> 
  ## safe with the police
  mutate(
    police_safe = case_when(
      CC20_307 == 1 ~ 0,
      CC20_307 == 2 ~ 1/3,
      CC20_307 == 3 ~ 2/3,
      CC20_307 == 4 ~ 1)
  ) |>
  ## party identification
  mutate(
    pid = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 %in% c(3, 4, 5) ~ "Independent",
      pid3 == 2 ~ "Republican"
    )
  ) |>
  ## select variables
  select(
    year,
    age,
    birthyr,
    gender,
    pid,
    educ,
    county = countyfips,
    wt = commonweight,
    police_safe
  ) |> 
  ## drop the variables
  drop_na()

# --- organize 2022
c2022 <- c2022 |>
  haven::zap_labels() |> 
  ## year + age
  mutate(year = 2022) |>
  mutate(age = 2022 - birthyr) |>
  ## filter for non-Hispanic Whites
  ## | 66.9% of the CCES 2022 data
  filter(race == 1 & hispanic == 2) |>
  ## safe with the police
  mutate(
    police_safe = case_when(
      CC22_307 == 1 ~ 0,
      CC22_307 == 2 ~ 1/3,
      CC22_307 == 3 ~ 2/3,
      CC22_307 == 4 ~ 1)
  ) |>
  ## gender
  mutate(
    gender = if_else(gender4 == 3 | gender4 == 4,
                     2,
                     gender4)
  ) |> 
  ## party identification
  mutate(
    pid = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 %in% c(3, 4, 5) ~ "Independent",
      pid3 == 2 ~ "Republican"
    )
  ) |>
  ## select variables
  select(
    year,
    age,
    birthyr,
    gender,
    pid,
    educ,
    county = countyfips,
    wt = commonweight,
    police_safe
  ) |> 
  ## drop the variables
  drop_na()

# ---------------------------------------------------------------------------- #
# PART 2: CCES 2016-2022, PREPARATION ---------------------------------------- #
# ---------------------------------------------------------------------------- #

d <- bind_rows(c2016, c2020, c2022) |> 
  ## factor of year
  mutate(year = factor(year)) |> 
  ## age groups
  mutate(agegr = case_when(
    age >= 18 & age <= 24 ~ "18-24",
    age >= 25 & age <= 34 ~ "25-34",
    age >= 35 & age <= 49 ~ "35-49",
    age >= 50 & age <= 64 ~ "50-64",
    age >= 65 ~ "65+"
  )) |> 
  mutate(agegr = factor(
    agegr, levels = c("18-24",
                      "25-34",
                      "35-49",
                      "50-64",
                      "65+")
  )) |> 
  ## clean-up
  mutate(
    gender = if_else(gender == 1, "Male", "Female")
  ) |>
  mutate(
    educ = case_when(
      educ == 1 | educ == 2 ~ "High School or Less",
      educ == 3 | educ == 4 ~ "Some College",
      educ == 5 ~ "College",
      educ == 6 ~ "Post-Graduate Degree"
    ),
    educ = factor(educ, levels = c("High School or Less",
                                   "Some College",
                                   "College",
                                   "Post-Graduate Degree"))
  )

# --- cohorts
d <- d |>
  mutate(birthyr = as.integer(as.character(year)) - age) |>
  ## cohort bins
  mutate(
    age_at_2020 = case_when(
      birthyr <= 1955 ~ "65+",
      birthyr >= 1956 & birthyr <= 1970 ~ "50-64",
      birthyr >= 1971 & birthyr <= 1985 ~ "35-49",
      birthyr >= 1986 & birthyr <= 1995 ~ "25-34",
      birthyr >= 1996 ~ "24 or Younger"
    ),
    age_at_2020 = factor(
      age_at_2020,
      levels = c("24 or Younger", 
                 "25-34", 
                 "35-49", 
                 "50-64", 
                 "65+")
    ),
    age_at_2020_cont = 2020 - birthyr,
    age_at_2020_cont =
      ifelse(age_at_2020_cont < 18, 18, age_at_2020_cont),
    age_at_2020_cont =
      ifelse(age_at_2020_cont > 80, 80, age_at_2020_cont)
  )

# --- rescale the weights
d <- d |> 
  mutate(wt = wt / mean(wt, na.rm = TRUE))

saveRDS(d, file = "./data/output/cces_16-22.rds")

# ---------------------------------------------------------------------------- #
