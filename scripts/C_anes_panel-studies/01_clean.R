
# - generational imprinting -------------------------------------------------- #
# - ANES panels | cleaner ---------------------------------------------------- #

### note: this script
###       (a) collects & cleans the ANES 2016-2020 longitudinal panel data,
###       (b) collects & cleans the ANES 2020-2022 longitudinal panel data,

rm(list = ls()) # clean-up

pacman::p_load(
  haven, 
  tidyverse)
`%nin%` = Negate(`%in%`)

# ---------------------------------------------------------------------------- #
# PART 1: ANES PANEL 2016-2020 ----------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- data
d <- haven::read_sav(
  "./data/source_files/anes_2016-2020.sav")

# --- organize
d <- d |>
  ## add id
  mutate(id = 1:nrow(d)) |>
  ## filter for non-Hispanic Whites
  ## | 74.1% of the ANES data
  filter(V161310x == 1) |>
  ## variables
  select(
    ## basics
    id,
    mode = V160501,
    ## weight
    wt = V200011a,
    psu = V200011c,
    str = V200011d,
    ## party!
    pid2016 = V161158x,
    pid2020 = V201231x,
    ## demographics
    age = V201507x,
    ## outcomes
    police_2016 = V162110,
    police_2020 = V202171
  )

# --- clean variables
d <- d |>
  
  ## strip labels
  haven::zap_labels() |>
  ## filters
  filter(age %nin% c(-9)) |>
  ## wrangle
  mutate(
    ## mode
    mode = factor(mode,
                  levels = c(1, 2),
                  labels = c("FTF",
                             "WEB")),
    ## party
    pid2016 = case_when(
      pid2016 %in% c(1, 2, 3) ~ 
        "Democrat",
      pid2016 %in% c(5, 6, 7) ~ 
        "Republican",
      pid2016 %in% c(4) ~ 
        "Independent",
      pid2016 == -8 ~ 
        "Independent", ## don't knows
      TRUE ~ NA_character_
    ),
    pid2020 = case_when(
      pid2020 %in% c(1, 2, 3) ~ 
        "Democrat",
      pid2020 %in% c(5, 6, 7) ~ 
        "Republican",
      pid2020 %in% c(4) ~ 
        "Independent",
      pid2020 == -8 ~ 
        "Independent", ## don't knows
      TRUE ~ NA_character_
    ),
    party = case_when(
      pid2016 == pid2020 ~ 
        pid2020,
      is.na(pid2016) == TRUE & 
        is.na(pid2020) == FALSE ~ 
        pid2020,
      is.na(pid2016) == FALSE & 
        is.na(pid2020) == TRUE ~ 
        pid2016,
      pid2016 != pid2020 ~
        "Switcher",
      TRUE ~ NA_character_
    ),
    party = factor(
      party,
      levels = c("Democrat",
                 "Independent",
                 "Republican",
                 "Switcher")),
    ## age
    agegr =
      case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 39 ~ "30-39",
        age >= 40 & age <= 64 ~ "40-64",
        age >= 65 ~ "65+",
        TRUE ~ NA_character_),
    agegr =
      factor(agegr,
             levels = c("18-29",
                        "30-39",
                        "40-64",
                        "65+")),
    police_2016 = ifelse(
      police_2016 < 0 | police_2016 > 100, 
      NA_real_, 
      police_2016),
    police_2020 = ifelse(
      police_2020 < 0 | police_2020 > 100, 
      NA_real_, 
      police_2020))

# --- wrap-up
d <- d |> 
  select(id,
         mode,
         wt,
         psu,
         str,
         agegr,
         age,
         party,
         pid2016,
         pid2020,
         police_2016,
         police_2020) |> 
  drop_na()

# --- rescale the weights
d <- d |> 
  mutate(wt = wt / mean(wt, na.rm = TRUE))

saveRDS(d, file = "./data/output/anes_16-20.rds")
rm(d) # clean-up

# ---------------------------------------------------------------------------- #
# PART 2: ANES PANEL 2020-2022 ----------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- data
d <- haven::read_dta(
  "./data/source_files/anes_2020-2022.dta")

# --- organize
d <- d |>
  ## add id
  mutate(id = 1:nrow(d)) |>
  ## filter for non-Hispanic Whites
  filter(profile_racethnicity == 1) |>
  ## variables
  select(
    ## basics
    id,
    wt = w3weight,
    ## demographics
    age = profile_age,
    ## outcomes: 2016
    police_2020 = w2ftpolice,
    police_2022 = w3ftpolice
  )

# --- clean variables
d <- d |>
  
  ## strip labels
  haven::zap_labels() |>
  ## filter
  filter(police_2020 %nin% c(-9:-1)) |>
  filter(police_2022 %nin% c(-9:-1)) |>
  ## changes in thermometers
  mutate(
    ## age
    agegr =
      case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 39 ~ "30-39",
        age >= 40 & age <= 64 ~ "40-64",
        age >= 65 ~ "65+",
        TRUE ~ NA_character_),
    agegr =
      factor(agegr,
             levels = c("18-29",
                        "30-39",
                        "40-64",
                        "65+"))) |> 
  drop_na()

# --- rescale the weights
d <- d |> 
  mutate(wt = wt / mean(wt, na.rm = TRUE))

saveRDS(d, file = "./data/output/anes_20-22.rds")

# ---------------------------------------------------------------------------- #
