
# - generational imprinting -------------------------------------------------- #
# - nationscape | cleaner ---------------------------------------------------- #

### note: this script
###       (a) collects & cleans the Nationscape time-series data,
###       (b) measures & appends GF protest data for congressional districts.

rm(list = ls()) # clean-up

pacman::p_load(
  zoo,
  purrr,
  haven,
  tidyverse)
`%nin%` = Negate(`%in%`)

# ---------------------------------------------------------------------------- #
# PART 1: NATIONSCAPE -------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- data list
files <- list.files(
  pattern = "\\.dta$",
  path = "./data/source_files/nationscape",
  recursive = TRUE,
  full.names = TRUE # yup!
) ## simply listing the data files in the `nationscape` source folder.

# --- variables
d <- files |> map_dfr(
  ~ read_dta(.x) |>
    select(
      ## basics
      id = response_id,
      wt = weight,
      condistrict = congress_district,
      day = start_date,
      ## demographics
      gender,
      age,
      race = race_ethnicity,
      hisp = hispanic,
      educ = education,
      ## politics
      pid3,
      ideo5,
      int = interest,
      kn1 = sen_knowledge,
      kn2 = sc_knowledge,
      ## outcomes
      police =
        group_favorability_the_police
    ),
  .id = "wave"
)

# --- organize
d <- d |>
  ## strip labels
  haven::zap_labels() |>
  ## filter for non-Hispanic Whites
  ## |67.4% of the Nationscape data
  filter(race == 1 & hisp == 1) |>
  select(-race, -hisp) |> 
  ## time variables
  mutate(wave = as.integer(wave)) |>
  mutate(day = as_date(day)) |>
  ## account for "parallel" studies
  mutate(
    # merge 07/16 with parallel study 2
    wave = if_else(wave == 54, 53, wave),
    wave = if_else(wave == 53 & day %in% c("2020-07-23",
                                           "2020-07-24",
                                           "2020-07-25",
                                           "2020-07-26",
                                           "2020-07-27"), 
                   55, wave),
    wave = if_else(wave >= 54, wave - 1, wave)) |>
  ## 4-week aggregation
  mutate(time = floor((wave - 46) / 4))

# --- wrangling
d <- d |> 

  ## clean-up variables
  mutate(
    
    ## age
    agegr =
      case_when(
        age >= 18 & age <= 24 ~ "18-24",
        age >= 25 & age <= 34 ~ "25-34",
        age >= 35 & age <= 49 ~ "35-49",
        age >= 50 & age <= 64 ~ "50-64",
        age >= 65 ~ "65+",
        TRUE ~ NA_character_
      ),
    agegr =
      factor(
        agegr,
        levels = c(
          "18-24",
          "25-34",
          "35-49",
          "50-64",
          "65+"
        )
      ),
    ## gender
    gender = factor(
      gender,
      levels = c(1, 2),
      labels = c("Female", "Male")
    ),
    ## partisanship
    pid = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 == 3 | pid3 == 4 ~ "Independent",
      pid3 == 2 ~ "Republican"
    ),
    pid = factor(pid, levels = c("Democrat", 
                                 "Independent", 
                                 "Republican")),
    ## ideology
    ideo = case_when(
      ideo5 %in% c(1, 2) ~ "Liberal",
      ideo5 %in% c(4, 5) ~ "Conservative",
      TRUE ~ "Moderate"
    ),
    ideo = factor(ideo, levels = c("Liberal",
                                   "Moderate",
                                   "Conservative")),
    ## education
    educ = case_when(
      educ <= 4 ~ "High School or Less",
      educ %in% c(5, 6, 7) ~ "Some College",
      educ %in% c(8, 9) ~ "College",
      educ %in% c(10, 11) ~ "Post-Graduate Degree",
      TRUE ~ NA_character_
    ),
    educ = factor(educ,
                  levels = c(
                    "High School or Less",
                    "Some College",
                    "College",
                    "Post-Graduate Degree"
                  )),
    ## attention
    kn = case_when(
      kn1 == 1 & kn2 == 1 ~ 1,
      kn1 == 1 | kn2 == 1 ~ 0.5,
      kn1 != 1 & kn2 != 1 ~ 0,
      TRUE ~ NA_real_
    ),
    int = (4 - int) / 3, ## normalized interest
    attention = (kn + int) / 2,
    ## outcome
    police =
      case_when(
        police == 1 ~ 0,
        police == 2 ~ 0.25,
        police == 999 ~ 0.5,
        police == 3 ~ 0.75,
        police == 4 ~ 1,
        TRUE ~ NA_real_
      )
  ) |>
  
  ## drop missing observations
  ## ~3,958 cases (roughly 1.2% of the initial dataframe)
  drop_na() |>
  
  ## organize the data file for further use
  arrange(wave, id) |>
  select(
    id,
    time,
    wave,
    wt,
    condistrict,
    age,
    agegr,
    educ,
    gender,
    pid,
    ideo,
    attention,
    police
  )

# --- rescale the weights
d <- d |> 
  mutate(wt = wt / mean(wt, na.rm = TRUE))

# ---------------------------------------------------------------------------- #
# PART 2: CROWD COUNTING CONSORTIUM ------------------------------------------ #
# ---------------------------------------------------------------------------- #

# --- crosswalk info
crosswalk <-

  ## generate baseline CDs
  read_csv("./data/helpers/geocorr_crosswalk.csv") |>
  janitor::clean_names() |>
  mutate(cd116 = as.character(cd116),
         cd116 = ifelse(
           str_length(cd116) == 1, 
           paste("0", cd116, sep = ""), 
           cd116)) |> ## make them compatible with Nationscape
  mutate(condistrict = paste(state, cd116, sep = "")) |>

  ## add George Floyd protest data
  left_join(
    read_csv("./data/source_files/ccc.csv") |>
      select(county = resolved_county, state = resolved_state) |>
      drop_na() |> ## drop missing values
      summarize(n = n(), .by = c("county", "state")) |>
      ## properly organize the matching values
      mutate(
        county = str_remove(county, pattern = " County"),
        county_name = paste(county, state, sep = " ")) |> 
      ## there are 1,382 counties.
      ## this matches with what Gethin and Pons report (1,381)
      select(county_name, state, n),
    by = c("state", "county_name")
  ) |> 
  
  ## organize and summarize
  mutate(n = ifelse(is.na(n) == TRUE, 0, n)) |> 
  summarize(protests = sum(n), .by = c("condistrict")) ## done

# --- append information
d <- d |> 
  left_join(crosswalk, by = "condistrict")

saveRDS(d, file = "./data/output/nationscape.rds")

# ---------------------------------------------------------------------------- #
