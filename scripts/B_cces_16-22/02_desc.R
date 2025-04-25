
# - generational imprinting -------------------------------------------------- #
# - CCES time-series | summary ----------------------------------------------- #

### note: this script
###       (a) provides descriptive statistics for the CCES 2016-2022 data.

rm(list = ls()) # clean-up

pacman::p_load(
  survey,
  gtsummary,
  tidyverse)
`%nin%` = Negate(`%in%`)

# --- datafile
d <- readRDS("./data/output/cces_16-22.rds")

# --- survey
d_survey <- d |>
  mutate(age_at_2020 = fct_recode(age_at_2020, `<24` = "24 or Younger")) |>
  survey::svydesign(data = _,
                    ids = ~ 1,
                    weights = ~ wt)

# ---------------------------------------------------------------------------- #
# PART 1: CCES 2016-2022 TIME SERIES ----------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- descriptives 1
desc1 <- suppressWarnings(
  d_survey |>
    tbl_svysummary(
      include = c(agegr, gender, educ, pid),
      by = year,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})", 
        all_categorical() ~ "{p}%"),
      digits = list(
        all_continuous() ~ 2, 
        all_categorical() ~ 1),
      label = list(
        agegr = "Age Groups",
        gender = "Gender",
        educ = "Educational Attainment",
        pid = "Party Identification",
        attention = "Political Attention"
      )
    )) |> 
  modify_header(
    all_stat_cols() ~ "**{level}**") |> 
  modify_footnote(
    all_stat_cols() ~ NA)
saveRDS(desc1,
        file = "./output/B_cces_16-22/desc1.rds")

# --- descriptives 2
desc2 <- suppressWarnings(
  d_survey |>
    tbl_svysummary(
      include = c(age_at_2020, gender, educ, pid),
      by = age_at_2020,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})", 
        all_categorical() ~ "{p}%"),
      digits = list(
        all_continuous() ~ 2, 
        all_categorical() ~ 1),
      label = list(
        agegr = "Age Groups",
        gender = "Gender",
        educ = "Educational Attainment",
        pid = "Party Identification",
        attention = "Political Attention"
      )
    )) |> 
  modify_header(
    all_stat_cols() ~ "**{level}**") |> 
  modify_footnote(
    all_stat_cols() ~ NA)

saveRDS(desc2,
        file = "./output/B_cces_16-22/desc2.rds")

# ---------------------------------------------------------------------------- #
