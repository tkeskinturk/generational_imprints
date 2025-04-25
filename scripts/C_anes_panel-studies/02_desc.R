
# - generational imprinting -------------------------------------------------- #
# - ANES panels | summary ---------------------------------------------------- #

### note: this script
###       (a) provides descriptive statistics for the ANES 2016-2020 data,
###       (b) checks whether covariates differ by survey mode.

rm(list = ls()) # clean-up

pacman::p_load(
  survey,
  gtsummary,
  tidyverse)

# --- helpers
`%nin%` = Negate(`%in%`)

# --- datafile
d <- readRDS("./data/output/anes_16-20.rds")

# --- survey
d_survey <- d |>
  survey::svydesign(data = _,
                    strata = ~str,
                    ids = ~ 1,
                    weights = ~ wt,
                    psu = ~ psu)

# ---------------------------------------------------------------------------- #
# PART 1: ANES 2016-2020 TIME SERIES ----------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- descriptives 1
desc1 <- suppressWarnings(
  d_survey |>
    tbl_svysummary(
      include = c(agegr, party),
      statistic = list(
        all_continuous() ~ "{mean} ({sd})", 
        all_categorical() ~ "{p}%"),
      digits = list(
        all_continuous() ~ 2, 
        all_categorical() ~ 1),
      label = list(
        agegr = "Age Groups",
        party = "Party Identification"
      )
    )) |>
  modify_header(
    all_stat_cols() ~ "**N = {n_unweighted}**") |> 
  modify_footnote(
    all_stat_cols() ~ NA)
saveRDS(desc1,
        file = "./output/C_anes_panel-studies/desc1.rds")

# --- descriptives 2
desc2 <- suppressWarnings(
  d_survey |>
    tbl_svysummary(
      include = c(agegr, party, mode),
      by = mode,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})", 
        all_categorical() ~ "{p}%"),
      digits = list(
        all_continuous() ~ 2, 
        all_categorical() ~ 1),
      label = list(
        mode = "Survey Mode",
        agegr = "Age Groups",
        party = "Party Identification"
      )
    )) |> 
  modify_header(
    all_stat_cols() ~ "**{level} = {n_unweighted}**") |> 
  modify_footnote(
    all_stat_cols() ~ NA)
saveRDS(desc2,
        file = "./output/C_anes_panel-studies/desc2.rds")

# ---------------------------------------------------------------------------- #
