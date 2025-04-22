
# - generational imprints ---------------------------------------------------- #
# - nationscape | summary ---------------------------------------------------- #

### note: this script
###       (a) provides descriptive statistics for the Nationscape data.

rm(list = ls()) # clean-up

pacman::p_load(
  survey,
  gtsummary,
  tidyverse)
`%nin%` = Negate(`%in%`)

# --- datafile
d <- readRDS("./data/output/nationscape.rds")

# --- survey
d_survey <- d |>
  survey::svydesign(data = _,
                    ids = ~ 1,
                    weights = ~ wt)

# ---------------------------------------------------------------------------- #
# PART 1: NATIONSCAPE -------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- descriptives 1
desc1 <- suppressWarnings(
  d_survey |>
    tbl_svysummary(
      include = c(agegr, gender, educ, pid, attention),
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
    all_stat_cols() ~ "**N = {n_unweighted}**") |> 
  modify_footnote(
    all_stat_cols() ~ NA)
saveRDS(desc1,
        file = "./output/A_nationscape/desc1.rds")

# --- descriptives 2
desc2 <- suppressWarnings(
  d_survey |>
    tbl_svysummary(
      include = c(agegr, gender, educ, pid, attention),
      by = agegr,
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
        file = "./output/A_nationscape/desc2.rds")

# ---------------------------------------------------------------------------- #
