
# - generational imprinting -------------------------------------------------- #
# - nationscape | analyze ---------------------------------------------------- #

### note: this script
###       (a) provides pre-treatment tests for the main diff-in-diff analyses.

rm(list = ls()) # clean-up

pacman::p_load(
  fixest,
  ggfixest,
  srvyr,
  MatchIt,
  purrr,
  furrr,
  splines,
  hrbrthemes,
  tidyverse,
  patchwork,
  tinytable,
  marginaleffects,
  modelsummary)

# --- helpers
my_oka <-
  c("#82B2C0",
    "#F6C7B3",
    "#eebd64")
us_oka <- 
  c("#013364",
    "#cbcaca",
    "#cc0000")
theme_set(theme_ipsum_rc(
  grid = "XY",
  axis_title_size = 12,
  plot_margin =
    margin(10, 10, 10, 10)
))
`%nin%` = Negate(`%in%`)

# --- datafile
d <- readRDS("./data/output/nationscape.rds")

# --- survey
d_survey <- srvyr::as_survey(d, weights = wt)

# ---------------------------------------------------------------------------- #
# PART 2: PARALLEL TRENDS CHECK ---------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- "treated" & "post"
d <- d |>
  ## define treated as 18-24-year-olds
  mutate(treated = if_else(age <= 24, 1, 0)) |>
  ## define post as the period starting with t = 0
  mutate(post = if_else(time >= 0, 1, 0)) |>
  ## define treatment as 18-24-year-old with t = 0
  mutate(treatment = treated * post) |> ## this covers it
  ## a new outcome column for ease of reading
  mutate(police_100 = police * 100)

# --- regression models with a DID set-up
# --- these results are comparable to Stata's `didregress`

# --- full estimates
m <- feols(
  police ~
    i(time, treated, ref = -1) +
    gender * pid * educ |
    agegr + time + condistrict,
  data = d,
  weights = d$wt,
  vcov = "HC1"
)
pre_test <- ggfixest::aggr_es(m, period = "pre")

# --- party estimates

## D
mD <- feols(
  police ~
    i(time, treated, ref = -1) +
    gender * educ |
    agegr + pid + agegr^pid + time + condistrict,
  data = d |> 
    mutate(treated = treated * (pid == "Democrat")),
  weights = d$wt,
  vcov = "HC1"
)
pre_testD <- ggfixest::aggr_es(mD, period = "pre")

## I
mI <- feols(
  police ~
    i(time, treated, ref = -1) +
    gender * educ |
    agegr + pid + agegr^pid + time + condistrict,
  data = d |> 
    mutate(treated = treated * (pid == "Independent")),
  weights = d$wt,
  vcov = "HC1"
)
pre_testI <- ggfixest::aggr_es(mI, period = "pre")

## R
mR <- feols(
  police ~
    i(time, treated, ref = -2) +
    gender * educ |
    agegr + pid + agegr^pid + time + condistrict,
  data = d |> 
    mutate(treated = treated * (pid == "Republican")),
  weights = d$wt,
  vcov = "HC1"
)
pre_testR <- ggfixest::aggr_es(mR, period = "pre")

# --- bind

b <- bind_rows(
  broom::tidy(pre_test) |>
    mutate(model = "Full Model") |>
    select(model, estimate, std.error, p.value),
  broom::tidy(pre_testD) |>
    mutate(model = "Democrats") |>
    select(model, estimate, std.error, p.value),
  broom::tidy(pre_testI) |>
    mutate(model = "Independents") |>
    select(model, estimate, std.error, p.value),
  broom::tidy(pre_testR) |>
    mutate(model = "Republicans") |>
    select(model, estimate, std.error, p.value)
) |>
  rename(
    Model = model,
    Estimate = estimate,
    Error = std.error,
    `p-value` = p.value
  ) |> 
  mutate(Estimate = round(Estimate, 3),
         Error = round(Error, 3),
         `p-value` = round(`p-value`, 3))
t <- tt(data.frame(b),
        width = 1) |>
  style_tt(j = 1:4, align = "lccc")
saveRDS(t, "./output/A_nationscape/pretrends.rds")

# ---------------------------------------------------------------------------- #
