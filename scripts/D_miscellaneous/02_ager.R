
# - generational imprinting -------------------------------------------------- #
# - misc --------------------------------------------------------------------- #

### note: this script
###       (a) analyzes the effects of different age cutoffs in the Nationscape.
###       (b) analyzes the effects of different age cutoffs in the CCES.

rm(list = ls()) # clean-up

pacman::p_load(
  fixest,
  purrr,
  furrr,
  hrbrthemes,
  tidyverse,
  patchwork,
  marginaleffects,
  modelsummary)

# --- helpers
my_oka <-
  c("#82B2C0",
    "#F6C7B3",
    "#eebd64")
theme_set(theme_ipsum_rc(
  grid = "XY",
  axis_title_size = 12,
  plot_margin =
    margin(10, 10, 10, 10)
))
`%nin%` = Negate(`%in%`)

# ---------------------------------------------------------------------------- #
# PART 1: NATIONSCAPE -------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- datafile
ns <- readRDS("./data/output/nationscape.rds")

# --- prep for `future`
d <- ## prepare the data sources
  tibble(age_cut = 18:30) |>
  mutate(data = rep(ns |> nest(), length(18:40))[[1]]) |> 
  ## add age cuts in each data
  mutate(data = map2(
    .x = data,
    .y = age_cut,
    .f = ~ . |> mutate(
      treated = if_else(age >= .y &
                          age <= .y + 5, 1, 0),
      post = if_else(time >= 0, 1, 0),
      treatment = treated * post ## for a specific group
    ) |> filter(age >= .y)
  ))

# --- estimate DID
d <- d |>
  mutate(did = map(
    .x = data,
    .f = ~ feols(
      police ~ treatment +
        gender * pid * educ |
        age + time + condistrict,
      data = .,
      weights = .$wt,
      vcov = "HC1"
    ),
    .progress = TRUE
  ))

# --- extract coefficients
d <- d |>
  mutate(tidy = map(.x = did, .f = ~ broom::tidy(.))) |>
  select(age_cut, tidy) |> 
  unnest(tidy) |> 
  filter(term == "treatment") |> 
  mutate(lo = estimate - 1.96*std.error,
         hi = estimate + 1.96*std.error) |> 
  select(age_cut, estimate, lo, hi) |> 
  mutate(age_cut = paste(age_cut, 
                         age_cut + 5, 
                         sep = "-"))

# --- plot coefficients
p1 <- d |>
  ggplot(aes(
    x = age_cut,
    y = estimate,
    ymin = lo,
    ymax = hi
  )) +
  geom_linerange(
    linewidth = 1.5,
    alpha = .5,
    col = my_oka[1]
  ) +
  geom_point(size = 1) +
  scale_y_continuous(limits = c(0, 0.12),
                     breaks = c(0, 0.04, 0.08, 0.12)) +
  labs(x = "Age Cut-Off",
       y = "Difference-in-Differences Estimate",
       subtitle = "Nationscape")

# ---------------------------------------------------------------------------- #
# PART 2: CCES --------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- datafile
cc <- readRDS("./data/output/cces_16-22.rds")

# --- prep for `future`
d <- ## prepare the data sources
  tibble(age_cut = 18:30) |>
  mutate(data = rep(cc |> nest(), length(18:40))[[1]]) |>
  ## add age cuts in each data
  mutate(data = map2(
    .x = data,
    .y = age_cut,
    .f = ~ . |> mutate(
      treated = if_else(age_at_2020_cont >= .y &
                          age_at_2020_cont <= .y + 5, 1, 0),
      post = if_else(year == 2016, 0, 1),
      treatment = treated * post ## for a specific group
    ) |> filter(age_at_2020_cont >= .y)
  ))

# --- estimate DID
d <- d |>
  mutate(did = map(
    .x = data,
    .f = ~ feols(
      police_safe ~ treatment +
        pid * gender * educ |
        age_at_2020_cont + year + county,
      data = .,
      weights = .$wt,
      vcov = "HC1"
    ),
    .progress = TRUE
  ))

# --- extract coefficients
d <- d |>
  mutate(tidy = map(.x = did, .f = ~ broom::tidy(.))) |>
  select(age_cut, tidy) |> 
  unnest(tidy) |> 
  filter(term == "treatment") |> 
  mutate(lo = estimate - 1.96*std.error,
         hi = estimate + 1.96*std.error) |> 
  select(age_cut, estimate, lo, hi) |> 
  mutate(age_cut = paste(age_cut, 
                         age_cut + 5, 
                         sep = "-"))

# --- plot coefficients
p2 <- d |>
  ggplot(aes(
    x = age_cut,
    y = estimate,
    ymin = lo,
    ymax = hi
  )) +
  geom_linerange(
    linewidth = 1.5,
    alpha = .5,
    col = my_oka[1]
  ) +
  geom_point(size = 1) +
  scale_y_continuous(limits = c(0.04, 0.18),
                     breaks = c(0.04, 0.11, 0.18)) +
  labs(x = "Age Cut-Off",
       y = "Difference-in-Differences Estimate",
       subtitle = "CCES")

# ---------------------------------------------------------------------------- #
# PART 3: DRAW --------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

p1 / p2
ggsave(
  filename = "./output/D_miscellaneous/agehtr.png",
  width = 8,
  height = 8,
  units = "in",
  dpi = 500,
  bg = "white"
)

# ---------------------------------------------------------------------------- #
