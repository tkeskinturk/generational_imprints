
# - generational imprinting -------------------------------------------------- #
# - ANES panels | analyze ---------------------------------------------------- #

### note: this script
###       (a) analyzes the effects of GF killing on attitudes towards police.

rm(list = ls()) # clean-up

pacman::p_load(
  survey,
  srvyr,
  surveyCV,
  fixest,
  splines,
  hrbrthemes,
  tidyverse,
  patchwork,
  marginaleffects)

# --- helpers
my_oka <-
  c("#82B2C0",
    "#F6C7B3",
    "#eebd64")
us_oka <- 
  c("#013364",
    "#cbcaca",
    "#cc0000",
    "#eebd64")
theme_set(theme_ipsum_rc(
  grid = "XY",
  axis_title_size = 12,
  plot_margin =
    margin(10, 10, 10, 10)
))
`%nin%` = Negate(`%in%`)

# ---------------------------------------------------------------------------- #
# PART 1: PLACEBO ------------------------------------------------------------ #
# ---------------------------------------------------------------------------- #

# --- preps
d_2022 <- 
  readRDS("./data/output/anes_20-22.rds") |>
  mutate(diff = police_2022 - police_2020)

# --- modeling
m1 <- fixest::feols(diff ~ agegr,
                    data = d_2022,
                    weights = d_2022$wt,
                    vcov = "hetero")
m2 <- fixest::feols(abs(diff) ~ agegr,
                    data = d_2022,
                    weights = d_2022$wt,
                    vcov = "hetero")

# --- plots

## plot 1
p1 <- m2 |>
  marginaleffects::avg_predictions(variables = "agegr",
                                   wts = d_2022$wt) |> 
  as_tibble() |>
  ggplot(aes(
    x = agegr,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  scale_y_continuous(limits = c(5, 20),
                     breaks = c(5, 10, 15, 20)) +
  labs(x = "Age Groups", 
       y = "Absolute Difference on Police Thermometer",
       subtitle = "Absolute Change") +
  geom_linerange(alpha = .75, color = my_oka[2], linewidth = 2.5) +
  geom_point(size = 2, color = "black")

# --- plot 2
p2 <- m1 |>
  marginaleffects::avg_predictions(variables = "agegr",
                                   wts = d_2022$wt) |> 
  as_tibble() |>
  ggplot(aes(
    x = agegr,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  scale_y_continuous(limits = c(-10, 10),
                     breaks = c(-10, -5, 0, 5, 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .4) +
  labs(x = "Age Groups", 
       y = "First-Difference on Police Thermometer",
       subtitle = "Relative Change") +
  geom_linerange(alpha = .75, color = my_oka[2], linewidth = 2.5) +
  geom_point(size = 2, color = "black")

# --- export
p1 + p2
ggsave(
  filename = "./output/C_anes_panel-studies/p01-D.png",
  width = 12,
  height = 5,
  units = "in",
  dpi = 500,
  bg = "white"
)
rm(d_2022, m1, m2, p1, p2) # clean-up

# ---------------------------------------------------------------------------- #
