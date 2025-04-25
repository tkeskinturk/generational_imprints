
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

# --- datafile
d <- readRDS("./data/output/anes_16-20.rds")

# ---------------------------------------------------------------------------- #
# PART 1: FIRST-DIFFERENCE --------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- modeler

## first-difference
d <- d |>
  mutate(diff = police_2020 - police_2016)
## survey design
d_survey <- d |>
  as_survey(strata = str,
            weights = wt,
            psu = psu)
## survey models
m <- svyglm(diff ~
              agegr + mode + party, design = d_survey)

# --- drawers

## plot 1
p1 <- d |>
  ggplot(aes(
    x = diff,
    y = after_stat(ndensity),
    weight = wt
  )) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  labs(x = "First-Difference on Police Thermometer", 
       y = "Scaled Density") +
  geom_histogram(
    binwidth = 10,
    center = 0,
    alpha = .9,
    fill = my_oka[1]
  )
## plot 2
p2 <- m |>
  avg_predictions(variables = "agegr", wts = d$wt) |>
  as_tibble() |>
  ggplot(aes(
    x = agegr,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  scale_y_continuous(limits = c(-22, 4),
                     breaks = c(-20, -15, -10, -5, 0, 5)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  labs(x = "Age Groups", 
       y = "First-Difference on Police Thermometer") +
  geom_linerange(alpha = .75,
                 color = my_oka[2],
                 linewidth = 2.5) +
  geom_point(size = 2, color = "black")

# --- export
p1 + p2
ggsave(
  filename = "./output/C_anes_panel-studies/p01.png",
  width = 12,
  height = 4,
  units = "in",
  dpi = 500,
  bg = "white"
)
rm(m, p1, p2) # clean-up

# ---------------------------------------------------------------------------- #
