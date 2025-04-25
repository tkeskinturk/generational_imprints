
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
# PART 1: PREPS -------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- preps

## first-difference
d <- d |>
  mutate(diff = police_2020 - police_2016)

## survey design
d_survey <- d |>
  as_survey(strata = str,
            weights = wt,
            psu = psu)

# ---------------------------------------------------------------------------- #
# PART 2: PARTISAN DIFFERENCES ----------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- modeling
m <- svyglm(diff ~
              agegr * pid2020 + pid2016 + mode, design = d_survey)

# --- draw
avg_predictions(m, variables = c("agegr", "pid2020"), 
                vcov = TRUE, wts = d$wt) |>
  as_tibble() |>
  filter(pid2020 != "Independent") |> 
  ggplot(aes(
    x = agegr,
    y = estimate,
    col = pid2020,
    ymin = conf.low,
    ymax = conf.high
  )) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  facet_wrap(~ pid2020) +
  geom_pointrange(alpha = .75, linewidth = 2.5) +
  scale_y_continuous(limits = c(-35, 10),
                     breaks = c(-30, -20, -10, 0, 10)) +
  scale_color_manual(values = c(us_oka[1],
                                us_oka[3])) +
  theme(legend.position = "none") +
  labs(x = "Age Groups", 
       y = "First-Difference on Police Thermometer", 
       col = "")
ggsave(
  filename = "./output/C_anes_panel-studies/p01-E.png",
  width = 8,
  height = 4,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(m) # clean-up

# ---------------------------------------------------------------------------- #
