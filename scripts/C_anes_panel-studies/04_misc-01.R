
# - generational imprints ---------------------------------------------------- #
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
d <- readRDS("./data/output/anes_16-20.rds") |> 
  mutate(diff = police_2020 - police_2016)
# --- survey design
d_survey <- d |>
  as_survey(strata = str,
            weights = wt,
            psu = psu)

# ---------------------------------------------------------------------------- #
# PART 1: AGE WITH SPLINES --------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- cross-validation
set.seed(11234)
cv.svy(
  d,
  c(
    "diff ~ ns(age, df = 1) + factor(mode) + party",
    "diff ~ ns(age, df = 2) + factor(mode) + party",
    "diff ~ ns(age, df = 3) + factor(mode) + party",
    "diff ~ ns(age, df = 4) + factor(mode) + party",
    "diff ~ ns(age, df = 5) + factor(mode) + party",
    "diff ~ ns(age, df = 6) + factor(mode) + party",
    "diff ~ ns(age, df = 7) + factor(mode) + party",
    "diff ~ ns(age, df = 8) + factor(mode) + party",
    "diff ~ ns(age, df = 9) + factor(mode) + party"
  ),
  nfolds = 4,
  strataID = "str",
  weightsID = "wt"
) ## model with 3 degrees of freedom is the best-fitting one.

# --- modeling
m_spline <- svyglm(
  diff ~ ns(age, df = 3) + factor(mode) + party,
  design = d_survey
)

# --- export
plot_predictions(m_spline,
                 condition = "age",
                 type = "response",
                 draw = FALSE) |>
  as_tibble() |>
  ggplot(aes(
    x = age,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  labs(x = "Age (Transformed)", y = "Estimates") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  geom_ribbon(fill = my_oka[1], alpha = 0.5) +
  geom_line(aes(group = 1)) +
  scale_x_continuous(breaks = c(-0.12, 0.58), 
                     labels = c(20, 80))
ggsave(
  filename = "./output/C_anes_panel-studies/p01-A.png",
  width = 8,
  height = 4,
  units = "in",
  dpi = 500,
  bg = "white"
)
rm(m_spline) # clean-up

# ---------------------------------------------------------------------------- #
# PART 2 OUTLIERS ------------------------------------------------------------ #
# ---------------------------------------------------------------------------- #

# --- preps
d_win <- d |>
  filter(diff >= quantile(diff, 0.05), ## -39
         diff <= quantile(diff, 0.95)) ## +28
d <- d |>
  mutate(treated = ifelse(age <= 29, 1, 0))

# --- modeling
m_win <- survey::svyglm(
  diff ~ agegr + mode + party,
  design = d_win |>
    as_survey(
      strata = str,
      weights = wt,
      psu = psu
    )
)
m_rob <- MASS::rlm(diff ~
                     agegr + mode + party,
                   data = d,
                   weights = d$wt)

# --- plots

## plot 1
p1 <- m_win |>
  marginaleffects::avg_predictions(variables = "agegr",
                                   wts = d_win$wt) |> 
  as_tibble() |>
  ggplot(aes(
    x = agegr,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  scale_y_continuous(limits = c(-16, 5),
                     breaks = c(-15, -10, -5, 0, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .4) +
  labs(x = "", 
       y = "First-Difference on Police Thermometer",
       subtitle = "Outliers Pruned") +
  geom_linerange(alpha = .75, color = my_oka[2], linewidth = 2.5) +
  geom_point(size = 2, color = "black")

## plot 2
p2 <- m_rob |>
  marginaleffects::avg_predictions(variables = "agegr",
                                   wts = d$wt,
                                   vcov = "HC1") |> 
  as_tibble() |>
  ggplot(aes(
    x = agegr,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  scale_y_continuous(limits = c(-16, 5),
                     breaks = c(-15, -10, -5, 0, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .4) +
  labs(x = "Age Groups", 
       y = "",
       subtitle = "Robust Regression (IWLS)") +
  geom_linerange(alpha = .75, color = my_oka[2], linewidth = 2.5) +
  geom_point(size = 2, color = "black")

# --- export
p1 + p2
ggsave(
  filename = "./output/C_anes_panel-studies/p01-B.png",
  width = 12,
  height = 5,
  units = "in",
  dpi = 500,
  bg = "white"
)
rm(d_win, m_rob, m_win, p1, p2) # clean-up

# ---------------------------------------------------------------------------- #
# PART 3 --------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- preps
d_web <- d |> filter(mode == "WEB")
d_survey_web <- d_web |>
  mutate(diff = police_2020 - police_2016) |>
  as_survey(strata = str,
            weights = wt,
            psu = psu)

# --- modeling
m <- survey::svyglm(diff ~ agegr + party, 
                    design = d_survey_web)

# --- plots

## plot 1
p1_web <- d_web |>
  mutate(diff = police_2020 - police_2016) |>
  ggplot(aes(
    x = diff,
    y = after_stat(ndensity),
    weight = d |> filter(mode == "WEB") |> pull(wt)
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
p2_web <- m |>
  marginaleffects::avg_predictions(variables = "agegr",
                                   wts = d_web$wt) |> 
  as_tibble() |>
  ggplot(aes(
    x = agegr,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  scale_y_continuous(limits = c(-24, 6),
                     breaks = c(-20, -15, -10, -5, 0, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .4) +
  labs(x = "Age Groups", 
       y = "First-Difference on Police Thermometer") +
  geom_linerange(alpha = .75, color = my_oka[2], linewidth = 2.5) +
  geom_point(size = 2, color = "black")

# --- export
p1_web + p2_web
ggsave(
  filename = "./output/C_anes_panel-studies/p01-C.png",
  width = 12,
  height = 4,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(d_survey_web, d_web, p1_web, p2_web, m) # clean-up

# ---------------------------------------------------------------------------- #
