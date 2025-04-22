
# - generational imprints ---------------------------------------------------- #
# - CCES time-series | analyze ----------------------------------------------- #

### note: this script
###       (a) analyzes partisan differentiation in police evaluations in 2016.

rm(list = ls()) # clean-up

pacman::p_load(
  fixest,
  ggfixest,
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
    "#cc0000")  
theme_set(theme_ipsum_rc(
  grid = "XY",
  axis_title_size = 12,
  plot_margin =
    margin(10, 10, 10, 10)
))
`%nin%` = Negate(`%in%`)

# --- datafile
d <- readRDS("./data/output/cces_16-22.rds") |> 
  filter(year == 2016)

# --- binarize
d <- d |>
  mutate(police_safe = if_else(police_safe == 0, 1, 0))

# --- estimate
m <- feols(
  police_safe ~ pid +
    age + gender + educ | county,
  data = d,
  weights = d$wt,
  vcov = "HC1"
)

# --- predict
avg <- 
  avg_predictions(m, variables = "pid") |>
  as_tibble()

# --- plot
avg |>
  ggplot(aes(
    x = pid,
    y = estimate,
    col = pid,
    ymin = conf.low,
    ymax = conf.high
  )) +
  geom_linerange(linewidth = 2) +
  scale_color_manual(values = us_oka) +
  geom_point(col = "black") +
  theme(legend.position = "none") +
  labs(x = "Party Identification",
       y = "% Feeling Safe with the Police in 2016") +
  scale_y_continuous(limits = c(0.4, 0.8),
                     labels = scales::percent_format())
ggsave(
  filename = "./output/B_cces_16-22/p01-A.png",
  width = 8,
  height = 4,
  units = "in",
  dpi = 500,
  bg = "white"
)

# ---------------------------------------------------------------------------- #
