
# - generational imprints ---------------------------------------------------- #
# - CCES time-series | analyze ----------------------------------------------- #

### note: this script
###       (a) analyzes the effects of GF killing on attitudes towards police.

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
d <- readRDS("./data/output/cces_16-22.rds")

# ---------------------------------------------------------------------------- #
# PART 1: REGRESSIONS -------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- treated
d <- d |>
  mutate(treated = ifelse(
    age_at_2020 == "24 or Younger", 1, 0)) |> 
  ## integer `year` for `feols`
  mutate(year = as.integer(as.character(year)))

# --- modeler

## baseline
m <- feols(
  police_safe ~ 
    i(year, treated, ref = 2016) +
    pid * gender * educ | 
    age_at_2020 + year + county,
  data = d,
  weights = d$wt,
  vcov = "HC1"
)
## baseline: Democrats
mD <- feols(
  police_safe ~
    i(year, treated, ref = 2016) +
    gender * educ |
    age_at_2020 + pid + age_at_2020^pid + year + county,
  data = d |>
    mutate(treated = treated * (pid == "Democrat")),
  weights = d$wt,
  vcov = "HC1"
)
## baseline: Independents
mI <- feols(
  police_safe ~
    i(year, treated, ref = 2016) +
    gender * educ |
    age_at_2020 + pid + age_at_2020^pid + year + county,
  data = d |>
    mutate(treated = treated * (pid == "Independent")),
  weights = d$wt,
  vcov = "HC1"
)
## baseline: Republicans
mR <- feols(
  police_safe ~
    i(year, treated, ref = 2016) +
    gender * educ |
    age_at_2020 + pid + age_at_2020^pid + year + county,
  data = d |>
    mutate(treated = treated * (pid == "Republican")),
  weights = d$wt,
  vcov = "HC1"
)

# ---------------------------------------------------------------------------- #
# PART 2: REGRESSIONS, DRAW -------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- plot 1
p1 <- ggiplot(
  ## event-study model
  m,
  geom_style = "ribbon",
  ## plotting elements
  xlab = "Time",
  ylab = "Estimate",
  col = my_oka[1],
  main = element_blank(),
  theme = theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  )
) +
  scale_x_continuous(breaks = c(2016, 2020, 2022)) +
  scale_y_continuous(limits = c(-.02, 0.15))

# --- plot 2
p2 <- ggiplot(
  ## event-study model
  list(
    "Democrats" = mD,
    "Independents" = mI,
    "Republicans" = mR
  ),
  geom_style = 'pointrange',
  pt.join = FALSE,
  multi_style = "facet",
  ## plotting elements
  xlab = "Time",
  ylab = "Estimate",
  col = us_oka,
  main = element_blank(),
  theme = theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  ) +
    theme(legend.position = "none")
) +
  scale_x_continuous(breaks = c(2016, 2020, 2022)) +
  scale_y_continuous(limits = c(-0.10, 0.35),
                     breaks = c(0, 0.1, 0.2, 0.3))

# --- export
p1 / p2 + 
  plot_layout(heights = c(0.6, 0.4))
ggsave(
  filename = "./output/B_cces_16-22/p01.png",
  width = 8,
  height = 7,
  units = "in",
  dpi = 500,
  bg = "white"
)

# ---------------------------------------------------------------------------- #
