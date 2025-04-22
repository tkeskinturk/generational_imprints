
# - generational imprints ---------------------------------------------------- #
# - nationscape | analyze ---------------------------------------------------- #

### note: this script
###       (a) provides alternative analyses for the local protest intensity,
###       (b) provides alternative analyses for the PID political attention.

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
d <- readRDS("./data/output/nationscape.rds") |>
  ## define treated as 18-24-year-olds
  mutate(treated = if_else(age <= 24, 1, 0)) |>
  ## define post as the period starting with t = 0
  mutate(post = if_else(time >= 0, 1, 0)) |>
  ## define treatment as 18-24-year-old with t = 0
  mutate(treatment = treated * post) ## this covers it

# ---------------------------------------------------------------------------- #
# PART 1: TRIPLE DIDs: PROTEST ----------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- measure with logs

## define cuts
cut_1 <- quantile(log(d$protests), 1/4, na.rm = TRUE)
cut_2 <- quantile(log(d$protests), 2/4, na.rm = TRUE)
cut_3 <- quantile(log(d$protests), 3/4, na.rm = TRUE)

## define "treated"
d_protests <- d |>
  mutate(
    d = case_when(
      log(protests) < cut_1 ~ 
        "Protest Intensity: \n 0%-25%",
      log(protests) >= cut_1 & log(protests) < cut_2 ~ 
        "Protest Intensity: \n 25%-50%",
      log(protests) >= cut_2 & log(protests) < cut_3 ~ 
        "Protest Intensity: \n 50%-75%",
      log(protests) >= cut_3 ~ 
        "Protest Intensity: \n 75%-100%"
    )
  ) |>
  mutate(d = factor(
    d,
    levels = c(
      "Protest Intensity: \n 0%-25%",
      "Protest Intensity: \n 25%-50%",
      "Protest Intensity: \n 50%-75%",
      "Protest Intensity: \n 75%-100%"
    )
  )) |>  
  drop_na() ## lose roughly 2.8% of the data due to NAs in `condistrict`

## modeling
m <- feols(
  police ~ treatment * pid * d +
    gender * educ |
    agegr + time,
  data = d_protests,
  weights = d_protests$wt,
  vcov = "HC1"
)

## "effects"
p <- avg_slopes(
  m,
  ## supply the fixest model
  variables = "treatment",
  by = c("pid", "d"),
  newdata = datagrid(treatment = 0:1, 
                     pid = levels(d_protests$pid),
                     d = levels(d_protests$d))
) |>
  as_tibble() ## replicates Stata's dydx

## draw 
p |>
  ggplot(aes(
    x = d,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high,
    col = pid
  )) +
  scale_y_continuous(limits = c(-0.15, 0.35)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(col = "Party Group",
       x = "",
       y = "Average Post-Treatment Effect") +
  theme(legend.position = "top") +
  scale_color_manual(values = us_oka)
ggsave(
  filename = "./output/A_nationscape/p03-A.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(cut_1, cut_2, cut_3, d_protests, m, p) # clean-up

# --- measure with bins

## define "treated"
d_protests <- d |>
  mutate(d = factor(ntile(protests, n = 10))) |> 
  drop_na() ## lose roughly 2.8% of the data due to NAs in `condistrict`

## modeling
m <- feols(
  police ~ treatment * pid * d +
    gender * educ |
    agegr + time,
  data = d_protests,
  weights = d_protests$wt,
  vcov = "HC1"
)

## "effects"
p <- avg_slopes(
  m,
  ## supply the fixest model
  variables = "treatment",
  by = c("pid", "d"),
  newdata = datagrid(treatment = 0:1, 
                     pid = levels(d_protests$pid),
                     d = levels(d_protests$d))
) |>
  as_tibble() ## replicates Stata's dydx

## draw 
p |>
  ggplot(aes(
    x = d,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high,
    col = pid
  )) +
  scale_y_continuous(limits = c(-0.17, 0.35)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(col = "Party Group",
       x = "",
       y = "Average Post-Treatment Effect") +
  theme(legend.position = "top") +
  scale_color_manual(values = us_oka)
ggsave(
  filename = "./output/A_nationscape/p03-B.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(d_protests, m, p) # clean-up

# ---------------------------------------------------------------------------- #
# PART 2: TRIPLE DIDs: ATTENTION --------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- change in attention

## modeling
m <- feols(
  attention ~ 
    i(time, treated, ref = -1) +
    pid * gender * educ |
    agegr + time + condistrict,
  data = d,
  weights = d$wt,
  vcov = "HC1"
)

## draw PS
ggiplot(
  ## event-study model
  m,
  ref.line = -1,
  geom_style = 'ribbon',
  pt.join = TRUE,
  ## plotting elements
  col = my_oka[1],
  xlab = "Time to Treatment",
  ylab = "Estimate",
  main = element_blank(),
  theme = theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  )
) +
  scale_x_continuous(limits = c(-12, 7),
                     breaks = seq(-12, 7, by = 1)) +
  scale_y_continuous(limits = c(-0.15, 0.15))
ggsave(
  filename = "./output/A_nationscape/p03-C.png",
  width = 8,
  height = 4,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(m) # clean-up

# ---------------------------------------------------------------------------- #
