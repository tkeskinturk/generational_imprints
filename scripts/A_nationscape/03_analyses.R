
# - generational imprints ---------------------------------------------------- #
# - nationscape | analyze ---------------------------------------------------- #

### note: this script
###       (a) analyzes the effects of GF killing on attitudes towards police,
###       (b) examines whether local protest intensity is the main mechanism.
###       (c) examines whether PID political attention is the main mechanism.

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
d <- readRDS("./data/output/nationscape.rds")

# --- survey
d_survey <- srvyr::as_survey(d, weights = wt)

# ---------------------------------------------------------------------------- #
# PART 1: CHANGE IN FAVORABILITY, BY GROUP ----------------------------------- #
# ---------------------------------------------------------------------------- #

# --- calculate

## summarize
sum <- d_survey |>
  group_by(pid, agegr, time) |>
  summarize(y = srvyr::survey_mean(police)) |>
  ungroup()

## baselines
pre <- d_survey |>
  filter(time < 0) |>
  group_by(pid, agegr) |>
  summarize(b = srvyr::survey_mean(police)) |>
  select(pid, agegr, b) |> 
  ungroup()

# --- plot estimates

sum |>
  left_join(pre, by = c("agegr", "pid")) |>
  mutate(timing = ifelse(time < 0, 
                         "Before", 
                         "After")) |>
  ggplot(aes(
    x = time,
    y = y,
    ymin = y - 1.96 * y_se,
    ymax = y + 1.96 * y_se,
    color = timing
  )) +
  facet_grid(pid ~ agegr, scales = "fixed") +
  scale_x_continuous(limits = c(-12, 8),
                     breaks = c(-12, -8, -4, 0, 4, 8)) +
  scale_y_continuous(limits = c(.04, .77),
                     breaks = seq(from = .1, to = .7, by = .2)) +
  labs(x = "Time Before and After the Event", 
       y = "Unfavorable Attitudes Toward the Police") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  geom_hline(aes(yintercept = b), 
             linetype = "dashed", 
             linewidth = .4) +
  geom_pointrange(size = .4, alpha = .75) +
  scale_color_manual(values = c(my_oka[1], my_oka[2])) +
  theme(legend.position = "none")
ggsave(
  filename = "./output/A_nationscape/p01.png",
  width = 12,
  height = 6,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(sum, pre) # clean-up

# ---------------------------------------------------------------------------- #
# PART 2: REGRESSIONS -------------------------------------------------------- #
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

## baseline model
m1 <- feols(
  police_100 ~ treatment | agegr + time,
  data = d,
  weights = d$wt,
  vcov = "HC1"
)

## adjusted model
m2 <- feols(
  police_100 ~ treatment +
    gender * pid * educ |
    agegr + time + condistrict,
  data = d,
  weights = d$wt,
  vcov = "HC1"
)

## clustered errors
m3 <- feols(
  police_100 ~ treatment +
    gender * pid * educ |
    agegr + time + condistrict,
  data = d,
  weights = d$wt,
  vcov = "twoway"
)

# --- regression models with matching

plan(multisession)

## match
d_matched <- d |>
  nest(.by = "time") |>
  ## performs
  mutate(matching = future_map(
    .x = data,
    .f = ~ matchit(
      treated ~
        educ + gender + pid + attention,
      data = .,
      method = "nearest",
      ratio = 4,
      s.weights = ~ wt,
      replace = FALSE
    ),
    .progress = TRUE,
    .options = furrr_options(seed = 11235)
  )) |>
  ## matched data
  mutate(matched = map(
    .x = matching,
    .f = ~ match_data(.))) |> 
  ## evaluations
  mutate(
    var_all = map(
      .x = matching, 
      .f = ~ summary(.)$sum.all[1, ]),
    var_sub = map(
      .x = matching, 
      .f = ~ summary(.)$sum.matched[1, ])
  )

## matched model
m4 <- feols(
  police_100 ~ treatment +
    gender * pid * educ |
    agegr + time + condistrict,
  data = d_matched |> 
    select(time, matched) |> 
    unnest(matched),
  weights = ~ weights,
  vcov = "HC1"
)

## interaction model
m5 <- feols(
  police_100 ~ treatment * pid +
    gender * pid * educ |
    agegr + time + condistrict,
  data = d,
  weights = d$wt,
  vcov = "HC1"
)

# ---------------------------------------------------------------------------- #
# PART 3: REGRESSIONS: OUTPUT ------------------------------------------------ #
# ---------------------------------------------------------------------------- #

# --- information
m_info <- tibble(
  term = c("Adjustment", "Matched Sample", "Standard Error"),
  `(1)` = c("-", "-", "Robust"),
  `(2)` = c("+", "-", "Robust"),
  `(3)` = c("+", "-", "Clustered"),
  `(4)` = c("+", "+", "Robust"),
  `(5)` = c("+", "-", "Robust")
)
attr(m_info, "position") <- "coef_end"

## notes
notes <- "\\footnotesize \\textit{Notes}: Two-way fixed effects estimates using the Nationscape data file. Model 1 presents unadjusted estimates. Model 2 adjusts for gender, party identification, education, and their interactions, as well as congressional district fixed effects. Model 3 presents (2) with standard errors clustered at the age group and time level. Model 4 uses a matched sample, where respondents are matched on all covariates within each time window using a variable-ratio nearest neighbor matching. Model 5 presents the same estimates where the treatment indicator is interacted with party identification. The outcome variable is multipled by 100---to range from 0 to 100---to increase legibility."

## tiny!
m_summary <- modelsummary(
  models = list(
    '(1)' = m1,
    '(2)' = m2,
    '(3)' = m3,
    '(4)' = m4,
    '(5)' = m5
  ),
  coef_map = c(
    "treatment" = "Treatment",
    "treatment:pidIndependent" = "Treatment x Independent",
    "treatment:pidRepublican" = "Treatment x Republican"
  ),
  fmt = 2,
  add_rows = m_info,
  gof_map =
    list(
      list(
        "raw" = "nobs",
        "clean" = "N",
        "fmt" = 0
      ),
      list(
        "raw" = "r.squared",
        "clean" = "R2",
        "fmt" = 2
      )
    ),
  escape = FALSE,
  output = "tinytable",
  notes = list(notes),
  width = c(.4, rep(.12, 5))
)
saveRDS(m_summary, "output/A_nationscape/regs.rds")

# ---------------------------------------------------------------------------- #
# PART 4: DYNAMIC DIDs ------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- estimates
m <- feols(
  police ~
    i(time, treated, ref = -1) +
    gender * pid * educ |
    agegr + time + condistrict,
  data = d,
  weights = d$wt,
  vcov = "HC1"
)
mD <- feols(
  police ~
    i(bin(time, bin = "bin::2"), treated, ref = -2) +
    gender * educ |
    agegr + pid + agegr^pid + time + condistrict,
  data = d |> 
    mutate(treated = treated * (pid == "Democrat")),
  weights = d$wt,
  vcov = "HC1"
)
mI <- feols(
  police ~
    i(bin(time, bin = "bin::2"), treated, ref = -2) +
    gender * educ |
    agegr + pid + agegr^pid + time + condistrict,
  data = d |> 
    mutate(treated = treated * (pid == "Independent")),
  weights = d$wt,
  vcov = "HC1"
)
mR <- feols(
  police ~
    i(bin(time, bin = "bin::2"), treated, ref = -2) +
    gender * educ |
    agegr + pid + agegr^pid + time + condistrict,
  data = d |> 
    mutate(treated = treated * (pid == "Republican")),
  weights = d$wt,
  vcov = "HC1"
)

# --- plots

## plot 1
p1 <- ggiplot(
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
  scale_y_continuous(limits = c(-0.07, 0.15))

## plot 2
p2 <- ggiplot(
  ## event-study model
  list(
    'Democrats' = mD,
    'Independents' = mI,
    'Republicans' = mR
  ),
  ref.line = -1,
  geom_style = 'pointrange',
  pt.join = FALSE,
  multi_style = 'facet',
  ## plotting elements
  col = us_oka,
  xlab = "Time to Treatment",
  ylab = "Estimate",
  main = element_blank(),
  theme = theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  ) |> 
    theme(legend.position = "none")
) +
  scale_x_continuous(limits = c(-12, 7),
                     breaks = seq(-12, 7, by = 3)) +
  scale_y_continuous(limits = c(-0.12, 0.25))

p1 / p2 + 
    plot_layout(heights = c(0.6, 0.4))
ggsave(
    filename = "./output/A_nationscape/p02.png",
    width = 8,
    height = 7,
    units = "in",
    dpi = 500,
    bg = "white"
)

# ---------------------------------------------------------------------------- #
# PART 4: TRIPLE DIDs: PROTEST ----------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- define "treated"
d_protests <- d |>
  ## the distribution of quantiles
  mutate(
    d = case_when(
      protests < 20 ~ 
        "Protest Intensity: \n 0%-25%",
      protests >= 20 & protests < 37 ~ 
        "Protest Intensity: \n 25%-50%",
      protests >= 37 & protests < 61 ~ 
        "Protest Intensity: \n 50%-75%",
      protests >= 61 ~ 
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
  mutate(post = ifelse(time < 0, 0, 1)) |>
  drop_na() ## lose roughly 2.8% of the data due to NAs in `condistrict`

# --- modeling
m <- feols(
  police ~ treatment * pid * d +
    gender * educ |
    agegr + time,
  data = d_protests,
  weights = d_protests$wt,
  vcov = "HC1"
)

# --- "effects"
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

# --- draw 
p1 <- p |>
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

# ---------------------------------------------------------------------------- #
# PART 5: TRIPLE DIDs: ATTENTION --------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- define "treated"
d_attention <- d |>
  mutate(
    d = case_when(
      attention < 0.25 ~ 
        "Political Attention: \n 0%-25%",
      attention >= 0.25 & attention < 0.50 ~ 
        "Political Attention: \n 25%-50%",
      attention >= 0.50 & attention < 0.75 ~ 
        "Political Attention: \n 50%-75%",
      attention >= 0.75 ~ 
        "Political Attention: \n 75%-100%"
    )
  ) |>
  mutate(d = factor(
    d,
    levels = c(
      "Political Attention: \n 0%-25%",
      "Political Attention: \n 25%-50%",
      "Political Attention: \n 50%-75%",
      "Political Attention: \n 75%-100%"
    )
  )) |>
  mutate(post = ifelse(time < 0, 0, 1))

# --- modeling
m <- feols(
  police ~ treatment * pid * d +
    gender * educ |
    agegr + time + condistrict,
  data = d_attention,
  weights = d_attention$wt,
  vcov = "HC1"
)

# --- "effects"
p <- avg_slopes(
  m,
  ## supply the fixest model
  variables = "treatment",
  by = c("pid", "d"),
  newdata = datagrid(treatment = 0:1, 
                     pid = levels(d_attention$pid),
                     d = levels(d_attention$d))
) |>
  as_tibble() ## replicates Stata's dydx

# --- draw 
p2 <- p |>
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
       y = "") +
  theme(legend.position = "none") +
  scale_color_manual(values = us_oka)

# ---------------------------------------------------------------------------- #
# PART 6: TRIPLE DIDs: EXPORT ------------------------------------------------ #
# ---------------------------------------------------------------------------- #

p1 / p2
ggsave(
  filename = "./output/A_nationscape/p03.png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 500,
  bg = "white"
)

# ---------------------------------------------------------------------------- #
